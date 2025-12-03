#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use YAML::XS;
use AI::XGBoost;
use List::Util qw(shuffle);

# Extract features from a pair of generals
sub extract_features ($g1_name, $g2_name) {
  my $g1_file = "share/collections/data/generals/$g1_name.yaml";
  my $g2_file = "share/collections/data/generals/$g2_name.yaml";
  
  return unless -f $g1_file && -f $g2_file;
  
  my $g1 = YAML::XS::LoadFile($g1_file);
  my $g2 = YAML::XS::LoadFile($g2_file);
  
  my $b1_file = "share/collections/data/skill books/$g1->{book}.yaml";
  my $b2_file = "share/collections/data/skill books/$g2->{book}.yaml";
  
  return unless -f $b1_file && -f $b2_file;
  
  my $b1 = YAML::XS::LoadFile($b1_file);
  my $b2 = YAML::XS::LoadFile($b2_file);
  
  # Count overlaps by condition type
  my $same_cond_overlaps = 0;
  my $diff_cond_overlaps = 0;
  my $min_delta = 999;
  my $max_delta = 0;
  my $sum_delta = 0;
  my $overlap_count = 0;
  
  # Track buff counts and grouping
  my $g1_buff_count = 0;
  my $g2_buff_count = 0;
  my $g1_has_multi_group = 0;
  my $g2_has_multi_group = 0;
  
  # Count buffs
  for my $buff (@{$b1->{buffs} // []}) {
    $g1_buff_count++ unless $buff->{passive};
  }
  for my $buff (@{$b2->{buffs} // []}) {
    $g2_buff_count++ unless $buff->{passive};
  }
  
  # Check for multi-attribute groups
  for my $book ($b1, $b2) {
    my %by_key;
    for my $buff (@{$book->{buffs} // []}) {
      next if $buff->{passive};
      my $conds = join('|', sort grep { $_ ne 'leading the army' && $_ ne 'you own the General' } @{$buff->{conditions} // []});
      my $key = join('|', $buff->{value}{number} // 0, $buff->{value}{unit} // '', $buff->{targetedType} // '', $conds);
      push @{$by_key{$key}}, $buff->{attribute};
    }
    
    for my $key (keys %by_key) {
      my @attrs = @{$by_key{$key}};
      if (@attrs >= 2) {
        $g1_has_multi_group = 1 if $book == $b1;
        $g2_has_multi_group = 1 if $book == $b2;
      }
    }
  }
  
  # Find overlaps
  for my $buff1 (@{$b1->{buffs} // []}) {
    next if $buff1->{passive};
    for my $buff2 (@{$b2->{buffs} // []}) {
      next if $buff2->{passive};
      
      next unless $buff1->{attribute} eq $buff2->{attribute};
      my $t1 = $buff1->{targetedType} // '';
      my $t2 = $buff2->{targetedType} // '';
      next unless $t1 eq $t2 || !$t1 || !$t2;
      
      $overlap_count++;
      
      my $delta = abs(($buff1->{value}{number} // 0) - ($buff2->{value}{number} // 0));
      $min_delta = $delta if $delta < $min_delta;
      $max_delta = $delta if $delta > $max_delta;
      $sum_delta += $delta;
      
      my $c1 = join(',', grep { $_ ne 'leading the army' } @{$buff1->{conditions} // []});
      my $c2 = join(',', grep { $_ ne 'leading the army' } @{$buff2->{conditions} // []});
      
      if ($c1 eq $c2) {
        $same_cond_overlaps++;
      } else {
        $diff_cond_overlaps++;
      }
    }
  }
  
  return unless $overlap_count > 0;
  
  my $avg_delta = $sum_delta / $overlap_count;
  
  return [
    $same_cond_overlaps,      # 0: overlaps with same conditions
    $diff_cond_overlaps,      # 1: overlaps with different conditions
    $min_delta,               # 2: minimum delta
    $max_delta,               # 3: maximum delta
    $avg_delta,               # 4: average delta
    $overlap_count,           # 5: total overlaps
    $g1_buff_count,           # 6: g1 buff count
    $g2_buff_count,           # 7: g2 buff count
    $g1_has_multi_group,      # 8: g1 has multi-attr group
    $g2_has_multi_group,      # 9: g2 has multi-attr group
  ];
}

# Load working pairs
say "Loading working pairs...";
open my $wf, '<', 't/mounted_pairs' or die "Can't open t/mounted_pairs: $!";
my @working_data;
while (my $line = <$wf>) {
  chomp $line;
  my ($g1, $g2) = split /\t/, $line;
  my $features = extract_features($g1, $g2);
  push @working_data, { features => $features, label => 0 } if $features;  # 0 = compatible
}
close $wf;
say "Loaded " . scalar(@working_data) . " working pairs";

# Load conflicting pairs
say "Loading conflicting pairs...";
open my $cf, '<', 't/conflicting_pairs' or die "Can't open t/conflicting_pairs: $!";
my @conflict_data;
while (my $line = <$cf>) {
  chomp $line;
  next if $line =~ /^\s*$/;
  
  # Parse general names (try different splits)
  my @words = split /\s+/, $line;
  next unless @words >= 2;
  
  for my $i (1 .. $#words) {
    my $g1 = join(' ', @words[0..$i-1]);
    my $g2 = join(' ', @words[$i..$#words]);
    
    my $features = extract_features($g1, $g2);
    if ($features) {
      push @conflict_data, { features => $features, label => 1 };  # 1 = conflict
      last;
    }
  }
}
close $cf;
say "Loaded " . scalar(@conflict_data) . " conflicting pairs";

# Combine and shuffle
my @all_data = shuffle(@working_data, @conflict_data);
say "Total dataset: " . scalar(@all_data) . " pairs";

# Split into train/test (80/20)
my $split_idx = int(0.8 * scalar(@all_data));
my @train_data = @all_data[0..$split_idx-1];
my @test_data = @all_data[$split_idx..$#all_data];

say "Training set: " . scalar(@train_data) . " pairs";
say "Test set: " . scalar(@test_data) . " pairs";

# Prepare training data
my @train_features = map { $_->{features} } @train_data;
my @train_labels = map { $_->{label} } @train_data;

# Train XGBoost model
say "\nTraining XGBoost model...";
my $model = AI::XGBoost->new(
  objective => 'binary:logistic',
  max_depth => 6,
  eta => 0.1,
  num_round => 100,
  eval_metric => 'error',
);

$model->train(\@train_features, \@train_labels);

# Test the model
say "\nEvaluating on test set...";
my $correct = 0;
my $total = scalar(@test_data);

for my $test (@test_data) {
  my $pred = $model->predict($test->{features});
  my $pred_label = $pred > 0.5 ? 1 : 0;
  $correct++ if $pred_label == $test->{label};
}

my $accuracy = $correct / $total * 100;
say sprintf("Test accuracy: %.1f%% (%d/%d)", $accuracy, $correct, $total);

# Feature importance
say "\nFeature importance:";
my @feature_names = (
  'same_cond_overlaps',
  'diff_cond_overlaps', 
  'min_delta',
  'max_delta',
  'avg_delta',
  'overlap_count',
  'g1_buff_count',
  'g2_buff_count',
  'g1_has_multi_group',
  'g2_has_multi_group',
);

my $importance = $model->feature_importance();
for my $i (0..$#feature_names) {
  say sprintf("  %s: %.3f", $feature_names[$i], $importance->[$i] // 0);
}

# Save model
say "\nSaving model to conflict_model.xgb...";
$model->save('conflict_model.xgb');
say "Done!";

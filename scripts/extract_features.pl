#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use YAML::XS;

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
  my $g1_max_group_size = 0;
  my $g2_max_group_size = 0;
  
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
        if ($book == $b1) {
          $g1_has_multi_group = 1;
          $g1_max_group_size = scalar(@attrs) if scalar(@attrs) > $g1_max_group_size;
        } else {
          $g2_has_multi_group = 1;
          $g2_max_group_size = scalar(@attrs) if scalar(@attrs) > $g2_max_group_size;
        }
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
  $min_delta = 0 if $min_delta == 999;
  
  return {
    same_cond_overlaps => $same_cond_overlaps,
    diff_cond_overlaps => $diff_cond_overlaps,
    min_delta => $min_delta,
    max_delta => $max_delta,
    avg_delta => $avg_delta,
    overlap_count => $overlap_count,
    g1_buff_count => $g1_buff_count,
    g2_buff_count => $g2_buff_count,
    g1_has_multi_group => $g1_has_multi_group,
    g2_has_multi_group => $g2_has_multi_group,
    g1_max_group_size => $g1_max_group_size,
    g2_max_group_size => $g2_max_group_size,
    both_have_multi_group => ($g1_has_multi_group && $g2_has_multi_group) ? 1 : 0,
  };
}

# Print CSV header
say join(',', qw(
  same_cond_overlaps
  diff_cond_overlaps
  min_delta
  max_delta
  avg_delta
  overlap_count
  g1_buff_count
  g2_buff_count
  g1_has_multi_group
  g2_has_multi_group
  g1_max_group_size
  g2_max_group_size
  both_have_multi_group
  label
));

# Load working pairs (label = 0)
open my $wf, '<', 't/mounted_pairs' or die "Can't open t/mounted_pairs: $!";
while (my $line = <$wf>) {
  chomp $line;
  my ($g1, $g2) = split /\t/, $line;
  my $features = extract_features($g1, $g2);
  next unless $features;
  
  say join(',',
    $features->{same_cond_overlaps},
    $features->{diff_cond_overlaps},
    $features->{min_delta},
    $features->{max_delta},
    $features->{avg_delta},
    $features->{overlap_count},
    $features->{g1_buff_count},
    $features->{g2_buff_count},
    $features->{g1_has_multi_group},
    $features->{g2_has_multi_group},
    $features->{g1_max_group_size},
    $features->{g2_max_group_size},
    $features->{both_have_multi_group},
    0  # label: 0 = compatible
  );
}
close $wf;

# Load conflicting pairs (label = 1)
open my $cf, '<', 't/conflicting_pairs' or die "Can't open t/conflicting_pairs: $!";
while (my $line = <$cf>) {
  chomp $line;
  next if $line =~ /^\s*$/;
  
  # Parse general names
  my @words = split /\s+/, $line;
  next unless @words >= 2;
  
  for my $i (1 .. $#words) {
    my $g1 = join(' ', @words[0..$i-1]);
    my $g2 = join(' ', @words[$i..$#words]);
    
    my $features = extract_features($g1, $g2);
    if ($features) {
      say join(',',
        $features->{same_cond_overlaps},
        $features->{diff_cond_overlaps},
        $features->{min_delta},
        $features->{max_delta},
        $features->{avg_delta},
        $features->{overlap_count},
        $features->{g1_buff_count},
        $features->{g2_buff_count},
        $features->{g1_has_multi_group},
        $features->{g2_has_multi_group},
        $features->{g1_max_group_size},
        $features->{g2_max_group_size},
        $features->{both_have_multi_group},
        1  # label: 1 = conflict
      );
      last;
    }
  }
}
close $cf;

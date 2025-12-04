#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use FindBin;
use lib "$FindBin::Bin/../lib";
use Getopt::Long qw( GetOptions );
require Game::EvonyTKR::Service::Cache;
use Game::EvonyTKR::Model::Base;
use List::AllUtils qw( any uniq max min sum );
use Scalar::Util qw( refaddr );

my $mode   = 'training';  # training or predict
my $output = undef;
my $help   = 0;

Getopt::Long::GetOptions(
  'mode=s'   => \$mode,
  'output=s' => \$output,
  'help'     => \$help,
) or die "Error in command line arguments\n";

if ($help || !$output) {
  print <<'USAGE';
Usage: extract_conflict_features.pl --mode=MODE --output=FILE

Modes:
  training  Extract features from known pairs in share/training_data/
  predict   Extract features for all possible general pairs

Options:
  --mode=MODE     Mode: training or predict (default: training)
  --output=FILE   Output CSV file path
  --help          Show this help

Examples:
  # Extract training data
  perl bin/extract_conflict_features.pl --mode=training --output=training_data.csv

  # Extract all pairs for prediction
  perl bin/extract_conflict_features.pl --mode=predict --output=all_pairs.csv
USAGE
  exit 0;
}

die "Invalid mode: $mode (must be 'training' or 'predict')\n"
  unless $mode =~ /^(training|predict)$/;

# Initialize data loader
my $common = Game::EvonyTKR::Model::Base->new();
my @generals =  $common->get_generals()->@*;
my %general_by_name = ();
foreach my $general (@generals){
  my $nn = lc($common->normalize($general->name));
  $general_by_name{$nn} = $general;
}

say STDERR "Loaded " . scalar(@generals) . " generals";

# Open output file
open my $out_fh, '>', $output or die "Cannot open $output: $!\n";

# Write CSV header
my @feature_names = (
  'general1', 'general2',
  # Troop type features
  'shared_troop_types', 'g1_troop_count', 'g2_troop_count',
  # Specialist type features
  'g1_mounted_specialist', 'g1_ranged_specialist', 'g1_ground_specialist', 'g1_siege_specialist',
  'g2_mounted_specialist', 'g2_ranged_specialist', 'g2_ground_specialist', 'g2_siege_specialist',
  'same_specialist_type',
  # Group structure features
  'g1_multi_attr_groups', 'g2_multi_attr_groups',
  'g1_solo_buffs', 'g2_solo_buffs',
  'g1_total_buffs', 'g2_total_buffs',
  # Value features (for each attribute: Attack, Defense, HP)
  'attack_overlap', 'attack_delta', 'attack_ratio',
  'defense_overlap', 'defense_delta', 'defense_ratio',
  'hp_overlap', 'hp_delta', 'hp_ratio',
  # Condition features
  'both_have_attacking', 'both_have_defending', 'both_have_monster',
  'g1_has_dragon', 'g2_has_dragon',
  'condition_overlap_count',
  # Special cases
  'is_sun_ce_pair',
  # Book text complexity
  'g1_book_text_length', 'g2_book_text_length',
);

if ($mode eq 'training') {
  push @feature_names, 'label';
}

say $out_fh join(',', @feature_names);

if ($mode eq 'training') {
  extract_training_data($out_fh, \%general_by_name);
} else {
  extract_all_pairs($out_fh, \@generals);
}

close $out_fh;
say STDERR "Wrote features to $output";

sub extract_training_data ($fh, $general_by_name) {
  my @pairs;

  # Read conflicting pairs (label = 1)
  if (-f 'share/training_data/conflicting_pairs') {
    open my $conflict_fh, '<', 'share/training_data/conflicting_pairs' or die "Cannot open share/training_data/conflicting_pairs: $!\n";
    while (my $line = <$conflict_fh>) {
      chomp $line;
      next if $line =~ /^\s*$/;
      my ($g1_name, $g2_name) = split /;/, $line, 2;
      next unless defined $g1_name && defined $g2_name;
      $g1_name =~ s/^\s+|\s+$//g;
      $g2_name =~ s/^\s+|\s+$//g;

      my $g1 = $general_by_name->{lc($common->normalize($g1_name))};
      my $g2 = $general_by_name->{lc($common->normalize($g2_name))};
      next unless $g1 && $g2;

      push @pairs, [$g1, $g2, 1];
    }
    close $conflict_fh;
    say STDERR "Loaded " . scalar(@pairs) . " conflicting pairs";
  }

  # Read working pairs (label = 0) from all *_pairs files (except conflicting_pairs)
  my $working_count = 0;
  my @pair_files = grep { $_ !~ /conflicting_pairs$/ } glob('share/training_data/*_pairs');
  for my $pair_file (@pair_files) {
    next unless -f $pair_file;

    open my $work_fh, '<', $pair_file or die "Cannot open $pair_file: $!\n";
    my $file_count = 0;
    while (my $line = <$work_fh>) {
      chomp $line;
      next if $line =~ /^\s*$/;
      my ($g1_name, $g2_name) = split /;/, $line, 2;
      next unless defined $g1_name && defined $g2_name;
      $g1_name =~ s/^\s+|\s+$//g;
      $g2_name =~ s/^\s+|\s+$//g;

      my $g1 = $general_by_name->{lc($common->normalize($g1_name))};
      my $g2 = $general_by_name->{lc($common->normalize($g2_name))};
      next unless $g1 && $g2;

      push @pairs, [$g1, $g2, 0];
      $file_count++;
    }
    close $work_fh;
    say STDERR "Loaded $file_count working pairs from $pair_file";
    $working_count += $file_count;
  }
  say STDERR "Loaded $working_count working pairs total";

  # Extract features for each pair
  for my $pair (@pairs) {
    my ($g1, $g2, $label) = @$pair;
    my @features = extract_features($g1, $g2);
    push @features, $label;
    say $fh join(',', @features);
  }
}

sub extract_all_pairs ($fh, $generals) {
  my $count = 0;
  for my $i (0 .. $#$generals - 1) {
    for my $j ($i + 1 .. $#$generals) {
      my $g1 = $generals->[$i];
      my $g2 = $generals->[$j];

      my @features = extract_features($g1, $g2);
      say $fh join(',', @features);
      $count++;
    }
  }
  say STDERR "Generated features for $count general pairs";
}

sub extract_features ($g1, $g2) {
  my @features;

  # General names
  push @features, lc($common->normalize($g1->name)), lc($common->normalize($g2->name));

  # Troop type overlap
  my @g1_types = @{ $g1->type // [] };
  my @g2_types = @{ $g2->type // [] };
  my %type_union = map { $_ => 1 } (@g1_types, @g2_types);
  my $shared = 0;
  for my $t (@g1_types) {
    $shared++ if any { $_ eq $t } @g2_types;
  }
  push @features, $shared, scalar(@g1_types), scalar(@g2_types);

  # Specialist type features
  my $g1_mounted = (any { $_ eq 'mounted_specialist' } @g1_types) ? 1 : 0;
  my $g1_ranged = (any { $_ eq 'ranged_specialist' } @g1_types) ? 1 : 0;
  my $g1_ground = (any { $_ eq 'ground_specialist' } @g1_types) ? 1 : 0;
  my $g1_siege = (any { $_ eq 'siege_specialist' } @g1_types) ? 1 : 0;

  my $g2_mounted = (any { $_ eq 'mounted_specialist' } @g2_types) ? 1 : 0;
  my $g2_ranged = (any { $_ eq 'ranged_specialist' } @g2_types) ? 1 : 0;
  my $g2_ground = (any { $_ eq 'ground_specialist' } @g2_types) ? 1 : 0;
  my $g2_siege = (any { $_ eq 'siege_specialist' } @g2_types) ? 1 : 0;

  my $same_specialist = (
    ($g1_mounted && $g2_mounted) ||
    ($g1_ranged && $g2_ranged) ||
    ($g1_ground && $g2_ground) ||
    ($g1_siege && $g2_siege)
  ) ? 1 : 0;

  push @features, $g1_mounted, $g1_ranged, $g1_ground, $g1_siege;
  push @features, $g2_mounted, $g2_ranged, $g2_ground, $g2_siege;
  push @features, $same_specialist;

  # Group structure
  $g1->populateBuiltinBook();
  $g2->populateBuiltinBook();
  my $g1_groups = find_groups($g1->builtInBook);
  my $g2_groups = find_groups($g2->builtInBook);

  my $g1_multi = count_multi_attr_groups($g1_groups);
  my $g2_multi = count_multi_attr_groups($g2_groups);
  push @features, $g1_multi, $g2_multi;

  # Solo buffs (non-grouped active buffs)
  my $g1_solo = count_solo_buffs($g1->builtInBook, $g1_groups);
  my $g2_solo = count_solo_buffs($g2->builtInBook, $g2_groups);
  push @features, $g1_solo, $g2_solo;

  # Total active buffs
  my @g1_buffs = grep { !$_->passive } @{ $g1->builtInBook->buffs };
  my @g2_buffs = grep { !$_->passive } @{ $g2->builtInBook->buffs };
  push @features, scalar(@g1_buffs), scalar(@g2_buffs);

  # Attribute overlap and deltas
  for my $attr ('Attack', 'Defense', 'HP') {
    my ($overlap, $delta, $ratio) = compare_attribute($g1, $g2, $attr);
    push @features, $overlap, $delta, $ratio;
  }

  # Condition features
  my ($both_attacking, $both_defending, $both_monster) = compare_conditions($g1, $g2);
  push @features, $both_attacking, $both_defending, $both_monster;

  my $g1_dragon = has_dragon_buff($g1);
  my $g2_dragon = has_dragon_buff($g2);
  push @features, $g1_dragon, $g2_dragon;

  my $cond_overlap = count_condition_overlap($g1, $g2);
  push @features, $cond_overlap;

  # Special cases
  my $is_sunce = ($g1->name eq 'Sun Ce' || $g2->name eq 'Sun Ce') ? 1 : 0;
  push @features,  $is_sunce;

  # Book text complexity
  my $g1_text_len = length($g1->builtInBook->text // '');
  my $g2_text_len = length($g2->builtInBook->text // '');
  push @features, $g1_text_len, $g2_text_len;

  return @features;
}

sub find_groups ($book) {
  my @buffs = grep { !$_->passive } @{ $book->buffs };

  my %by_key;
  for my $buff (@buffs) {
    my $conds = join('|',
      sort grep { $_ ne 'leading the army' && $_ ne 'you own the General' }
        @{ $buff->conditions // [] });

    my $key = join('|',
      $buff->value->number // 0,
      $buff->value->unit   // '',
      $conds);

    push @{ $by_key{$key} }, $buff;
  }

  my @groups;
  for my $key (keys %by_key) {
    my $buffs = $by_key{$key};
    next unless @$buffs >= 2;

    push @groups, {
      key   => $key,
      buffs => $buffs,
      attrs => [uniq map { $_->attribute } @$buffs],
    };
  }

  return \@groups;
}

sub count_multi_attr_groups ($groups) {
  my $count = 0;
  for my $group (@$groups) {
    $count++ if @{ $group->{attrs} } > 1;
  }
  return $count;
}

sub count_solo_buffs ($book, $groups) {
  my @buffs = grep { !$_->passive } @{ $book->buffs };
  my %grouped;

  for my $group (@$groups) {
    for my $buff (@{ $group->{buffs} }) {
      $grouped{refaddr($buff)} = 1;
    }
  }

  my $solo = 0;
  for my $buff (@buffs) {
    $solo++ unless exists $grouped{refaddr($buff)};
  }

  return $solo;
}

sub compare_attribute ($g1, $g2, $attr) {
  my @g1_buffs = grep { $_->attribute eq $attr && !$_->passive } @{ $g1->builtInBook->buffs };
  my @g2_buffs = grep { $_->attribute eq $attr && !$_->passive } @{ $g2->builtInBook->buffs };

  return (0, 0, 0) unless @g1_buffs && @g2_buffs;

  # Check for troop type overlap
  my %g1_types = map { $_->targetedType // 'global' => 1 } @g1_buffs;
  my %g2_types = map { $_->targetedType // 'global' => 1 } @g2_buffs;

  my $overlap = 0;
  for my $t (keys %g1_types) {
    $overlap = 1 if exists $g2_types{$t} || $t eq 'global' || (grep { $_ eq 'global' } keys %g2_types);
  }

  # Get max values
  my $g1_max = max(map { $_->value->number // 0 } @g1_buffs) // 0;
  my $g2_max = max(map { $_->value->number // 0 } @g2_buffs) // 0;

  my $delta = abs($g1_max - $g2_max);
  my $ratio = $g2_max > 0 ? $g1_max / $g2_max : 0;

  return ($overlap, $delta, $ratio);
}

sub compare_conditions ($g1, $g2) {
  my @g1_buffs = grep { !$_->passive } @{ $g1->builtInBook->buffs };
  my @g2_buffs = grep { !$_->passive } @{ $g2->builtInBook->buffs };

  my $g1_attacking = any { my @c = @{ $_->conditions // [] }; any { /Attacking/i } @c } @g1_buffs;
  my $g2_attacking = any { my @c = @{ $_->conditions // [] }; any { /Attacking/i } @c } @g2_buffs;

  my $g1_defending = any { my @c = @{ $_->conditions // [] }; any { /Defending/i } @c } @g1_buffs;
  my $g2_defending = any { my @c = @{ $_->conditions // [] }; any { /Defending/i } @c } @g2_buffs;

  my $g1_monster = any { my @c = @{ $_->conditions // [] }; any { /Monster/i } @c } @g1_buffs;
  my $g2_monster = any { my @c = @{ $_->conditions // [] }; any { /Monster/i } @c } @g2_buffs;

  return (
    ($g1_attacking && $g2_attacking) ? 1 : 0,
    ($g1_defending && $g2_defending) ? 1 : 0,
    ($g1_monster && $g2_monster) ? 1 : 0,
  );
}

sub has_dragon_buff ($general) {
  my @buffs = grep { !$_->passive } @{ $general->builtInBook->buffs };
  return any {
    my @c = @{ $_->conditions // [] };
    any { /dragon|spiritual beast/i } @c
  } @buffs;
}

sub count_condition_overlap ($g1, $g2) {
  my @g1_buffs = grep { !$_->passive } @{ $g1->builtInBook->buffs };
  my @g2_buffs = grep { !$_->passive } @{ $g2->builtInBook->buffs };

  my %g1_conds;
  for my $buff (@g1_buffs) {
    for my $cond (@{ $buff->conditions // [] }) {
      $g1_conds{$cond} = 1;
    }
  }

  my $overlap = 0;
  for my $buff (@g2_buffs) {
    for my $cond (@{ $buff->conditions // [] }) {
      $overlap++ if exists $g1_conds{$cond};
    }
  }

  return $overlap;
}

1;

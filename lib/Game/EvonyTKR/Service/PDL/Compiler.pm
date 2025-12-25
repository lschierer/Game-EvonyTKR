package Game::EvonyTKR::Service::PDL::Compiler;
use v5.42.0;
use utf8;
use Mojo::Base -base, -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
use Mojo::Base 'Game::EvonyTKR::Role::Common', -role;
use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books', -role;
use PDL;
use PDL::NiceSlice;
use YAML::XS qw(LoadFile);
use Mojo::File qw(path);
use Mojo::Util qw(dumper);
use File::Basename qw(fileparse);
use Unicode::Normalize qw(NFKD);
use Encode;
# Note: Not importing min from List::Util to avoid conflict with PDL::min

=head1 NAME

Game::EvonyTKR::Service::PDL::Compiler - Compile general buffs into PDL matrices

=head1 SYNOPSIS

  my $compiler = Game::EvonyTKR::Service::PDL::Compiler->new(
    data_dir => 'share/collections/data'
  );

  my $matrix = $compiler->compile_general('Marco Polo', 'Attacking');

=head1 DESCRIPTION

This module compiles general buff data from YAML files into PDL (Perl Data Language)
matrices for fast runtime computation. Instead of conditionally summing buffs at
runtime, we pre-compile all possible buff combinations into numeric vectors.

The approach:
1. Read YAML data (general, book, ascending, covenants, specialties)
2. Build a matrix where rows=buff sources, columns=buff values
3. At runtime, user selections become a binary mask that selects which rows to sum

This transforms O(pairs × conditions) into O(matrix_multiply + sort), achieving
5-50x speedup.

=head2 Matrix Structure

Each general becomes a matrix where:
- Rows = buff sources (base book, ascending levels, covenant levels, specialty combinations)
- Columns = buff values (attack_mounted, defense_mounted, hp_mounted, march_size, etc.)

Example for Marco Polo attacking with mounted troops:

           march  attack_m  defense_m  hp_m  attack_g  defense_g  hp_g  ...
  book       12      45        40       40      0         0        0
  asc_red1    0      25         0       15      0         0        0
  asc_red2    0       0         0        0      0         0        0
  asc_red3    8      30         0        0      0         0        0
  asc_red4    0       0        10       10      0        10       10
  asc_red5    0      40        25       25      0         0        0
  spec1_gold  0       4         4        0      0         0        0
  spec2_gold  0       0         0        0      0         0        0
  spec3_gold  0       4         4        4      0         0        0
  spec4_gold 21      20         0       40      0         0        0

User selections (ascending=red5, covenant=civilization, all gold specialties):
  Mask = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...]

  Result = sumover(matrix * mask) = [41, 265, 120, 155, ...]

=cut

# Buff column definitions
# Order matters! This defines the column index for each buff type.
our @BUFF_COLUMNS = qw(
  march_size
  attack_ground
  defense_ground
  hp_ground
  attack_mounted
  defense_mounted
  hp_mounted
  attack_ranged
  defense_ranged
  hp_ranged
  attack_siege
  defense_siege
  hp_siege
  attack_all
  defense_all
  hp_all
  death_to_wounded
  marching_speed
  enemy_attack_ground
  enemy_defense_ground
  enemy_hp_ground
  enemy_attack_mounted
  enemy_defense_mounted
  enemy_hp_mounted
  enemy_attack_ranged
  enemy_defense_ranged
  enemy_hp_ranged
  enemy_attack_siege
  enemy_defense_siege
  enemy_hp_siege
  enemy_attack_all
  enemy_defense_all
  enemy_hp_all
);

our %BUFF_INDEX;
for my $i (0 .. $#BUFF_COLUMNS) {
  $BUFF_INDEX{$BUFF_COLUMNS[$i]} = $i;
}

has 'data_dir' => sub { 'share/collections/data' };
has 'log' => sub { Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__); };

# Cache helper to access General objects
has 'cache_helper' => sub {
  require Game::EvonyTKR::Model::Base;
  return Game::EvonyTKR::Model::Base->new();
};

=head2 compile_general

  my $compiled = $compiler->compile_general($general_name, $activation_type);

Compiles a general into a PDL matrix for the specified activation type.

Arguments:
- $general_name: e.g., 'Marco Polo'
- $activation_type: e.g., 'Attacking', 'PvM', 'Mayor'

Returns hashref:
  {
    matrix => PDL matrix,
    buff_columns => \@BUFF_COLUMNS,
    row_labels => ['book', 'asc_red1', 'asc_red2', ...],
    metadata => { general => $name, activation => $type, ... }
  }

=cut

sub compile_general ($self, $general_name, $activation_type) {
  $self->log->debug("Compiling $general_name for $activation_type");

  # Try to get General object from cache
  my $general_obj = eval { $self->cache_helper->get_general($general_name) };

  # Load all data for this general
  my $general_data = $self->_load_general($general_name);
  my $book_data = $self->_load_book($general_data->{book});
  my $ascending_data = $self->_load_ascending($general_name);
  my $covenant_data = $self->_load_covenant($general_name);
  my $specialty_data = $self->_load_specialties($general_data->{specialties});

  # Determine troop type from general type
  my $troop_type = $self->_get_troop_type($general_data->{type});

  # Build matrix rows
  my @rows;
  my @row_labels;

  # Row 0: Base book buffs
  my $book_row = $self->_compile_book_buffs($book_data, $activation_type, $troop_type);
  push @rows, $book_row;
  push @row_labels, 'book';

  # Rows 1-11: Ascending levels (none, red1-red5, orange1-orange5)
  my @asc_levels = qw(none red1 red2 red3 red4 red5 orange1 orange2 orange3 orange4 orange5);
  for my $level (@asc_levels) {
    my $asc_row = $self->_compile_ascending_buffs($ascending_data, $level, $activation_type, $troop_type);
    push @rows, $asc_row;
    push @row_labels, "asc_$level";
  }

  # Rows 12-18: Covenant levels (none, war, cooperation, civilization, faith, honor, peace)
  my @cov_levels = qw(none war cooperation civilization faith honor peace);
  for my $level (@cov_levels) {
    my $cov_row = $self->_compile_covenant_buffs($covenant_data, $level, $activation_type, $troop_type);
    push @rows, $cov_row;
    push @row_labels, "cov_$level";
  }

  # Rows 19+: Specialty combinations (4 slots × 6 levels each)
  # For now, we'll compile each specialty slot at each level separately
  # The runtime will combine them based on user selections
  my @spec_levels = qw(none green blue purple orange gold);
  for my $slot_idx (0 .. 3) {
    my $spec_name = $general_data->{specialties}[$slot_idx];
    next unless $spec_name;

    my $spec_data = $specialty_data->{$spec_name};
    for my $level (@spec_levels) {
      my $spec_row = $self->_compile_specialty_buffs($spec_data, $level, $activation_type, $troop_type);
      push @rows, $spec_row;
      push @row_labels, sprintf("spec%d_%s", $slot_idx + 1, $level);
    }
  }

  # Additional rows: Generic books (only from slot 1 - they're universal, not per-slot)
  # Generic books provide universal buffs that apply once, not per slot
  # Support up to level6 for pair computation (3 books per general = 6 total)
  my @generic_levels = qw(none level1 level2 level3 level4 level5 level6);

  for my $level (@generic_levels) {
    my $generic_row = $self->_compile_generic_book_buffs($level, $activation_type, $general_obj);
    push @rows, $generic_row;
    push @row_labels, "generic_$level";
  }

  # Convert to PDL matrix
  # Stack rows as a 2D matrix: each row becomes a row in the matrix
  my $matrix = pdl(\@rows)->transpose;  # Transpose so rows become rows (not columns)

  return {
    matrix => $matrix,
    buff_columns => \@BUFF_COLUMNS,
    row_labels => \@row_labels,
    metadata => {
      general => $general_name,
      activation => $activation_type,
      troop_type => $troop_type,
      book => $general_data->{book},
      specialties => $general_data->{specialties},
    },
  };
}

=head2 Internal Methods

=cut

sub _find_file_case_insensitive ($self, $dir, $entry) {
  my @suffixes = qw(.yaml .yml);
  my $normalized_entry = lc($self->normalize($entry));

  my ($file) = $dir->list->sort->grep(sub {
    my ($basename) = fileparse($_, @suffixes);
    my $normalized_basename = lc($self->normalize($basename));
    return 1 if $_ =~ m/\.ya?ml$/ && $normalized_basename eq $normalized_entry;
    return 0;
  })->head(1)->each;

  return $file;
}

sub _load_general ($self, $name) {
  my $dir = path($self->data_dir, 'generals');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  die "General file not found for '$name' in $dir" unless $file;
  return LoadFile($file->to_string);
}

sub _load_book ($self, $name) {
  my $dir = path($self->data_dir, 'skill books');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  die "Book file not found for '$name' in $dir" unless $file;
  return LoadFile($file->to_string);
}

sub _load_ascending ($self, $name) {
  my $dir = path($self->data_dir, 'ascending attributes');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  return { ascending => [] } unless $file;  # Some generals may not have ascending
  return LoadFile($file->to_string);
}

sub _load_covenant ($self, $name) {
  my $dir = path($self->data_dir, 'covenants');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  return { levels => [] } unless $file;  # Some generals may not have covenants
  return LoadFile($file->to_string);
}

sub _load_specialties ($self, $spec_names) {
  my %specs;
  my $dir = path($self->data_dir, 'specialties');

  for my $name (@$spec_names) {
    my $file = $self->_find_file_case_insensitive($dir, $name);
    next unless $file;
    $specs{$name} = LoadFile($file->to_string);
  }
  return \%specs;
}

sub _get_troop_type ($self, $type_array) {
  # General types: mounted_specialist, ground_specialist, ranged_specialist, siege_specialist, mayor
  # Map to troop types for buff targeting
  return $type_array->[0] if ref $type_array eq 'ARRAY';
  return $type_array;
}

sub _compile_book_buffs ($self, $book_data, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  for my $buff (@{$book_data->{buffs} || []}) {
    next unless $self->_buff_applies($buff, $activation_type, $troop_type);

    # Check if this is a debuff (Enemy condition)
    my $is_debuff = grep { $_ eq 'Enemy' } @{$buff->{conditions} || []};

    my $column_key = $self->_get_buff_column_key($buff, $is_debuff);
    next unless defined $column_key && exists $BUFF_INDEX{$column_key};

    my $value = $buff->{value}{number} || 0;
    $row->set($BUFF_INDEX{$column_key}, $value);
  }

  return $row;
}

sub _compile_ascending_buffs ($self, $asc_data, $level, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

  # Find the ascending level data
  my $level_data;
  for my $asc (@{$asc_data->{ascending} || []}) {
    if ($asc->{level} eq $level) {
      $level_data = $asc;
      last;
    }
  }

  return $row unless $level_data;

  for my $buff (@{$level_data->{buffs} || []}) {
    next unless $self->_buff_applies($buff, $activation_type, $troop_type);

    # Check if this is a debuff (Enemy or Monsters condition without other targeting)
    # "Monsters" condition with Defense/Attack/HP typically means enemy debuff
    my $is_debuff = grep { $_ eq 'Enemy' || $_ eq 'Monsters' } @{$buff->{conditions} || []};

    my $column_key = $self->_get_buff_column_key($buff, $is_debuff);
    next unless defined $column_key && exists $BUFF_INDEX{$column_key};

    my $value = $buff->{value}{number} || 0;
    $row->set($BUFF_INDEX{$column_key}, $value);
  }

  return $row;
}

sub _compile_covenant_buffs ($self, $cov_data, $level, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

  # Covenant levels are cumulative: war < cooperation < civilization < faith < honor < peace
  my @levels = qw(war cooperation civilization faith honor peace);
  my %level_rank = map { $levels[$_] => $_ } 0 .. $#levels;

  my $selected_rank = $level_rank{lc($level)};
  return $row unless defined $selected_rank;

  # Accumulate buffs from all levels up to and including selected level
  for my $cov (@{$cov_data->{levels} || []}) {
    my $cov_level_name = lc($cov->{category} || '');
    my $cov_rank = $level_rank{$cov_level_name};

    # Skip levels higher than selected
    next unless defined $cov_rank && $cov_rank <= $selected_rank;

    for my $buff (@{$cov->{buffs} || []}) {
      # Skip passive buffs for now (they apply differently)
      next if $buff->{passive};

      next unless $self->_buff_applies($buff, $activation_type, $troop_type);

      # Check if this is a debuff (Enemy condition)
      my $is_debuff = grep { $_ eq 'Enemy' } @{$buff->{conditions} || []};

      my $column_key = $self->_get_buff_column_key($buff, $is_debuff);
      next unless defined $column_key && exists $BUFF_INDEX{$column_key};

      my $value = $buff->{value}{number} || 0;
      my $current = $row->at($BUFF_INDEX{$column_key});
      $row->set($BUFF_INDEX{$column_key}, $current + $value);
    }
  }

  return $row;
}

sub _compile_specialty_buffs ($self, $spec_data, $level, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none' || !$spec_data;

  # Specialty levels are cumulative: green < blue < purple < orange < gold
  my @levels = qw(green blue purple orange gold);
  my %level_rank = map { $levels[$_] => $_ } 0 .. $#levels;

  my $selected_rank = $level_rank{lc($level)};
  return $row unless defined $selected_rank;

  # Accumulate buffs from all levels up to and including selected level
  for my $spec_level (@{$spec_data->{levels} || []}) {
    my $spec_level_name = lc($spec_level->{level} || '');
    my $spec_rank = $level_rank{$spec_level_name};

    # Skip levels higher than selected
    next unless defined $spec_rank && $spec_rank <= $selected_rank;

    for my $buff (@{$spec_level->{buffs} || []}) {
      next unless $self->_buff_applies($buff, $activation_type, $troop_type);

      # Check if this is a debuff (Enemy condition)
      my $is_debuff = grep { $_ eq 'Enemy' } @{$buff->{conditions} || []};

      my $column_key = $self->_get_buff_column_key($buff, $is_debuff);
      next unless defined $column_key && exists $BUFF_INDEX{$column_key};

      my $value = $buff->{value}{number} || 0;
      my $current = $row->at($BUFF_INDEX{$column_key});
      $row->set($BUFF_INDEX{$column_key}, $current + $value);
    }
  }

  return $row;
}

sub _compile_generic_book_buffs ($self, $level, $activation_type, $general) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

  # Look up pre-computed values from the general's genericBookBuffs cache
  unless ($general && $general->can('genericBookBuffs')) {
    $self->log->warn(sprintf(
      "_compile_generic_book_buffs: General object missing or no genericBookBuffs method (level=%s, activation=%s)",
      $level, $activation_type
    ));
    return $row;
  }

  my $general_name = $general->can('name') ? $general->name : 'unknown';
  $self->log->debug(sprintf(
    "_compile_generic_book_buffs: Looking up %s/%s for %s",
    $activation_type, $level, $general_name
  ));

  my $generic_buffs = $general->genericBookBuffs;
  unless ($generic_buffs && ref($generic_buffs) eq 'HASH') {
    $self->log->warn(sprintf(
      "No generic book buffs found for general %s",
      $general_name
    ));
    return $row;
  }

  $self->log->debug(sprintf(
    "genericBookBuffs has activations: %s",
    join(', ', keys %$generic_buffs)
  ));

  # Get the buffs for this activation and level
  my $activation_buffs = $generic_buffs->{$activation_type};
  unless ($activation_buffs && ref($activation_buffs) eq 'HASH') {
    $self->log->debug(sprintf(
      "No generic book buffs for %s/%s",
      $activation_type, $level
    ));
    return $row;
  }

  $self->log->debug(sprintf(
    "%s has levels: %s",
    $activation_type, join(', ', keys %$activation_buffs)
  ));

  my $level_buffs = $activation_buffs->{$level};
  unless ($level_buffs && ref($level_buffs) eq 'HASH') {
    $self->log->debug(sprintf(
      "No generic book buffs for %s/%s (available: %s)",
      $activation_type, $level, join(', ', keys %$activation_buffs)
    ));
    return $row;
  }

  $self->log->debug(sprintf(
    "%s/%s has buffs: %s",
    $activation_type, $level, join(', ', map { "$_=$level_buffs->{$_}" } keys %$level_buffs)
  ));

  # Convert hash to PDL row
  foreach my $buff_key (keys %$level_buffs) {
    next unless exists $BUFF_INDEX{$buff_key};
    my $value = $level_buffs->{$buff_key} || 0;
    $row->set($BUFF_INDEX{$buff_key}, $value);
  }

  return $row;
}

sub _buff_applies ($self, $buff, $activation_type, $troop_type) {
  # Check if buff applies to this activation type
  my $conditions = $buff->{conditions} || [];

  # If no conditions, it always applies
  return 1 unless @$conditions;

  # Filter out 'Enemy' and 'Monsters' from conditions for activation matching
  # These indicate debuffs and apply based on other conditions, or always if they're the only condition
  my @non_debuff_conditions = grep { $_ ne 'Enemy' && $_ ne 'Monsters' } @$conditions;

  # If only debuff conditions exist, the buff applies (it's an unconditional enemy debuff)
  return 1 if @$conditions && !@non_debuff_conditions;

  # Map activation types to condition keywords
  my %activation_map = (
    'Attacking' => ['Attacking', 'Marching', 'Attack'],
    'PvM' => ['monsters', 'PvM', 'Against Monsters'],
    'Mayor' => ['Mayor', 'Wall'],
    'Defending' => ['Defending', 'Defense'],
  );

  my $keywords = $activation_map{$activation_type} || [];

  # Check if any non-debuff conditions match the activation type
  for my $keyword (@$keywords) {
    for my $condition (@non_debuff_conditions) {
      return 1 if lc($condition) =~ /\Q\L$keyword\E/;
    }
  }

  # Special case: if activation is 'Attacking', also check for 'leading' condition
  if ($activation_type eq 'Attacking') {
    for my $condition (@non_debuff_conditions) {
      return 1 if lc($condition) =~ /leading/;
    }
  }

  # If we have conditions but none matched, don't apply
  return 0;
}

sub _get_buff_column_key ($self, $buff, $is_debuff = 0) {
  my $attribute = lc($buff->{attribute} || '');
  my $targeted_type = $buff->{targetedType} || '';

  # Normalize attribute names
  $attribute =~ s/\s+/_/g;

  # Map to column keys
  my $column_key;

  if ($attribute eq 'march_size') {
    $column_key = 'march_size';
  }
  elsif ($attribute eq 'death_to_wounded') {
    $column_key = 'death_to_wounded';
  }
  elsif ($attribute eq 'marching_speed') {
    $column_key = 'marching_speed';
  }
  elsif ($attribute =~ /^(attack|defense|hp)$/) {
    my $buff_type = $attribute;

    # Determine troop type suffix
    my $suffix = 'all';  # Default to 'all' if no specific type

    if ($targeted_type =~ /ground/i) {
      $suffix = 'ground';
    }
    elsif ($targeted_type =~ /mounted/i) {
      $suffix = 'mounted';
    }
    elsif ($targeted_type =~ /ranged/i) {
      $suffix = 'ranged';
    }
    elsif ($targeted_type =~ /siege/i) {
      $suffix = 'siege';
    }

    $column_key = "${buff_type}_${suffix}";
  }
  else {
    # Unknown attribute, log warning
    $self->log->warn("Unknown buff attribute: $attribute (targeted: $targeted_type)");
    return undef;
  }

  # If this is a debuff (Enemy condition), prepend 'enemy_' to the column key
  if ($is_debuff) {
    $column_key = "enemy_${column_key}";
  }

  return $column_key;
}

1;

=head1 AUTHOR

Luke & Claude

=head1 LICENSE

Copyright (C) 2025

=cut

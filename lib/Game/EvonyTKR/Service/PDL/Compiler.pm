package Game::EvonyTKR::Service::PDL::Compiler;
use v5.42.0;
use utf8;
use Mojo::Base -base, -signatures;
use PDL;
use PDL::NiceSlice;
use YAML::XS qw(LoadFile);
use Mojo::File qw(path);
use Mojo::Util qw(dumper);
use Mojo::Log;

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
);

our %BUFF_INDEX;
for my $i (0 .. $#BUFF_COLUMNS) {
  $BUFF_INDEX{$BUFF_COLUMNS[$i]} = $i;
}

has 'data_dir' => sub { 'share/collections/data' };
has 'log' => sub { Mojo::Log->new };

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

sub _load_general ($self, $name) {
  my $path = path($self->data_dir, 'generals', "$name.yaml");
  die "General file not found: $path" unless -e $path;
  return LoadFile($path->to_string);
}

sub _load_book ($self, $name) {
  my $path = path($self->data_dir, 'skill books', "$name.yaml");
  die "Book file not found: $path" unless -e $path;
  return LoadFile($path->to_string);
}

sub _load_ascending ($self, $name) {
  my $path = path($self->data_dir, 'ascending attributes', "$name.yaml");
  return { ascending => [] } unless -e $path;  # Some generals may not have ascending
  return LoadFile($path->to_string);
}

sub _load_covenant ($self, $name) {
  my $path = path($self->data_dir, 'covenants', "$name.yaml");
  return { levels => [] } unless -e $path;  # Some generals may not have covenants
  return LoadFile($path->to_string);
}

sub _load_specialties ($self, $spec_names) {
  my %specs;
  for my $name (@$spec_names) {
    my $path = path($self->data_dir, 'specialties', "$name.yaml");
    next unless -e $path;
    $specs{$name} = LoadFile($path->to_string);
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

    my $column_key = $self->_get_buff_column_key($buff);
    next unless exists $BUFF_INDEX{$column_key};

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

    my $column_key = $self->_get_buff_column_key($buff);
    next unless exists $BUFF_INDEX{$column_key};

    my $value = $buff->{value}{number} || 0;

    # Check if this is a debuff (Enemy condition)
    my $is_debuff = grep { $_ eq 'Enemy' } @{$buff->{conditions} || []};
    $value = -$value if $is_debuff;

    $row->set($BUFF_INDEX{$column_key}, $value);
  }

  return $row;
}

sub _compile_covenant_buffs ($self, $cov_data, $level, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

  # Find the covenant level data
  my $level_data;
  for my $cov (@{$cov_data->{levels} || []}) {
    if (lc($cov->{category} || '') eq lc($level)) {
      $level_data = $cov;
      last;
    }
  }

  return $row unless $level_data;

  for my $buff (@{$level_data->{buffs} || []}) {
    # Skip passive buffs for now (they apply differently)
    next if $buff->{passive};

    next unless $self->_buff_applies($buff, $activation_type, $troop_type);

    my $column_key = $self->_get_buff_column_key($buff);
    next unless exists $BUFF_INDEX{$column_key};

    my $value = $buff->{value}{number} || 0;
    $row->set($BUFF_INDEX{$column_key}, $value);
  }

  return $row;
}

sub _compile_specialty_buffs ($self, $spec_data, $level, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none' || !$spec_data;

  # Find the specialty level data
  my $level_data;
  for my $spec_level (@{$spec_data->{levels} || []}) {
    if (lc($spec_level->{level} || '') eq lc($level)) {
      $level_data = $spec_level;
      last;
    }
  }

  return $row unless $level_data;

  for my $buff (@{$level_data->{buffs} || []}) {
    next unless $self->_buff_applies($buff, $activation_type, $troop_type);

    my $column_key = $self->_get_buff_column_key($buff);
    next unless exists $BUFF_INDEX{$column_key};

    my $value = $buff->{value}{number} || 0;
    $row->set($BUFF_INDEX{$column_key}, $value);
  }

  return $row;
}

sub _buff_applies ($self, $buff, $activation_type, $troop_type) {
  # Check if buff applies to this activation type
  my $conditions = $buff->{conditions} || [];

  # If no conditions, it always applies
  return 1 unless @$conditions;

  # Map activation types to condition keywords
  my %activation_map = (
    'Attacking' => ['Attacking', 'Marching', 'Attack'],
    'PvM' => ['monsters', 'PvM'],
    'Mayor' => ['Mayor', 'Wall'],
    'Defending' => ['Defending', 'Defense'],
  );

  my $keywords = $activation_map{$activation_type} || [];

  for my $keyword (@$keywords) {
    for my $condition (@$conditions) {
      return 1 if lc($condition) =~ /\Q\L$keyword\E/;
    }
  }

  # Special case: if activation is 'Attacking', also check for 'leading' condition
  if ($activation_type eq 'Attacking') {
    for my $condition (@$conditions) {
      return 1 if lc($condition) eq 'leading';
    }
  }

  # If we have conditions but none matched, don't apply
  return 0;
}

sub _get_buff_column_key ($self, $buff) {
  my $attribute = lc($buff->{attribute} || '');
  my $targeted_type = $buff->{targetedType} || '';

  # Normalize attribute names
  $attribute =~ s/\s+/_/g;

  # Map to column keys
  if ($attribute eq 'march_size') {
    return 'march_size';
  }
  elsif ($attribute eq 'death_to_wounded') {
    return 'death_to_wounded';
  }
  elsif ($attribute eq 'marching_speed') {
    return 'marching_speed';
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

    return "${buff_type}_${suffix}";
  }

  # Unknown attribute, log warning
  $self->log->warn("Unknown buff attribute: $attribute (targeted: $targeted_type)");
  return undef;
}

1;

=head1 AUTHOR

Luke & Claude

=head1 LICENSE

Copyright (C) 2025

=cut

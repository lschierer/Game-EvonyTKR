package Game::EvonyTKR::Service::PDL::Compiler;
use v5.42.0;
use utf8;
use Moo;
use experimental 'signatures';
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';
with 'Game::EvonyTKR::Role::Constants::Books';
with 'Game::EvonyTKR::Role::Constants::BuffConstants';
with 'Game::EvonyTKR::Role::Constants::Covenants';
with 'Game::EvonyTKR::Role::Constants::Specialties';
use PDL;
use PDL::NiceSlice;
use YAML::XS           qw(LoadFile);
use Path::Tiny         qw(path);
use Data::Dumper       qw(Dumper);
use File::Basename     qw(fileparse);
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
  $BUFF_INDEX{ $BUFF_COLUMNS[$i] } = $i;
}

has 'data_dir' => (
  is      => 'ro',
  default => sub {'share/collections/data'},
);

has 'log' => (
  is      => 'ro',
  lazy    => 1,
  default => sub { Log::Handler->get_logger(__PACKAGE__); },
);

# Cache helper to access General objects
has 'cache_helper' => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    require Game::EvonyTKR::Model::Base;
    return Game::EvonyTKR::Model::Base->new();
  },
);

# Model loaders and conflict service for generic book selection.
# Lazy: only built the first time a generic book row is compiled.
has '_model_books_loader' => (
  is      => 'ro',
  lazy    => 1,
  default => sub ($self) {
    require Game::EvonyTKR::Loader::Books;
    my $loader =
      Game::EvonyTKR::Loader::Books->new(data_dir => $self->data_dir);
    $loader->load_all;
    return $loader;
  },
);

has '_model_generals_loader' => (
  is      => 'ro',
  lazy    => 1,
  default => sub ($self) {
    require Game::EvonyTKR::Loader::Generals;
    require Path::Tiny;
    my $loader = Game::EvonyTKR::Loader::Generals->new(
      data_dir => Path::Tiny::path($self->data_dir, 'generals')->stringify);
    $loader->load_all;
    return $loader;
  },
);

has '_conflicts_service' => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    require Game::EvonyTKR::Service::Conflicts;
    return Game::EvonyTKR::Service::Conflicts->new;
  },
);

has '_general_model_cache' => (
  is      => 'ro',
  default => sub { {} },
);

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
  my $general_data   = $self->_load_general($general_name);
  my $book_data      = $self->_load_book($general_data->{book});
  my $ascending_data = $self->_load_ascending($general_name);
  my $covenant_data  = $self->_load_covenant($general_name);
  my $specialty_data = $self->_load_specialties($general_data->{specialties});

  # Create a minimal object for generic book selection
  # We just need an object with name() and type() methods
  my $general_obj = bless {
    name => $general_name,
    type => $general_data->{type},
    },
    'Game::EvonyTKR::Service::PDL::Compiler::SimpleGeneral';

  # Determine troop type from general type
  my $troop_type = $self->_get_troop_type($general_data->{type});

  # Build matrix rows
  my @rows;
  my @row_labels;

  # Row 0: Base book buffs
  my $book_row =
    $self->_compile_book_buffs($book_data, $activation_type, $troop_type);
  push @rows,       $book_row;
  push @row_labels, 'book';

  # Rows 1-11: Ascending levels (none, purple1-purple5, red1-red5)
  # Order matches Role::Constants::AscendingAttributes: purple (lower) < red (higher)
  my @asc_levels =
    qw(none purple1 purple2 purple3 purple4 purple5 red1 red2 red3 red4 red5);
  for my $level (@asc_levels) {
    my $asc_row = $self->_compile_ascending_buffs($ascending_data, $level,
      $activation_type, $troop_type);
    push @rows,       $asc_row;
    push @row_labels, "asc_$level";
  }

  # Rows 12-18: Covenant levels — derived from Role::Constants::Covenants::CovenantCategoryValues
  my @cov_levels = $self->CovenantCategoryValues->@*;
  for my $level (@cov_levels) {
    my $cov_row =
      $self->_compile_covenant_buffs($covenant_data, $level, $activation_type,
      $troop_type);
    push @rows,       $cov_row;
    push @row_labels, "cov_$level";
  }

  # Rows 19+: Specialty combinations (4 slots × 6 levels each)
  # For now, we'll compile each specialty slot at each level separately
  # The runtime will combine them based on user selections
  my @spec_levels = $self->SpecialtyLevelValues->@*;
  for my $slot_idx (0 .. 3) {
    my $spec_name = $general_data->{specialties}[$slot_idx];
    next unless $spec_name;

    my $spec_data = $specialty_data->{$spec_name};
    for my $level (@spec_levels) {
      my $spec_row =
        $self->_compile_specialty_buffs($spec_data, $level, $activation_type,
        $troop_type);
      push @rows,       $spec_row;
      push @row_labels, sprintf("spec%d_%s", $slot_idx + 1, $level);
    }
  }

# Additional rows: Generic books (only from slot 1 - they're universal, not per-slot)
# Generic books provide universal buffs that apply once, not per slot
# top6_levelN rows select top 6 books at level N, used for pair primary (no duplicates)
  my @generic_levels =
    qw(none level1 level2 level3 level4 top6_level1 top6_level2 top6_level3 top6_level4);

  for my $level (@generic_levels) {
    my $generic_row =
      $self->_compile_generic_book_buffs($level, $activation_type,
      $general_obj);
    push @rows,       $generic_row;
    push @row_labels, "generic_$level";
  }

  # Convert to PDL matrix
  # Stack rows as a 2D matrix: each row becomes a row in the matrix
  my $matrix =
    pdl(\@rows)->transpose;    # Transpose so rows become rows (not columns)

  return {
    matrix       => $matrix,
    buff_columns => \@BUFF_COLUMNS,
    row_labels   => \@row_labels,
    metadata     => {
      general     => $general_name,
      activation  => $activation_type,
      troop_type  => $troop_type,
      book        => $general_data->{book},
      specialties => $general_data->{specialties},
    },
  };
}

=head2 Internal Methods

=cut

sub _find_file_case_insensitive ($self, $dir, $entry) {
  my @suffixes         = qw(.yaml .yml);
  my $normalized_entry = lc($self->normalize($entry));

  my @files    = sort { $a cmp $b } $dir->children;
  my @matching = grep {
    my ($basename) = fileparse($_, @suffixes);
    my $normalized_basename = lc($self->normalize($basename));
    $_ =~ m/\.ya?ml$/ && $normalized_basename eq $normalized_entry;
  } @files;

  return $matching[0] if @matching;
  return;
}

sub _load_general ($self, $name) {
  my $dir  = path($self->data_dir, 'generals');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  die "General file not found for '$name' in $dir" unless $file;
  return LoadFile($file->stringify);
}

sub _load_book ($self, $name) {
  my $dir  = path($self->data_dir, 'skill books');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  die "Book file not found for '$name' in $dir" unless $file;
  return LoadFile($file->stringify);
}

sub _load_ascending ($self, $name) {
  my $dir  = path($self->data_dir, 'ascending attributes');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  return { ascending => [] }
    unless $file;    # Some generals may not have ascending
  return LoadFile($file->stringify);
}

sub _load_covenant ($self, $name) {
  my $dir  = path($self->data_dir, 'covenants');
  my $file = $self->_find_file_case_insensitive($dir, $name);
  return { levels => [] } unless $file;   # Some generals may not have covenants
  return LoadFile($file->stringify);
}

sub _load_specialties ($self, $spec_names) {
  my %specs;
  my $dir = path($self->data_dir, 'specialties');

  for my $name (@$spec_names) {
    my $file = $self->_find_file_case_insensitive($dir, $name);
    next unless $file;
    $specs{$name} = LoadFile($file->stringify);
  }
  return \%specs;
}

sub _get_troop_type ($self, $type_array) {
# General types: mounted_specialist, ground_specialist, ranged_specialist, siege_specialist, mayor
# Map to troop types for buff targeting
  return $type_array->[0] if ref $type_array eq 'ARRAY';
  return $type_array;
}

# One in-game wall-general effect is often encoded as several per-troop buff
# entries differing only in targetedType (e.g. "ranged troops and siege
# machines' attack +20%" is two YAML entries). When collapsing those into the
# 'all' columns they must count once. $scope keeps cumulative sources
# (covenant/specialty levels, generic books) from deduping across levels.
sub _is_wall_duplicate ($self, $seen, $scope, $buff, $is_debuff, $troop_type) {
  return 0 unless $troop_type eq 'wall' && !$is_debuff;
  return 0
    unless $self->_buff_targeted_type($buff) =~
    /ground|mounted|ranged|siege/i;
  my $sig = join "\x1f", $scope, lc($buff->{attribute} // ''),
    ($buff->{value}{number} // 0),
    sort @{ $buff->{conditions} || [] };
  return $seen->{$sig}++ ? 1 : 0;
}

sub _compile_book_buffs ($self, $book_data, $activation_type, $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  my %seen_wall;
  for my $buff (@{ $book_data->{buffs} || [] }) {
    next unless $self->_buff_applies($buff, $activation_type, $troop_type);

    # Check if this is a debuff (Enemy condition)
    my $is_debuff = grep { $_ eq 'Enemy' } @{ $buff->{conditions} || [] };

    my $column_key = $self->_get_buff_column_key($buff, $is_debuff, $troop_type);
    next unless defined $column_key && exists $BUFF_INDEX{$column_key};
    next
      if $self->_is_wall_duplicate(\%seen_wall, '', $buff, $is_debuff,
      $troop_type);

    my $value   = $buff->{value}{number} || 0;
    my $current = $row->at($BUFF_INDEX{$column_key});
    $row->set($BUFF_INDEX{$column_key}, $current + $value);
  }

  return $row;
}

sub _compile_ascending_buffs ($self, $asc_data, $level, $activation_type,
  $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

  # Handle both YAML formats:
  # Format 1: ascending: [ {level: red1, ...}, ... ]
  # Format 2: ascending: { levels: [ {level: red1, ...}, ... ] }
  my $ascending_list = $asc_data->{ascending} || [];
  if (ref($ascending_list) eq 'HASH' && exists $ascending_list->{levels}) {
    $ascending_list = $ascending_list->{levels} || [];
  }
  $ascending_list = [] unless ref($ascending_list) eq 'ARRAY';

  # Find the ascending level data
  my $level_data;
  for my $asc (@$ascending_list) {
    if ($asc->{level} eq $level) {
      $level_data = $asc;
      last;
    }
  }

  return $row unless $level_data;

  my %seen_wall;
  for my $buff (@{ $level_data->{buffs} || [] }) {
    next unless $self->_buff_applies($buff, $activation_type, $troop_type);

# Check if this is a debuff (Enemy or Monsters condition without other targeting)
# "Monsters" condition with Defense/Attack/HP typically means enemy debuff
    my $is_debuff =
      grep { $_ eq 'Enemy' || $_ eq 'Monsters' } @{ $buff->{conditions} || [] };

    my $column_key = $self->_get_buff_column_key($buff, $is_debuff, $troop_type);
    next unless defined $column_key && exists $BUFF_INDEX{$column_key};
    next
      if $self->_is_wall_duplicate(\%seen_wall, '', $buff, $is_debuff,
      $troop_type);

    my $value   = $buff->{value}{number} || 0;
    my $current = $row->at($BUFF_INDEX{$column_key});
    $row->set($BUFF_INDEX{$column_key}, $current + $value);
  }

  return $row;
}

sub _compile_covenant_buffs ($self, $cov_data, $level, $activation_type,
  $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

# Covenant levels are cumulative: war < cooperation < peace < faith < honor < civilization
  # Order derived from Role::Constants::Covenants::CovenantCategoryValues (excludes 'none')
  my @levels     = grep { $_ ne 'none' } $self->CovenantCategoryValues->@*;
  my %level_rank = map { $levels[$_] => $_ } 0 .. $#levels;

  my $selected_rank = $level_rank{ lc($level) };
  return $row unless defined $selected_rank;

  # Accumulate buffs from all levels up to and including selected level
  my %seen_wall;
  for my $cov (@{ $cov_data->{levels} || [] }) {
    my $cov_level_name = lc($cov->{category} || '');
    my $cov_rank       = $level_rank{$cov_level_name};

    # Skip levels higher than selected
    next unless defined $cov_rank && $cov_rank <= $selected_rank;

    for my $buff (@{ $cov->{buffs} || [] }) {
      next unless $self->_buff_applies($buff, $activation_type, $troop_type);

      # Check if this is a debuff (Enemy condition)
      my $is_debuff = grep { $_ eq 'Enemy' } @{ $buff->{conditions} || [] };

      my $column_key =
        $self->_get_buff_column_key($buff, $is_debuff, $troop_type);
      next unless defined $column_key && exists $BUFF_INDEX{$column_key};
      next
        if $self->_is_wall_duplicate(\%seen_wall, $cov_level_name, $buff,
        $is_debuff, $troop_type);

      my $value   = $buff->{value}{number} || 0;
      my $current = $row->at($BUFF_INDEX{$column_key});
      $row->set($BUFF_INDEX{$column_key}, $current + $value);
    }
  }

  return $row;
}

sub _compile_specialty_buffs ($self, $spec_data, $level, $activation_type,
  $troop_type) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none' || !$spec_data;

  # Specialty levels are cumulative: green < blue < purple < orange < gold
  # Order derived from Role::Constants::Specialties::SpecialtyLevelValues (excludes 'none')
  my @levels     = grep { $_ ne 'none' } $self->SpecialtyLevelValues->@*;
  my %level_rank = map { $levels[$_] => $_ } 0 .. $#levels;

  my $selected_rank = $level_rank{ lc($level) };
  return $row unless defined $selected_rank;

  # Accumulate buffs from all levels up to and including selected level
  my %seen_wall;
  for my $spec_level (@{ $spec_data->{levels} || [] }) {
    my $spec_level_name = lc($spec_level->{level} || '');
    my $spec_rank       = $level_rank{$spec_level_name};

    # Skip levels higher than selected
    next unless defined $spec_rank && $spec_rank <= $selected_rank;

    for my $buff (@{ $spec_level->{buffs} || [] }) {
      next unless $self->_buff_applies($buff, $activation_type, $troop_type);

      # Check if this is a debuff (Enemy condition)
      my $is_debuff = grep { $_ eq 'Enemy' } @{ $buff->{conditions} || [] };

      my $column_key =
        $self->_get_buff_column_key($buff, $is_debuff, $troop_type);
      next unless defined $column_key && exists $BUFF_INDEX{$column_key};
      next
        if $self->_is_wall_duplicate(\%seen_wall, $spec_level_name, $buff,
        $is_debuff, $troop_type);

      my $value   = $buff->{value}{number} || 0;
      my $current = $row->at($BUFF_INDEX{$column_key});
      $row->set($BUFF_INDEX{$column_key}, $current + $value);
    }
  }

  return $row;
}

# TODO (Phase 2/3 - Pairs): This simplified implementation does NOT perform conflict detection.
# For single generals, partial conflicts are acceptable (general still gets the book).
# For PAIRS, two identical partial conflicts become a full conflict (neither general gets the book).
# When implementing pair support, we'll need to:
#   1. Use Game::EvonyTKR::Service::Conflicts for proper conflict detection
#   2. Pre-compute genericBookBuffs during data loading (like External/General/Loader.pm does)
#   3. Handle the case where pairs lose books due to double partial conflicts
sub _compile_generic_book_buffs ($self, $level, $activation_type, $general) {
  my $row = zeros(scalar @BUFF_COLUMNS);

  return $row if $level eq 'none';

# Compute generic books on-demand using simplified selection (no conflict detection)
  my $generic_buffs =
    $self->_select_best_generic_books_simple($general, $activation_type,
    $level);

  # Convert hash to PDL row
  foreach my $buff_key (keys %$generic_buffs) {
    next unless exists $BUFF_INDEX{$buff_key};
    my $value = $generic_buffs->{$buff_key} || 0;
    $row->set($BUFF_INDEX{$buff_key}, $value);
  }

  return $row;
}

# Simplified generic book selection without conflict detection
# This is sufficient for single generals (partial conflicts are OK)
# For pairs, see TODO comment above
sub _select_best_generic_books_simple ($self, $general, $activation_type,
  $book_level) {
  my %buffs = ();

  unless ($general) {
    $self->log->warn("_select_best_generic_books_simple: No general provided");
    return \%buffs;
  }

  # Determine troop type from general
  my $general_name =
    ref($general) && $general->{name} ? $general->{name} : $general;
  my $troop_type = $self->_get_troop_type($general->{type});

  # Determine activation key
  my $activation_key = $activation_type eq 'PvM' ? 'PvM' : 'default';

  # Get ordered list of best books for this troop type
  my $best_books = $self->BestSkillBooks->{$troop_type}{$activation_key};
  unless ($best_books) {
    $self->log->warn(sprintf(
      "No BestSkillBooks found for troop_type=%s, activation=%s",
      $troop_type, $activation_key
    ));
    return \%buffs;
  }

  # Sort by priority
  my @sorted_book_names =
    sort { $best_books->{$a} <=> $best_books->{$b} } keys %$best_books;

  # top6_levelN selects top 6 books at level N (for pair primary, no duplicates)
  # levelN selects top 3 books at level N (single general)
  my ($book_count, $book_level_num);
  if ($book_level =~ /^top(\d+)_level(\d+)$/) {
    $book_count    = $1;
    $book_level_num = $2;
  }
  else {
    $book_count    = 3;
    $book_level_num = $book_level =~ /(\d+)/ ? $1 : 4;
  }
# Walk the ranked pool skipping books that conflict with the general's
# built-in book, taking the first $book_count compatible ones. A general only
# gets 3 generic books in game; the ranked pool exists so a conflict with a
# top book falls through to the next one (e.g. Aethelflaed conflicts with the
# Against-Monster attack book and takes the plain attack book instead).
  my $general_model = $self->_general_model($general_name);
  my @selected_books;
  foreach my $book_name (@sorted_book_names) {
    last if scalar @selected_books >= $book_count;
    my $base_name = $book_name =~ s/^Level \d+ //r;
    if ($general_model) {
      my $book_model = $self->_model_books_loader->get_generic_book(
        sprintf('%s (level %d)', $base_name, $book_level_num));
      if ($book_model) {
        my $compatible =
          $self->_conflicts_service->is_general_and_book_compatible(
          $general_model, $book_model, { delta_threshold => 15 });
        unless ($compatible) {
          $self->log->debug(sprintf(
            'Skipping generic book "%s" for %s: conflicts with built-in book',
            $book_name, $general_name
          ));
          next;
        }
      }
    }
    push @selected_books, $book_name;
  }

# Load and sum buffs from selected books
# Note: Generic book filenames include the level (e.g., "Level 4 Ground Troop Attack.yaml")
  my $books_dir = path($self->data_dir, 'generic books');
  my %seen_wall;
  foreach my $book_name (@selected_books) {
    # book_name already has "Level X" prefix (e.g., "Level 4 March Size")
    # But we need to use the correct level from book_level_num
    my $base_name      = $book_name =~ s/^Level \d+ //r;
    my $full_book_name = "Level $book_level_num $base_name";

    # Load the book YAML
    my $book_file =
      $self->_find_file_case_insensitive($books_dir, $full_book_name);
    unless ($book_file) {
      $self->log->warn("Could not find generic book: $full_book_name");
      next;
    }

    my $book_data = LoadFile($book_file->stringify);

    # Generic books have a flat structure with buffs at top level
    foreach my $buff (@{ $book_data->{buffs} || [] }) {
      next unless $self->_buff_applies($buff, $activation_type, $troop_type);

      my $is_debuff = grep { $_ eq 'Enemy' } @{ $buff->{conditions} || [] };
      my $column_key =
        $self->_get_buff_column_key($buff, $is_debuff, $troop_type);
      next unless defined $column_key && exists $BUFF_INDEX{$column_key};
      next
        if $self->_is_wall_duplicate(\%seen_wall, $full_book_name, $buff,
        $is_debuff, $troop_type);

      my $value = $buff->{value}{number} || 0;
      $buffs{$column_key} += $value;
    }
  }

  $self->log->debug(sprintf(
    "Selected generic books for %s/%s/level%d: %s",
    $general_name,   $activation_type,
    $book_level_num, join(', ', map {"$_=$buffs{$_}"} keys %buffs)
  ));

  return \%buffs;
}

# Model::General with inflated builtInBook, for conflict checks. Returns
# undef (and selection proceeds unfiltered) when the general or book cannot
# be resolved, so partial test datasets do not die here.
sub _general_model ($self, $general_name) {
  my $cache = $self->_general_model_cache;
  return $cache->{$general_name} if exists $cache->{$general_name};

  my $model = eval {
    my $g = $self->_model_generals_loader->get_general(
      $self->normalize($general_name));
    return undef unless $g;
    if (!$g->builtInBook && $g->builtInBookName) {
      my $book =
        $self->_model_books_loader->get_skill_book($g->builtInBookName);
      $g->builtInBook($book) if $book;
    }
    $g->builtInBook ? $g : undef;
  };
  if ($@) {
    $self->log->warn(
      "Could not build general model for '$general_name': $@");
    $model = undef;
  }
  unless ($model) {
    $self->log->warn(sprintf(
      'No general model with built-in book for "%s"; '
        . 'generic book selection will not be conflict-filtered',
      $general_name
    ));
  }
  return $cache->{$general_name} = $model;
}

sub _buff_applies ($self, $buff, $activation_type, $troop_type) {
  my $conditions = $buff->{conditions} || [];

  # If no conditions, it always applies
  return 1 unless @$conditions;

  # Separate debuff conditions (Enemy, Monsters) from activation conditions
  my %is_debuff = map { $_ => 1 } $self->DebuffConditionValues->@*;
  my @non_debuff_conditions = grep { !$is_debuff{$_} } @$conditions;

  # If only debuff conditions exist, the buff applies (it's an unconditional enemy debuff)
  return 1 if !@non_debuff_conditions;

  # Use BuffConditionValues as the canonical source of condition→activation mappings.
  # Also normalise any alternate condition spellings via MappedConditionNames.
  my $condition_map  = $self->BuffConditionValues;
  my $condition_aliases = $self->MappedConditionNames;

  for my $condition (@non_debuff_conditions) {
    # MappedConditionNames and BuffConditionValues are Const::Fast restricted hashes;
    # must use exists before accessing to avoid "disallowed key" exceptions.
    my $canonical = (exists $condition_aliases->{$condition})
      ? $condition_aliases->{$condition}
      : $condition;
    if (exists $condition_map->{$canonical}) {
      my $activation_map = $condition_map->{$canonical};
      return 1
        if exists $activation_map->{$activation_type}
        && $activation_map->{$activation_type};
    }
  }

  return 0;
}

# Data files spell the troop-targeting field four ways: targetedType,
# targetedTroops (array), troop, and class. Model::Buff::from_hash handles
# the first three; raw-YAML consumers like this compiler must accept all.
sub _buff_targeted_type ($self, $buff) {
  my $tt = $buff->{targetedType} // $buff->{targetedTroops} // $buff->{troop}
    // $buff->{class} // '';
  return ref $tt eq 'ARRAY' ? join(', ', @$tt) : $tt;
}

sub _get_buff_column_key ($self, $buff, $is_debuff = 0, $troop_type = '') {
  my $attribute     = lc($buff->{attribute} || '');
  my $targeted_type = $self->_buff_targeted_type($buff);

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
    my $suffix = 'all';    # Default to 'all' if no specific type

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

# Wall generals defend with all troop types at once, so their troop-specific
# buffs are collapsed into the 'all' columns ("dropping the adjectives").
# Debuffs stay broken out per troop type. See _is_wall_duplicate for the
# double-count guard.
    $suffix = 'all' if !$is_debuff && $troop_type eq 'wall';

    $column_key = "${buff_type}_${suffix}";
  }
  else {
    # Unknown attribute, log warning
    $self->log->warn(
      "Unknown buff attribute: $attribute (targeted: $targeted_type)");
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

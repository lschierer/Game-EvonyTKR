package Game::EvonyTKR::Service::Conflicts;
use v5.42.0;
use utf8::all;
use Mojo::Base -base,                                               -signatures;
use Mojo::Base 'WebFramework::Role::Logger',                     -role;
use Mojo::Base 'Game::EvonyTKR::Role::Common',                      -role;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Pairs',          -role;
use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',    -role;
with 'Game::EvonyTKR::Role::Constants::GeneralConstants';

require Data::Printer;
use List::AllUtils qw( any none uniq );
use Carp;

use Game::EvonyTKR::Service::Conflicts::BuffComparator;
use Game::EvonyTKR::Service::Conflicts::GroupedBuffComparator;
use Game::EvonyTKR::Service::Conflicts::BookComparator;

# Configuration
has build_index       => 0;
has assume_g1_is_main => 1;
has main_has_dragon   => 1;
has main_has_spirit   => 1;
has asst_has_dragon   => 0;
has asst_has_spirit   => 0;
#TODO -- toggle on WALL buffs.
has allow_wall_buffs => 1;
has persistence      => undef;    # Set via load_from_persistence()

# Output caches
has ProcessedGenerals       => sub { {} };
has groups_by_conflict_type => sub { {} };
has by_general              => sub { {} };
has cache_hits              => 0;

# Constants
has TROOP_BIT => sub { {
  'Ground Troops'  => 1,
  'Ranged Troops'  => 2,
  'Mounted Troops' => 4,
  'Siege Machines' => 8,
} };

has CONDLESS => sub { {
  'March Size'             => 1,
  'Marching Speed'         => 1,
  'Stamina cost'           => 1,
  'Double Items Drop Rate' => 1,
} };

has TRIADS => sub { {
  'Attack'  => 1,
  'Defense' => 1,
  'HP'      => 1,
} };

# Main conflict detection
sub are_generals_compatible ($self, $g1, $g2) {
  $self->logger->debug(sprintf('testing %s and %s', $g1->name, $g2->name));

  # Check cache first (includes ML predictions loaded from persistence)
  my $cached = $self->_check_cache($g1, $g2);
  if (defined $cached) {
    return $cached == 0
      ? 1
      : 0;    # 0 = compatible (return 1), 1 = conflict (return 0)
  }

  return 1 unless $self->_troop_overlap($g1, $g2);

# TEMPORARY: Disable fallback to see if ML data is loading correctly
#$self->logger->warn(sprintf(
#  'No cached ML result for %s ↔ %s - returning compatible (fallback disabled)',
#  $g1->name, $g2->name
#));
#return 1;  # Assume compatible if no ML data

  # Try grouped buff detection first (handles complex cases like Haakon/Cheng)
  my $grouped = Game::EvonyTKR::Service::Conflicts::GroupedBuffComparator->new(
    service => $self);
  my $grouped_result = $grouped->conflicts($g1, $g2);
  if (defined $grouped_result) {
    if ($grouped_result) {
      $self->_record_conflict($g1, $g2);
    }
    else {
      $self->_record_compatible($g1, $g2);
    }
    return $grouped_result ? 0 : 1;
  }

  # Fall back to individual buff comparison
  my $comparator =
    Game::EvonyTKR::Service::Conflicts::BuffComparator->new(service => $self);

  for my $b1 (@{ $g1->builtInBook->buffs }) {
    for my $b2 (@{ $g2->builtInBook->buffs }) {
      if ($comparator->conflicts($b1, $b2, $g1, $g2)) {
        $self->logger->debug(sprintf(
          '%s/%s conflict: %s vs %s (conds: [%s] vs [%s])',
          $g1->name, $g2->name, $b1->attribute, $b2->attribute,
          join(',', @{ $b1->conditions // [] }),
          join(',', @{ $b2->conditions // [] })
        ));
        $self->_record_conflict($g1, $g2);
        return 0;
      }
    }
  }

  # No conflicts found - record as compatible
  $self->_record_compatible($g1, $g2);
  return 1;
}

# Check if generic book conflicts with general's builtin book
# Returns: true if compatible, false if conflicts
sub is_general_and_book_compatible ($self, $general, $book, $opts = {}) {
  my $comparator =
    Game::EvonyTKR::Service::Conflicts::BookComparator->new(service => $self);
  my $result = $comparator->conflicts($general, $book, $opts);

  # 0 = compatible, 1+ = some kind of conflict
  return $result == 0 ? 1 : 0;
}

# Load conflict data from persistence layer
sub load_from_persistence ($self, $persistence) {
  $self->persistence($persistence);    # Store reference for ML lookups

  my $conflicts = $self->load_all_conflicts();

  if ($conflicts && ref($conflicts) eq 'HASH') {
    $self->by_general($conflicts);
    my $count = scalar(keys %$conflicts);
    $self->logger->debug("Loaded conflicts for $count generals from persistence");
  }

  return $self;
}

# Store new conflict to persistence
sub store_to_persistence ($self, $persistence, $g1_name, $g2_name, $conflicts) {
  $persistence->store_conflict($g1_name, $g2_name, $conflicts);
  return $self;
}

sub _check_cache ($self, $g1, $g2) {
  my $name1 = $self->assume_g1_is_main ? $g1->name : $g2->name;
  my $name2 = $self->assume_g1_is_main ? $g2->name : $g1->name;

  # Normalize names to match how they're stored in persistence
  my $norm1 = $self->normalize($name1);
  my $norm2 = $self->normalize($name2);

  if (exists $self->by_general->{$norm1}{$norm2}) {
    $self->cache_hits($self->cache_hits + 1);
    return $self->by_general->{$norm1}{$norm2}
      ;    # Return the conflict status (0 or 1)
  }
  return undef;
}

sub _check_ml_prediction ($self, $g1, $g2) {
 # ML predictions are already loaded into by_general via load_from_persistence()
 # Just check the cache - if it exists, it's an ML prediction
  my $cached = $self->_check_cache($g1, $g2);

  # If found in cache, return it in the expected format
  if (defined $cached) {
    return {
      conflict   => $cached,
      confidence => 1.0        # No confidence info stored in SQLite
    };
  }

  return undef;
}

sub _record_conflict ($self, $g1, $g2) {
  # Normalize names to match how they're stored in persistence
  my $norm1 = $self->normalize($g1->name);
  my $norm2 = $self->normalize($g2->name);

  $self->by_general->{$norm1}{$norm2} = 1;
  $self->by_general->{$norm2}{$norm1} = 1;

  # Also store to persistence if available
  if ($self->persistence) {
    $self->persistence->store_conflict($norm1, $norm2, 1);
  }
}

sub _record_compatible ($self, $g1, $g2) {
  # Normalize names to match how they're stored in persistence
  my $norm1 = $self->normalize($g1->name);
  my $norm2 = $self->normalize($g2->name);

  $self->logger->debug(sprintf('Recording compatible: %s ↔ %s', $norm1, $norm2));

  $self->by_general->{$norm1}{$norm2} = 0;
  $self->by_general->{$norm2}{$norm1} = 0;

  # Also store to persistence if available
  if ($self->persistence) {
    $self->persistence->store_conflict($norm1, $norm2, 0);
  }
}

sub _troop_overlap ($self, $g1, $g2) {
  my $mask1 = 0;
  my $mask2 = 0;

  for my $buff (@{ $g1->builtInBook->buffs }) {
    next if $buff->passive;
    $mask1 |= $self->TROOP_BIT->{ $buff->targetedType // '' } // 0;
  }

  for my $buff (@{ $g2->builtInBook->buffs }) {
    next if $buff->passive;
    $mask2 |= $self->TROOP_BIT->{ $buff->targetedType // '' } // 0;
  }

  return $mask1 & $mask2;
}

sub preseed ($self, $new_by_general, $new_groups_by_conflict_type) {
  $self->by_general($new_by_general);
  $self->groups_by_conflict_type($new_groups_by_conflict_type);
}

1;
__END__

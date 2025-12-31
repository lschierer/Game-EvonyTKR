package Game::EvonyTKR::Role::Persistence::Pairs;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;
use List::AllUtils qw(uniq none all any);
use List::UtilsBy;
use Hash::Util qw(lock_keys);
use Carp;

sub get_conflict_detector ($self) {
  # Always reload from SQLite - it's fast and avoids stale cache issues
  my $cd = $self->initialize_conflict_detector();
  return $cd;
}

sub add_wire_pair ($self, $wire_pair) {
  my $key = $self->wire_pair_to_key($wire_pair);

  # Store to SQLite (single source of truth)
  eval { $self->persistence->store_pair($key, $wire_pair); };
  if ($@) {
    $self->log_error("Failed to store pair to persistence: $@");
    return 0;
  }

  return 1;
}

sub get_pair ($self, $key) {
  $key = $self->normalize($key);

  # Load directly from SQLite
  my $wire_pair;
  eval { $wire_pair = $self->persistence->get_pair($key); };
  if ($@) {
    $self->log_error("Failed to get pair from persistence: $@");
  }

  unless ($wire_pair) {
    my @caller_info = caller(1);
    $self->log_warn(sprintf(
      'cannot find pair for key %s (called from %s line %d)',
      $key,
      $caller_info[3] // 'unknown',
      $caller_info[2] // 0
    ));
    return;
  }

  my $pair = Game::EvonyTKR::Model::General::Pair->from_wire_hash($wire_pair);
  unless ($pair) {
    $self->log_error(sprintf(
      'cannot create pair from wire_pair %s/%s/%s; key %s',
      $wire_pair->{type},      $wire_pair->{primary},
      $wire_pair->{secondary}, $key
    ));
    return;
  }
  return $pair;
}

sub get_pair_list ($self, $requested_type = undef) {
  my $list = [];

  # Load from SQLite
  my $pairs_by_type = {};
  eval {
    my $all_types = $self->persistence->get_all_pair_types();
    foreach my $type (@$all_types) {
      my $type_pairs = $self->persistence->list_pairs_by_type($type);
      $pairs_by_type->{$type} = $type_pairs;
    }
  };

  foreach my $type (keys($pairs_by_type->%*)) {
    if (defined $requested_type && $type ne $requested_type) {
      $self->log_debug(sprintf(
        'skipping type %s as it does not match requested type %s',
        $type, $requested_type
      ));
      next;
    }
    $list = [
      uniq(
        $list->@*,
        map { $self->wire_pair_to_key($_) } $pairs_by_type->{$type}->@*
      )
    ];
  }
  $list = [sort $list->@*];
  return $list;
}

# Batch version: returns inflated Pair objects directly (avoids N+1 queries)
# Delegates to persistence layer for efficient type-based query using PostgreSQL LIKE
# Options:
#   skip_generic_books => 1  # Skip expensive generic book precomputation (for diagnostics)
sub get_pairs_for_type_batch ($self, $type, $opts = {}) {
  return $self->persistence->get_pairs_for_type_batch($type, $opts);
}

sub get_all_pairs ($self) {
  my $pairs = [];

  eval {
    my $all_types = $self->persistence->get_all_pair_types();
    foreach my $type (@$all_types) {
      my $type_pairs = $self->get_pairs_for_type_batch($type);
      push @$pairs, @$type_pairs;
    }
  };
  if ($@) {
    $self->log_error("Failed to load all pairs: $@");
  }

  return $pairs;
}

sub validatePairParams($self, $ascendingLevel, $primaryCovenantLevel,
  $primarySpecialties, $secondaryCovenantLevel, $secondarySpecialties,) {
  my $data_model = Game::EvonyTKR::Model::Data->new();

  if (!$data_model->checkAscendingLevel($ascendingLevel)) {
    $self->log_warn(
      "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
    $ascendingLevel = 'none';
  }

  if (!$self->checkCovenantLevel($primaryCovenantLevel)) {
    $self->log_warn(
      sprintf('Invalid covenantLevel: %s, using default "civilization"',
        $primaryCovenantLevel)
    );
    $primaryCovenantLevel = 'none';
  }

  @$primarySpecialties =
    $data_model->normalizeSpecialtyLevels(@$primarySpecialties);

  if (!$self->checkCovenantLevel($secondaryCovenantLevel)) {
    $self->log_warn(
      sprintf('Invalid covenantLevel: %s, using default "civilization"',
        $secondaryCovenantLevel)
    );
    $secondaryCovenantLevel = 'none';
  }

  @$secondarySpecialties =
    $data_model->normalizeSpecialtyLevels(@$secondarySpecialties);

  return {
    ascendingLevel         => $ascendingLevel,
    primaryCovenantLevel   => $primaryCovenantLevel,
    primarySpecialties     => $primarySpecialties,
    secondaryCovenantLevel => $secondaryCovenantLevel,
    secondarySpecialties   => $secondarySpecialties,
  };
}

sub initialize_conflict_detector($self, $conflict_detector = undef) {
  require Game::EvonyTKR::Service::Conflicts;

  $conflict_detector //= Game::EvonyTKR::Service::Conflicts->new(
    build_index      => 1,
    asst_has_dragon  => 1,
    asst_has_spirit  => 1,
    allow_wall_buffs => 1,
  );

  # Load from SQLite - this is the source of truth
  $conflict_detector->load_from_persistence($self->persistence);

  return $conflict_detector;
}

sub load_all_conflicts ($self) {
  return $self->persistence->load_all_conflicts();
}

sub wire_pair_to_key($self, $wire_pair) {
  my $key = sprintf('%s/%s/%s',
    $wire_pair->{type},
    $self->normalize($wire_pair->{primary}),
    $self->normalize($wire_pair->{secondary}),
  );
  $key = lc($key);
  return $key;
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Pairs - Pair persistence operations

=head1 DESCRIPTION

Handles general pair storage, retrieval, and conflict detection.

=cut

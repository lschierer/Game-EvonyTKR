package Game::EvonyTKR::Role::Persistence::Pairs;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;
use List::AllUtils qw(uniq none all any);
use List::UtilsBy;
use Carp;

sub pairs_by_type ($self, $new_pbt = undef) {
  state $pairs_by_type = {};
  if (defined($new_pbt) && ref($new_pbt) eq 'HASH') {
    $pairs_by_type = $new_pbt;
  }
  return $pairs_by_type;
}

sub get_conflict_detector ($self) {
  # Always reload from SQLite - it's fast and avoids stale cache issues
  my $cd = $self->initialize_conflict_detector();
  return $cd;
}

sub setup_pairs_by_type ($self) {
  my $pairs = $self->pairs_by_type();
  foreach my $key ($self->GeneralKeys->@*) {
    $pairs->{$key} = []
      unless (ref($pairs) eq 'HASH' && exists $pairs->{$key});
  }
  # Store in state variable only - no memcache
  $self->pairs_by_type($pairs);
  return 1;
}

sub add_wire_pair ($self, $wire_pair) {
  my $key = $self->wire_pair_to_key($wire_pair);

  # Get current pairs_by_type from state
  my $npbt = $self->pairs_by_type();

  # Add wire_pair to the appropriate type array
  my %hash = map { $self->wire_pair_to_key($_) => $_ }
    ($wire_pair, ($npbt->{ $wire_pair->{type} } // [])->@*);
  $npbt->{ $wire_pair->{type} } =
    [sort { $self->wire_pair_to_key($a) cmp $self->wire_pair_to_key($b) }
      values %hash];

  # Store to SQLite
  eval { $self->persistence->store_pair($key, $wire_pair); };
  if ($@) {
    $self->logger->error("Failed to store pair to persistence: $@");
    return 0;
  }

  # Update state
  $self->pairs_by_type($npbt);
  return 1;
}

sub get_pair ($self, $key) {
  $key = $self->normalize($key);
  $key =~ s/ /_/g;

  # Load directly from SQLite
  my $wire_pair;
  eval { $wire_pair = $self->persistence->get_pair($key); };
  if ($@) {
    $self->logger->error("Failed to get pair from persistence: $@");
  }

  unless ($wire_pair) {
    $self->logger->warn(sprintf('cannot find pair for key %s', $key));
    return;
  }

  my $pair = Game::EvonyTKR::Model::General::Pair->from_wire_hash($wire_pair);
  unless ($pair) {
    $self->logger->error(sprintf(
      'cannot create pair from wire_pair %s/%s/%s; key %s',
      $wire_pair->{type},      $wire_pair->{primary},
      $wire_pair->{secondary}, $key
    ));
    return;
  }
  return $pair;
}

sub get_pairs_by_type ($self) {
  state $inflated_pairs = {};
  state $all_pairs_built = 0;

  # If pair building is complete and we've already cached, return cache
  if ($all_pairs_built && %$inflated_pairs) {
    return $inflated_pairs;
  }

  # Check if pair building is complete
  my $building_complete = $self->persistence->get_metadata('pair_building_complete');

  # Load from SQLite
  my $pairs_by_type = {};
  eval {
    my $all_types = $self->persistence->get_all_pair_types();
    foreach my $type (@$all_types) {
      my $type_pairs = $self->persistence->list_pairs_by_type($type);
      $pairs_by_type->{$type} = $type_pairs;
      $self->logger->debug(sprintf(
        'Loaded %d pairs of type %s from persistence',
        scalar(@$type_pairs), $type
      ));
    }
  };
  if ($@) {
    $self->logger->error("Failed to load pairs_by_type from persistence: $@");
  }

  # Inflate pairs
  $inflated_pairs = {};
  foreach my $type (sort keys %$pairs_by_type) {
    $self->logger->debug(sprintf(
      'Inflating %s pairs for type %s',
      scalar(@{ $pairs_by_type->{$type} }), $type
    ));
    $inflated_pairs->{$type} = [];
    foreach my $wire_pair (@{ $pairs_by_type->{$type} }) {
      if (my $pair_obj =
        Game::EvonyTKR::Model::General::Pair->from_wire_hash($wire_pair)) {
        push @{ $inflated_pairs->{$type} }, $pair_obj;
      }
    }
  }

  # Only mark as built if building is actually complete
  $all_pairs_built = $building_complete ? 1 : 0;

  return $inflated_pairs;
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
      $self->logger->debug(sprintf(
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

sub get_all_pairs ($self) {
  my $pairs_by_type = $self->get_pairs_by_type() // {};
  my $pairs         = [];

  foreach my $type (keys %$pairs_by_type) {
    push @$pairs, @{ $pairs_by_type->{$type} };
  }

  return $pairs;
}

sub validatePairParams($self, $ascendingLevel, $primaryCovenantLevel,
  $primarySpecialties, $secondaryCovenantLevel, $secondarySpecialties,) {
  my $data_model = Game::EvonyTKR::Model::Data->new();

  if (!$data_model->checkAscendingLevel($ascendingLevel)) {
    $self->logger->warn(
      "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
    $ascendingLevel = 'none';
  }

  if (!$self->checkCovenantLevel($primaryCovenantLevel)) {
    $self->logger->warn(
      sprintf('Invalid covenantLevel: %s, using default "civilization"',
        $primaryCovenantLevel)
    );
    $primaryCovenantLevel = 'none';
  }

  @$primarySpecialties =
    $data_model->normalizeSpecialtyLevels(@$primarySpecialties);

  if (!$self->checkCovenantLevel($secondaryCovenantLevel)) {
    $self->logger->warn(
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

sub wire_pair_to_key($self, $wire_pair) {
  my $key = sprintf('%s/%s/%s',
    $wire_pair->{type},
    $self->normalize($wire_pair->{primary}),
    $self->normalize($wire_pair->{secondary}),
  );
  $key = lc($key);
  $key =~ s/ /_/g;
  return $key;
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Pairs - Pair persistence operations

=head1 DESCRIPTION

Handles general pair storage, retrieval, and conflict detection.

=cut

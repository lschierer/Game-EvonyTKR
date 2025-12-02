package Game::EvonyTKR::Role::Persistence::Pairs;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;
use List::AllUtils qw(uniq none all any);
use List::UtilsBy;
use Carp;

has 'pair_cache' => sub ($job) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs__');
};

has 'conflict_cache' => sub ($job) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts__');
};

sub pairs_by_type ($self, $new_pbt = undef) {
  state $pairs_by_type = {};
  if (defined($new_pbt) && ref($new_pbt) eq 'HASH') {
    $pairs_by_type = $new_pbt;
  }
  return $pairs_by_type;
}

sub conflict_data ($self, $new_cd = undef) {
  state $conflict_data = {};
  if (defined($new_cd) && ref($new_cd) eq 'HASH') {
    $conflict_data = $new_cd;
  }
  return $conflict_data;
}

sub add_conflict_data ($self, $by_general, $groups_by_conflict_type) {
  my $cas_val = $self->conflict_cache->gets('merged_conflicts');
  if (defined($cas_val) && ref($cas_val) eq 'ARRAY') {
    my $current_data = $$cas_val[1] // {
      by_general              => {},
      groups_by_conflict_type => {},
      timestamp               => time
    };

    foreach my $general (keys %$by_general) {
      $current_data->{by_general}->{$general} //= {};
      %{ $current_data->{by_general}->{$general} } = (
        %{ $current_data->{by_general}->{$general} },
        %{ $by_general->{$general} }
      );
    }

    foreach my $type (keys %$groups_by_conflict_type) {
      $current_data->{groups_by_conflict_type}->{$type} //= [];
      my %seen =
        map { $_ => 1 } @{ $current_data->{groups_by_conflict_type}->{$type} };
      push @{ $current_data->{groups_by_conflict_type}->{$type} },
        grep { !$seen{$_}++ } @{ $groups_by_conflict_type->{$type} };
    }

    $current_data->{timestamp} = time;
    $$cas_val[1] = $current_data;

    my $result = $self->conflict_cache->cas('merged_conflicts', @$cas_val);
    if ($result) {
      $self->conflict_data($current_data);
      return 1;
    }
  }
  return 0;
}

sub update_conflict_data ($self, $cd) {
  state $all_conflicts_compiled;
  state $last_update_timestamp = 0;
  state $delay;
  unless ($all_conflicts_compiled) {

    $delay++;
    $delay = $delay % 60;
    $delay = $delay ? $delay : 0.01;

    my $now = time;
    if ($now - $last_update_timestamp <= $delay) {
      $self->logger->debug(
        'update_conflict_data called too frequently, returning.');
      return;
    }
    my $cached_data = $self->conflict_cache->get('merged_conflicts');
    my $local_data  = $self->conflict_data();

    if (
      $cached_data
      && (!$local_data->{timestamp}
        || $cached_data->{timestamp} > $local_data->{timestamp})
    ) {
      $cd->preseed($cached_data->{by_general},
        $cached_data->{groups_by_conflict_type});
      $self->conflict_data($cached_data);
    }
    $last_update_timestamp = $now;

    $all_conflicts_compiled =
      $self->conflict_cache->get('conflict_building_complete');
  }
}

sub get_conflict_detector ($self) {
  state $cd;

  unless ($cd) {
    $cd = $self->initialize_conflict_detector();
  }

  Mojo::IOLoop->timer(
    0.001 => sub {
      $self->update_conflict_data($cd);
    }
  );

  return $cd;
}

sub setup_pairs_by_type ($self) {
  my $pairs = $self->pairs_by_type();
  foreach my $key ($self->GeneralKeys->@*) {
    $pairs->{$key} = []
      unless (ref($pairs) eq 'HASH' && exists $pairs->{$key});
  }
  my $success = $self->pair_cache->add('pairs_by_type', $pairs);
  my $verify  = $self->pair_cache->get('pairs_by_type');
  $self->logger->debug(sprintf(
    'After add with return value "%s", stored value: %s',
    defined($success) ? $success : 'undef return',
    Data::Printer::np($verify)
  ));
  return (defined($success) && length($success) && $success ne '0');
}

sub add_wire_pair ($self, $wire_pair) {
  my $key = $self->wire_pair_to_key($wire_pair);

  my $add_result = 0;

  my $pbt_cas_val = $self->pair_cache->gets('pairs_by_type');
  if ( defined($pbt_cas_val)
    && ref($pbt_cas_val)
    && ref($pbt_cas_val) eq 'ARRAY') {
    my $npbt = $$pbt_cas_val[1];
    my %hash = map { $self->wire_pair_to_key($_) => $_ }
      ($wire_pair, $npbt->{ $wire_pair->{type} }->@*);
    $npbt->{ $wire_pair->{type} } =
      [sort { $self->wire_pair_to_key($a) cmp $self->wire_pair_to_key($b) }
        values %hash];
    $$pbt_cas_val[1] = $npbt;
    my $pbt_result = $self->pair_cache->cas('pairs_by_type', @$pbt_cas_val);
    if ($pbt_result) {
      my $wp_result =
        $self->pair_cache->set($self->wire_pair_to_key($wire_pair), $wire_pair);
      if ($wp_result) {
        eval { $self->persistence->store_pair($key, $wire_pair); };
        if ($@) {
          $self->logger->error("Failed to store pair to persistence: $@");
        }

        $self->pairs_by_type($npbt);
        $add_result = 1;
      }
    }
  }

  return $add_result;
}

sub get_pair ($self, $key) {
  $key = $self->normalize($key);
  $key =~ s/ /_/g;

  my $wire_pair = $self->pair_cache->get($key);

  unless ($wire_pair) {
    $self->logger->debug(
      sprintf('Pair not in memcached, checking persistence for key %s', $key));
    eval {
      $wire_pair = $self->persistence->get_pair($key);
      if ($wire_pair) {
        $self->pair_cache->set($key, $wire_pair);
        $self->logger->debug("Populated memcached with pair from persistence");
      }
    };
    if ($@) {
      $self->logger->error("Failed to get pair from persistence: $@");
    }
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
  my $pairs_by_type = $self->pair_cache->get('pairs_by_type') // {};

  if (!keys %$pairs_by_type) {
    $self->logger->info(
      'pairs_by_type not in memcached, rebuilding from persistence');
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

      if (keys %$pairs_by_type) {
        $self->pair_cache->set('pairs_by_type', $pairs_by_type);
        $self->logger->info(
          'Rebuilt pairs_by_type in memcached from persistence');
      }
    };
    if ($@) {
      $self->logger->error(
        "Failed to rebuild pairs_by_type from persistence: $@");
    }
  }

  state $all_pairs_built;
  state $inflated_pairs = {};
  unless ($all_pairs_built) {
    foreach my $type (sort keys %$pairs_by_type) {
      $self->logger->debug(sprintf(
        'merging %s pairs for type %s',
        scalar(@{ $pairs_by_type->{$type} }), $type
      ));
      $inflated_pairs->{$type} = [];
      my $type_starts_at_zero = scalar(@{ $inflated_pairs->{$type} });
      foreach my $wire_pair (@{ $pairs_by_type->{$type} }) {
        my $wpk = $self->wire_pair_to_key($wire_pair);
        if (!$type_starts_at_zero
          && any { $wpk eq $self->wire_pair_to_key($_->to_wire_hash()) }
          $inflated_pairs->{$type}->@*) {
          $self->logger->debug(sprintf('pair %s is already present', $wpk));
          next;
        }
        else {
          $self->logger->debug("merging pair $wpk for type $type");
        }
        if (my $pair_obj =
          Game::EvonyTKR::Model::General::Pair->from_wire_hash($wire_pair)) {
          push @{ $inflated_pairs->{$type} }, $pair_obj;
        }
      }
    }
    $all_pairs_built = $self->pair_cache->get('pair_building_complete');
  }
  return $inflated_pairs;
}

sub get_pair_list ($self, $requested_type = undef) {
  my $list          = [];
  my $pairs_by_type = $self->pair_cache->get('pairs_by_type') // {};

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

  $conflict_detector->load_from_persistence($self->persistence);

  my $cached_conflicts = $self->conflict_cache->get('merged_conflicts');
  if ($cached_conflicts && $cached_conflicts->{by_general}) {
    my $by_gen = $conflict_detector->by_general;
    foreach my $g1 (keys %{ $cached_conflicts->{by_general} }) {
      foreach my $g2 (keys %{ $cached_conflicts->{by_general}{$g1} }) {
        $by_gen->{$g1}{$g2} = 1;
      }
    }
    $conflict_detector->by_general($by_gen);
    $self->logger->debug('Merged memcached conflicts with persistence');
  }

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

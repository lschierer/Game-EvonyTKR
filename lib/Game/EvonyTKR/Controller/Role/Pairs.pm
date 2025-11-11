use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;
require Game::EvonyTKR::Model::General::Conflict;

package Game::EvonyTKR::Controller::Role::Pairs {
  use Mojo::Base -role, -signatures;
  use List::AllUtils qw(uniq);
  use List::UtilsBy;
  use Carp;

  has 'pair_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs__');
  };

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts__');
  };

  sub get_conflict_detector ($self) {
    state $cd;
    unless ($cd) {
      $cd = $self->initialize_conflict_detector();
    }
    return $cd;
  }

  sub setup_pairs_by_type ($self) {
    my $pairs = {};
    foreach my $key ($self->GeneralKeys->@*) {
      $pairs->{$key} = [];
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

    # Store the individual pair first
    my $store_result = $self->pair_cache->set($key, $wire_pair);

    # Update pair_list efficiently using CAS
    my $cas_val = $self->pair_cache->gets('pair_list');
    if (defined($cas_val)) {
      my $current_list = $$cas_val[1] || '';
      # Only add if not already present (simple string check)
      unless (index($current_list, $key) >= 0) {
        $$cas_val[1] = $current_list ? "$current_list;$key" : $key;
        $self->pair_cache->cas('pair_list', @$cas_val);
      }
    } else {
      $self->pair_cache->add('pair_list', $key);
    }

    return $store_result;
  }

  sub sort_pair_list($self){
    my $cas_val = $self->pair_cache->gets('pair_list');
    if (defined($cas_val)) {
      my $current_list = $$cas_val[1] || '';
      my $cl = [sort split ';', $current_list];
      $$cas_val[1] = join ';', $cl->@*;
      $self->pair_cache->cas('pair_list', @$cas_val);
    }
    else {
      $self->logger->warn('no current pair_list');
    }
  }

  sub merge_into_pairs_by_type($self, $pairs_by_type, @new_pairs) {
    foreach my $np (@new_pairs) {
      unless (List::AllUtils::any { $_ eq $np->{type} } $self->GeneralKeys->@*)
      {
        $self->logger->error(sprintf(
          'pair type must be one of %s, not %s',
          join(', ', $self->GeneralKeys->@*),
          $np->{type}
        ));
        next;
      }

      $pairs_by_type->{ $np->{type} } //= [];
      $pairs_by_type->{ $np->{type} } =
        [$np, $pairs_by_type->{ $np->{type} }->@*];
    }

    foreach my $type (keys $pairs_by_type->%*){
      my %hash = map { $self->wire_pair_to_key($_) => $_ } $pairs_by_type->{$type}->@*;

      $pairs_by_type->{$type} =  [
        sort { $self->wire_pair_to_key($a) cmp $self->wire_pair_to_key($b) } values %hash
      ];

      $self->logger->debug(sprintf('after merge, there are %s %s type pairs',
      scalar($pairs_by_type->{$type}->@*), $type));
    }
    return $pairs_by_type;
  }

  sub get_pairs_by_type ($self) {
    return $self->pair_cache->get('pairs_by_type');
  }

  sub get_all_pairs ($self) {
    my $key_list = $self->pair_cache->get('pair_list') // '';
    my $keys     = [split ';', $key_list];
    my $pairs    = [];
    foreach my $pair_key ($keys->@*) {
      my $pair = $self->pair_cache->get($pair_key);
      unless ($pair) {
        $self->logger->error("failed to get $pair_key");
        next;
      }
      push @{$pairs}, $pair;
    }
    return $pairs;
  }

  sub initialize_conflict_detector($self, $conflict_detector = undef) {
    # Create or use provided conflict detector
    $conflict_detector //= Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
    );

    # Load existing conflict data from cache
    my $cached_conflicts = $self->conflict_cache->get('merged_conflicts');

    if ($cached_conflicts) {
      $self->logger->debug('Loading existing conflict data from cache');

      # Merge existing by_general conflicts
      if (my $by_general = $cached_conflicts->{by_general}) {
        while (my ($general, $conflicts) = each %$by_general) {
          $conflict_detector->by_general->{$general} = {
            %{$conflict_detector->by_general->{$general} // {}},
            %$conflicts
          };
        }
      }

      # Merge existing groups_by_conflict_type
      if (my $groups = $cached_conflicts->{groups_by_conflict_type}) {
        while (my ($type, $cached_groups) = each %$groups) {
          my $existing = $conflict_detector->groups_by_conflict_type->{$type} //= [];
          my %seen = map { $_ => 1 } @$existing;
          push @$existing, grep { !$seen{$_}++ } @$cached_groups;
        }
      }

      $self->logger->debug(sprintf(
        'Initialized conflict detector with %d generals and %d conflict types',
        scalar(keys %{$conflict_detector->by_general}),
        scalar(keys %{$conflict_detector->groups_by_conflict_type})
      ));
    } else {
      $self->logger->debug('No existing conflict data found in cache');
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

}
1;
__END__

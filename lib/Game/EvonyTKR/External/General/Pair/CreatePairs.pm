use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::CreatePairs {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  require Game::EvonyTKR::Service::Cache;
  require Game::EvonyTKR::Model::General::Conflict;

  sub register ($plugin, $app, $conf = {}) {
    $app->minion->add_task(create_pairs => __PACKAGE__);
    $app->plugins->emit('Game_EvonyTKR_External_General_Pair_CreatePairs');
  }

  has 'pair_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs:');
  };

  has 'conflict_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts:');
  };

  sub run ($self, $general_name, $type) {
    $self->logger->debug("Creating pairs for general: $general_name, type: $type");

    # Validate the type
    unless ($self->ValidateGeneralType($type)) {
      return $self->fail("Invalid general type: $type");
    }

    # Get all generals from cache
    my $generals = $self->get_generals();

    # Find the primary general
    my $primary = $generals->{$self->normalize($general_name)};
    unless ($primary) {
      return $self->fail("General '$general_name' not found");
    }

    # Filter generals for the specified type (excluding primary)
    my @matching_generals;
    foreach my $general (values %$generals) {
      next if $general->name eq $primary->name;  # Skip self

      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

      if (grep { $_ eq $type } @$general_types) {
        push @matching_generals, $general;
      }
    }

    $self->logger->info(sprintf('Found %d generals of type %s to pair with %s',
      scalar @matching_generals, $type, $general_name));

    # Initialize conflict detector
    my $conflict_detector = Game::EvonyTKR::Model::General::Conflict->new();

    my @pairs;
    my $conflicts_found = 0;

    # Build pairs with matching generals
    foreach my $secondary (@matching_generals) {
      $self->logger->debug(sprintf('Testing compatibility: %s <-> %s',
        $primary->name, $secondary->name));

      # Check for conflicts
      unless ($conflict_detector->are_generals_compatible($primary, $secondary)) {
        $self->logger->debug(sprintf('Conflict detected: %s <-> %s',
          $primary->name, $secondary->name));
        $conflicts_found++;
        next;
      }

      # Find common troop types between primary and secondary
      my $primary_types = $primary->type // [];
      $primary_types = [$primary_types] unless ref($primary_types) eq 'ARRAY';

      my $secondary_types = $secondary->type // [];
      $secondary_types = [$secondary_types] unless ref($secondary_types) eq 'ARRAY';

      my %primary_types_map = map { $_ => 1 } @$primary_types;
      my @common_types = grep { $primary_types_map{$_} } @$secondary_types;

      next unless @common_types;

      # Create pair for each common type that matches our target type
      foreach my $common_type (@common_types) {
        next unless $common_type eq $type;  # Only create pairs for the requested type

        my $pair = {
          primary   => $primary->name,
          secondary => $secondary->name,
          type      => $common_type,
        };

        push @pairs, $pair;

        $self->logger->debug(sprintf('Created pair: %s <-> %s (type: %s)',
          $pair->{primary}, $pair->{secondary}, $common_type));
      }
    }

    # Store pairs in cache
    if (@pairs) {
      my $cache_key = sprintf('%s_%s', $self->normalize($general_name), $type);
      $self->pair_cache->set($cache_key, \@pairs);
      $self->logger->info(sprintf('Stored %d pairs for %s/%s',
        scalar @pairs, $general_name, $type));
    }

    # Store conflict data
    my $conflict_data = {
      by_general => $conflict_detector->by_general,
      groups_by_conflict_type => $conflict_detector->groups_by_conflict_type,
    };

    my $conflict_key = sprintf('%s_%s_conflicts', $self->normalize($general_name), $type);
    $self->conflict_cache->set($conflict_key, $conflict_data);

    $self->logger->info(sprintf('CreatePairs completed for %s/%s: %d pairs, %d conflicts',
      $general_name, $type, scalar @pairs, $conflicts_found));
  }
}

1;

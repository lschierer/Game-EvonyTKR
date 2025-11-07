use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::CreatePairs {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  require Game::EvonyTKR::Service::Cache;
  require Game::EvonyTKR::Model::General::Conflict::Book;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(create_pairs => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts:');
  };

  sub run ($job, $general_name, $type) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run([$general_name, $type]);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));
    $job->logger->debug(
      "Creating pairs for general: $general_name, type: $type");

    # Validate the type
    unless ($job->ValidateGeneralType($type)) {
      return $job->fail("Invalid general type: $type");
    }

    # Get all generals from cache
    my $generals = $job->get_generals($job->app);

    # Find the primary general
    my $primary = $generals->{ $job->normalize($general_name) };
    unless ($primary) {
      return $job->fail("General '$general_name' not found");
    }
    $primary->populateBuiltinBook();

    unless ($primary->builtInBook) {
      my $errmessage = sprintf('failed to populate builtInBook %s for %s',
        $primary->builtInBookName, $primary->name);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    # Filter generals for the specified type (excluding primary)
    my @matching_generals;
    foreach my $general (values %$generals) {
      next if $general->name eq $primary->name;    # Skip self
      $general->populateBuiltinBook();
      unless ($general->builtInBook) {
        my $errmessage = sprintf('failed to populate builtInBook %s for %s',
          $general->builtInBookName, $general->name);
        $job->logger->error($errmessage);
        next;
      }

      # Check if this general supports the target type
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

      if (grep { $_ eq $type } @$general_types) {
        push @matching_generals, $general;
      }
    }

    $job->logger->info(sprintf(
      'Found %d generals of type %s to pair with %s',
      scalar @matching_generals,
      $type, $general_name
    ));

    # Initialize conflict detector
    my $conflict_detector = Game::EvonyTKR::Model::General::Conflict->new();

    my @pairs;
    my $conflicts_found = 0;

    # Build pairs with matching generals
    foreach my $secondary (@matching_generals) {
      # Check for conflicts
      $job->logger->debug(sprintf(
        'Testing compatibility: %s <-> %s',
        $primary->name, $secondary->name
      ));
      unless ($conflict_detector->are_generals_compatible($primary, $secondary))
      {
        $job->logger->debug(sprintf(
          'Conflict detected: %s <-> %s',
          $primary->name, $secondary->name
        ));
        $conflicts_found++;
        next;
      }

      # Since we already filtered for matching type, just create the pair
      my $pair = {
        primary   => $primary->name,
        secondary => $secondary->name,
        type      => $type,
      };

      push @pairs, $pair;

      $job->logger->debug(sprintf(
        'Created pair: %s <-> %s (type: %s)',
        $pair->{primary}, $pair->{secondary}, $type
      ));

      # Return conflict data in notes for monitor job to merge
      my $conflict_data = {
        by_general              => $conflict_detector->by_general,
        groups_by_conflict_type => $conflict_detector->groups_by_conflict_type,
      };

      $job->note(
        pairs_created           => scalar @pairs,
        pairs                   => \@pairs,
        conflicts_found         => $conflicts_found,
        by_general              => $conflict_data->{by_general},
        groups_by_conflict_type => $conflict_data->{groups_by_conflict_type},
        general_name            => $general_name,
        type                    => $type,
      );
    }

    $job->logger->info(sprintf(
      'CreatePairs completed for %s/%s: %d pairs, %d conflicts',
      $general_name, $type, scalar @pairs,
      $conflicts_found
    ));

    return $job->finish(sprintf(
      'Created %d pairs for %s as %s with %d conflicts',
      scalar @pairs,                             $general_name,
      ref($type) ? join(',', $type->@*) : $type, $conflicts_found
    ));
  }
}

1;

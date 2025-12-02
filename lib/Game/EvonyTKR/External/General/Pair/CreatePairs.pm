use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::CreatePairs {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',        -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  require Game::EvonyTKR::Service::Cache;

  sub task_name {'create_pairs'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

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
      $job->logger->error("Invalid general type: $type");
      return $job->fail("Invalid general type: $type");
    }

    # Get all generals from cache
    my $generals = $job->get_generals();

    # Find the primary general
    my ($primary) =
      grep { $job->normalize($general_name) eq $job->normalize($_->name) }
      $generals->@*;
    unless ($primary) {
      $job->logger->error("General '$general_name' not found");
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
    foreach my $general (@$generals) {
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
    my $conflict_detector = $job->initialize_conflict_detector();
    my $initial_conflicts = scalar(keys %{ $conflict_detector->by_general });
    $job->logger->info(
      sprintf('Initialized conflict detector with %d existing conflicts',
        $initial_conflicts)
    );

    my @pairs;
    my $conflicts_found      = 0;
    my $skipped_existing     = 0;
    my $compatibility_checks = 0;
    my $conflict_cache_hits  = 0;

    # Build pairs with matching generals
    foreach my $secondary (@matching_generals) {
      # Create potential pairs to check if they already exist
      my $pair_ab = {
        primary   => $primary->name,
        secondary => $secondary->name,
        type      => $type,
      };

      my $pair_ba = {
        primary   => $secondary->name,
        secondary => $primary->name,
        type      => $type,
      };

      # Check if this pair relationship already exists (either direction)
      my $pair_key_ab = $job->wire_pair_to_key($pair_ab);
      my $pair_key_ba = $job->wire_pair_to_key($pair_ba);

      # Skip if either direction already processed
      if ( $job->pair_cache->get($pair_key_ab)
        || $job->pair_cache->get($pair_key_ba)) {
        $skipped_existing++;
        $job->logger->debug(sprintf(
          'Pair relationship already exists: %s <-> %s (type: %s)',
          $primary->name, $secondary->name, $type
        ));
        next;
      }

      # Check for conflicts
      $compatibility_checks++;
      my $pre_cache_hits = $conflict_detector->cache_hits;

      $job->logger->debug(sprintf(
        'Testing compatibility: %s <-> %s',
        $primary->name, $secondary->name
      ));

      unless ($conflict_detector->are_generals_compatible($primary, $secondary))
      {
        # Check if this was a cache hit
        if ($conflict_detector->cache_hits > $pre_cache_hits) {
          $conflict_cache_hits++;
        }

        $job->logger->debug(sprintf(
          'Conflict detected: %s <-> %s',
          $primary->name, $secondary->name
        ));
        $conflicts_found++;
        next;
      }

      # Add both directions to our pairs list
      push @pairs, $pair_ab, $pair_ba;

      # Store both pairs to cache immediately to prevent duplicate work
      $job->add_wire_pair($pair_ab);
      $job->add_wire_pair($pair_ba);

      $job->logger->info(sprintf(
        'Created bidirectional pairs: %s <-> %s (type: %s)',
        $pair_ab->{primary}, $pair_ab->{secondary}, $type
      ));
    }

    # Store new conflicts to persistence
    my $final_by_general     = $conflict_detector->by_general;
    my $new_conflicts_stored = 0;

    foreach my $g1 (keys %$final_by_general) {
      foreach my $g2 (keys %{ $final_by_general->{$g1} }) {
       # Store to persistence (uses INSERT OR IGNORE so duplicates are harmless)
        $job->persistence->store_conflict($g1, $g2);
        $new_conflicts_stored++;
      }
    }

    # Note: stored count includes duplicates, actual new conflicts will be less
    $job->logger->debug(sprintf('Stored %d conflict records to persistence',
      $new_conflicts_stored));

    # Return conflict data in notes for monitor job to merge
    my $conflict_data = {
      by_general              => $conflict_detector->by_general,
      groups_by_conflict_type => $conflict_detector->groups_by_conflict_type,
    };

    $job->note(
      pairs_created           => scalar @pairs,
      pairs                   => \@pairs,
      conflicts_found         => $conflicts_found,
      conflict_cache_hits     => $conflict_cache_hits,
      by_general              => $conflict_data->{by_general},
      groups_by_conflict_type => $conflict_data->{groups_by_conflict_type},
      general_name            => $general_name,
      type                    => $type,
    );

    $job->logger->info(sprintf(
'CreatePairs completed for %s/%s: %d pairs, %d conflicts (%d cache hits), %d skipped, %d compatibility checks of %d candidates',
      $general_name,         $type,
      scalar @pairs,         $conflicts_found,
      $conflict_cache_hits,  $skipped_existing,
      $compatibility_checks, scalar @matching_generals
    ));

    return $job->finish(sprintf(
      'Created %d pairs for %s as %s with %d conflicts (%d skipped, %d checks)',
      scalar @pairs,                             $general_name,
      ref($type) ? join(',', $type->@*) : $type, $conflicts_found,
      $skipped_existing,                         $compatibility_checks
    ));
  }
}

1;

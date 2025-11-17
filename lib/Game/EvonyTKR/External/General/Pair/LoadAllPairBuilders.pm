use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $taskClass->setup_pairs_by_type();
    $app->minion->add_task(load_all_pair_builders => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    return
      if $job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_ascending_attributes', 'load_all_builtin_books',
        'load_all_generals',             'load_all_specialties',
        'load_ascending_attributes',     'load_book',
        'load_general',                  'load_specialty',
      ]
      );

    $job->logger->info('Starting LoadAllPairBuilders job');

    # Get all generals from cache
    my $generals = $job->get_generals($job->app);

    $job->logger->info(
      sprintf('Found %d generals to process', scalar keys %$generals));

    # Spawn CreatePairs jobs for each general/type combination
    my $job_count     = 0;
    my %type_batches  = ();    # Track jobs by type for balanced batching
    my $batch_count   = 0;
    my %type_counters = ();    # Track how many jobs per type

    foreach my $general_name (sort keys %$generals)
    {                          # Sort for deterministic order
      my $general = $generals->{$general_name};

      # Handle scalar vs array types for this general
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

      # Create jobs for each type this general supports
      foreach my $type (@$general_types) {
        $type_counters{$type}++;

        # High priority for first 3 generals of each type
        my $priority = ($type_counters{$type} <= 10) ? 3 : 1;
        $priority = ($type_counters{$type} <= 9) ? 4  : $priority;
        $priority = ($type_counters{$type} <= 8) ? 5  : $priority;
        $priority = ($type_counters{$type} <= 7) ? 6  : $priority;
        $priority = ($type_counters{$type} <= 6) ? 7  : $priority;
        $priority = ($type_counters{$type} <= 5) ? 8  : $priority;
        $priority = ($type_counters{$type} <= 4) ? 9  : $priority;
        $priority = ($type_counters{$type} <= 3) ? 10 : $priority;

        my $job_id = $job->minion->enqueue(
          'create_pairs' => [$general_name, $type] => {
            priority => $priority,
            expire   => 2700,
          }
        );
        $job->logger->debug(sprintf(
          'Enqueued create_pairs job %s for general %s, type %s (priority %d)',
          $job_id, $general_name, $type, $priority
        ));

        # Add to type-specific batch
        push @{ $type_batches{$type} }, $job_id;
        $job_count++;

        # Check if we can form a complete batch (one job per type)
        my @available_types =
          grep { @{ $type_batches{$_} // [] } > 0 } keys %type_batches;
        if (@available_types >= 5) {    # We have all 5 types available
          my @current_batch = ();

          # Take one job from each type
          foreach my $batch_type (
            qw(mayor ranged_specialist mounted_specialist siege_specialist ground_specialist)
          ) {
            if (@{ $type_batches{$batch_type} // [] } > 0) {
              push @current_batch, shift @{ $type_batches{$batch_type} };
            }
          }

          if (@current_batch > 0) {
            my $reduce_jid = $job->minion->enqueue(
              'reduce_batch' => [] => {
                parents  => [@current_batch],
                priority => 50,
                expire   => 2700,
              }
            );
            $job->logger->debug(sprintf(
'Spawned type-balanced reduce_batch job %s for batch %d (%d jobs)',
              $reduce_jid, ++$batch_count, scalar(@current_batch)
            ));
          }
        }
      }
    }

    # Spawn remaining jobs in type-balanced batches
    while (1) {
      my @current_batch = ();
      my $jobs_added    = 0;

      # Try to add one job from each type that has jobs remaining
      foreach my $type (
        qw(mayor ranged_specialist mounted_specialist siege_specialist ground_specialist)
      ) {
        if (@{ $type_batches{$type} // [] } > 0) {
          push @current_batch, shift @{ $type_batches{$type} };
          $jobs_added++;
        }
      }

      last if $jobs_added == 0;    # No more jobs to process

      my $reduce_jid = $job->minion->enqueue(
        'reduce_batch' => [] => {
          parents => [@current_batch],
          expire  => 2700,
        }
      );
      $job->logger->debug(sprintf(
'Spawned final type-balanced reduce_batch job %s for batch %d (%d jobs)',
        $reduce_jid, ++$batch_count, scalar(@current_batch)
      ));
    }

    $job->logger->info(sprintf(
'LoadAllPairBuilders completed: spawned %d create_pairs jobs in %d batches',
      $job_count, $batch_count
    ));
  }
}

1;
__END__

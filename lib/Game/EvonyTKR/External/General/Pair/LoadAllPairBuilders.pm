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

    return if $job->are_prereqs_outstanding();

    $job->logger->info('Starting LoadAllPairBuilders job');

    # Get all generals from cache
    my $generals = $job->get_generals($job->app);

    $job->logger->info(
      sprintf('Found %d generals to process', scalar keys %$generals));

    # Spawn CreatePairs jobs for each general/type combination
    my $job_count     = 0;
    my @current_batch = ();
    my $batch_count   = 0;

    foreach my $general_name (keys %$generals) {
      my $general = $generals->{$general_name};

      # Handle scalar vs array types for this general
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

      # Create jobs for each type this general supports
      foreach my $type (@$general_types) {
        my $job_id =
          $job->minion->enqueue('create_pairs' => [$general_name, $type]);
        $job->logger->debug(sprintf(
          'Enqueued create_pairs job %s for general %s, type %s',
          $job_id, $general_name, $type
        ));

        push @current_batch, $job_id;
        $job_count++;

        # Spawn ReduceBatch when we have 10 jobs
        if (@current_batch >= 10) {
          my $reduce_jid = $job->minion->enqueue(
            'reduce_batch' => [] => {
              parents  => [@current_batch],
              priority => 50,
            }
          );
          $job->logger->debug(sprintf(
            'Spawned reduce_batch job %s for batch %d (%d jobs)',
            $reduce_jid, ++$batch_count, scalar(@current_batch)
          ));
          @current_batch = ();
        }
      }
    }

    # Spawn final ReduceBatch for remaining jobs
    if (@current_batch > 0) {
      my $reduce_jid = $job->minion->enqueue(
        'reduce_batch' => [] => {
          parents => [@current_batch]
        }
      );
      $job->logger->debug(sprintf(
        'Spawned final reduce_batch job %s for batch %d (%d jobs)',
        $reduce_jid, ++$batch_count, scalar(@current_batch)
      ));
    }

    $job->logger->info(sprintf(
'LoadAllPairBuilders completed: spawned %d create_pairs jobs in %d batches',
      $job_count, $batch_count
    ));
  }

  sub are_prereqs_outstanding ($job) {
    my $generalLoaderFinishedCount = $job->app->minion->jobs({
      tasks  => ['load_general'],
      states => ['finished'],
    })->total // 0;

    my $generalLoaderPendingCount = $job->app->minion->jobs({
      tasks  => ['load_general'],
      states => ['active', 'inactive'],
    })->total // 0;

    my $generalLoaderFailedCount = $job->app->minion->jobs({
      tasks  => ['load_general'],
      states => ['failed'],
    })->total // 0;

    if ($generalLoaderFailedCount > 0) {
      return $job->fail('cannot build pairs if general import failed');
    }

    $job->note(generalLoaderPendingCount => $generalLoaderPendingCount);

    if ($generalLoaderPendingCount > 0) {
      my $delay = List::Util::min(2 * $generalLoaderPendingCount, 30);
      return $job->retry({ delay => $delay });
    }

    if ($generalLoaderFinishedCount > 0) {
      return 0;    # Ready to proceed
    }

    return $job->fail('no general loading jobs found');
  }

}

1;

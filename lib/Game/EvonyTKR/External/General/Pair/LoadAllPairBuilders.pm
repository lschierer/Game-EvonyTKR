use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',        -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;

  sub task_name {'load_all_pair_builders'}

  sub register ($taskClass, $app, $conf = {}) {
    return 1 unless $taskClass->SUPER::register($app, $conf);
    $taskClass->setup_pairs_by_type();
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);

    return 1;
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    $job->log_debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    return
      if $job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_generals',    'load_all_builtin_books',
        'load_all_specialties', 'load_all_ascending_attributes',
        'load_all_covenants',   'load_ml_conflicts',
      ]
      );

    $job->log_info('Starting LoadAllPairBuilders job');

    # Get all generals from cache
    my $generals = $job->get_generals();

    $job->log_info(sprintf('Found %d generals to process', scalar @$generals));

    # Spawn CreatePairs jobs for each general/type combination
    my $job_count     = 0;
    my %type_batches  = ();    # Track jobs by type for balanced batching
    my $batch_count   = 0;
    my %type_counters = ();    # Track how many jobs per type

    foreach my $general (sort { $a->name cmp $b->name } @$generals) {

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

        # Check if job already exists for this general/type in current run
        my $existing_jobs = $job->minion->jobs({
          tasks => ['create_pairs'],
          states => ['active', 'inactive']
        });
        
        my $job_exists = 0;
        while (my $existing = $existing_jobs->next) {
          if ($existing->{args} && 
              $existing->{args}[0] && $existing->{args}[0] eq $general->name &&
              $existing->{args}[1] && $existing->{args}[1] eq $type &&
              $existing->{notes} && $existing->{notes}->{prebuild_run_id} && 
              $existing->{notes}->{prebuild_run_id} eq $job->prebuild_run_id) {
            $job_exists = 1;
            last;
          }
        }
        
        if ($job_exists) {
          $job->log_debug(sprintf(
            'Skipping %s/%s - job already exists for current run', 
            $general->name, $type));
          next;
        }

        my $job_id = $job->minion->enqueue(
          'create_pairs' => [$general->name, $type] => {
            priority => $priority,
            notes    => { prebuild_run_id => $job->prebuild_run_id },
          }
        );
        $job->log_debug(sprintf(
          'Enqueued create_pairs job %s for general %s, type %s (priority %d)',
          $job_id, $general->name, $type, $priority
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
                notes    => { prebuild_run_id => $job->prebuild_run_id },
              }
            );
            $job->log_debug(sprintf(
              'Spawned type-balanced reduce_batch job %s '
                . 'for batch %d (%d jobs)',
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
          parents  => [@current_batch],
          priority => 50,
          notes    => { prebuild_run_id => $job->prebuild_run_id },
        }
      );
      $job->log_debug(sprintf(
'Spawned final type-balanced reduce_batch job %s for batch %d (%d jobs)',
        $reduce_jid, ++$batch_count, scalar(@current_batch)
      ));
    }

    $job->log_info(sprintf(
'LoadAllPairBuilders completed: spawned %d create_pairs jobs in %d batches',
      $job_count, $batch_count
    ));

    # Mark job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    return $job->finish(sprintf(
      'Spawned %d create_pairs jobs in %d batches',
      $job_count, $batch_count
    ));
  }
}

1;
__END__

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::MonitorCreatePairs {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',       -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',                      -role;
  require Game::EvonyTKR::Service::Cache;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);

    # make sure the base hash exists in memcache for merging into
    $taskClass->setup_pairs_by_type();

    $app->minion->add_task(monitor_create_pairs => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts:');
  };

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
    $job->logger->info('Starting MonitorCreatePairs job');

    my $check_interval = 10;     # seconds
    my $max_wait       = 300;    # 5 minutes max wait
    state $waited = 0;

    my %merged_by_general;
    my %merged_groups_by_conflict_type;
    my $total_pairs     = [];
    my $total_conflicts = 0;
    my $active_count    = 0;
    my $finished_count  = 0;
    my $failed_count    = 0;

    # Get processed jobs from notes to preserve state across retries
    my $processed_jobs = $job->info->{notes}->{processed_jobs} // {};

    if ($waited < $max_wait) {
      # Get all create_pairs jobs

      my $jobs = $job->minion->jobs({
        tasks  => ['create_pairs'],
        states => ['finished', 'failed', 'active', 'inactive']
      });

      if ($jobs->total == 0) {
        $waited += $check_interval;
        return $job - retry({ delay => $check_interval });
      }

      # Process finished jobs and accumulate conflict data
      while (my $job_info = $jobs->next) {
        my $state  = $job_info->{state};
        my $job_id = $job_info->{id};

        if ($state eq 'finished') {
          $finished_count++;

          # Skip if we've already processed this job
          next if $processed_jobs->{$job_id};
          $processed_jobs->{$job_id} = 1;

          # Extract conflict data from notes
          my $notes = $job_info->{notes} // {};
          $job->logger->debug(
            sprintf('Processing new job %s with notes', $job_id));

          if ($notes->{by_general}) {
            $job->logger->debug(sprintf('by_general in job is %s',
              Data::Printer::np($notes->{by_general})));
            # Merge by_general data
            foreach my $general (keys %{ $notes->{by_general} }) {
              $merged_by_general{$general} //= {};
              foreach
                my $other_general (keys %{ $notes->{by_general}->{$general} }) {
                $merged_by_general{$general}->{$other_general} = 1;
              }
            }
          }

          if ($notes->{groups_by_conflict_type}) {
            $job->logger->debug(sprintf('groups_by_conflict_type in job is %s',
              Data::Printer::np($notes->{groups_by_conflict_type})));
            # Merge groups_by_conflict_type data
            foreach
              my $conflict_type (keys $notes->{groups_by_conflict_type}->%*) {
              $merged_groups_by_conflict_type{$conflict_type} //= [];
              push @{ $merged_groups_by_conflict_type{$conflict_type} },
                @{ $notes->{groups_by_conflict_type}{$conflict_type} // [] };
            }
          }

          my $current_conflict_data = {
            by_general              => \%merged_by_general,
            groups_by_conflict_type => \%merged_groups_by_conflict_type,
          };
          if (defined($notes->{pairs})) {
            push @{$total_pairs}, $notes->{pairs}->@*;
            foreach my $pair ($total_pairs->@*) {
              # I do not need to run a ->to_hash() here
              # because I have not yet instantiated any sort of object
              $job->add_wire_pair($pair);
            }
          }
          $total_conflicts += $notes->{conflicts_found} // 0;
          $job->note(
            finished_jobs   => $finished_count,
            total_pairs     => scalar(@{$total_pairs}),
            pairs           => $total_pairs,
            total_conflicts => $total_conflicts,
            conflicts       => $current_conflict_data,
          );

        }
        elsif ($state eq 'active' || $state eq 'inactive') {
          $active_count++;
          $job->note(active_jobs => $active_count,);
        }
        elsif ($state eq 'failed') {
          $failed_count++;
          my $errmessage = sprintf('found failed job %s', $job_info->{id});
          $job->logger->error($errmessage);
          $job->note(failed_jobs => $failed_count,);
          return $job->fail($errmessage);
        }
      }

      $job->logger->debug(sprintf(
        'Monitor status: %d finished, %d active, %d failed create_pairs jobs',
        $finished_count, $active_count, $failed_count
      ));

      # Update progress notes
      $job->note(
        finished_jobs   => $finished_count,
        active_jobs     => $active_count,
        failed_jobs     => $failed_count,
        total_pairs     => $total_pairs,
        total_conflicts => $total_conflicts,
        processed_jobs  => $processed_jobs,    # Preserve state across retries
      );
    }

    # Store the merged conflict data
    my $final_conflict_data = {
      by_general              => \%merged_by_general,
      groups_by_conflict_type => \%merged_groups_by_conflict_type,
    };

    $job->conflict_cache->set('merged_conflicts', $final_conflict_data);

    # Final notes
    $job->note(
      pairs_by_type   => { total => $total_pairs },
      conflicts       => $final_conflict_data,
      total_pairs     => $total_pairs,
      total_conflicts => $total_conflicts,
    );

    $job->logger->info(sprintf(
      'MonitorCreatePairs completed: '
        . '%d total pairs, %d total conflicts, '
        . '%d generals with conflicts',
      $total_pairs, $total_conflicts, scalar keys %merged_by_general
    ));
    if ($active_count >= 1) {
      $waited += $check_interval;
      return $job->retry({ delay => $check_interval });
    }
    $job->conflict_cache->set('conflicts_complete', 1);
    return $job->finish('all pair builders complete');
  }
}

1;

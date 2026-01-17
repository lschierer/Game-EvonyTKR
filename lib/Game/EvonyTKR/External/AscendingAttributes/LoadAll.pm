use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::AscendingAttributes::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::File;

  sub task_name {'load_all_ascending_attributes'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
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
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));
    $job->logger->info('Starting load_all_ascending_attributes job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $ascendingAttributesDir = $collectionDir->child('ascending attributes');

    my @files =
      $ascendingAttributesDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->each;

    $job->logger->info(
      sprintf('Found %d ascendingAttributes files to process', scalar @files));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    foreach my $file (@files) {
      # Extract ascending attribute name from filename
      my $attr_name = $file->basename('.yaml', '.yml');

      # Check if already in persistence
      if ($job->get_ascending_attribute($attr_name)) {
        $job->logger->debug(sprintf(
          'Skipping %s - already in persistence', $attr_name));
        $skipped_count++;
        next;
      }

      # Check if job already exists for this file in current run
      my $existing_jobs = $job->minion->jobs({
        tasks  => ['load_ascending_attributes'],
        states => ['active', 'inactive']
      });

      my $job_exists = 0;
      while (my $existing = $existing_jobs->next) {
        if ( $existing->{args}
          && $existing->{args}[0]
          && $existing->{args}[0] eq $file->to_string
          && $existing->{notes}
          && $existing->{notes}->{prebuild_run_id}
          && $existing->{notes}->{prebuild_run_id} eq
          $job->info->{notes}->{prebuild_run_id}) {
          $job_exists = 1;
          last;
        }
      }

      if ($job_exists) {
        $job->logger->debug(sprintf(
          'Skipping %s - job already exists for current run',
          $attr_name));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        'load_ascending_attributes' => [$file->to_string] => {
          attempts => 3,
          delay    => rand(10),
          notes    =>
            { prebuild_run_id => $job->info->{notes}->{prebuild_run_id} },
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_ascending_attributes job %s for file %s',
        $job_id, $file->basename
      ));
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->logger->info(sprintf(
'Enqueued %d load_ascending_attributes jobs, skipped %d already in persistence',
      $enqueued_count, $skipped_count
    ));

    # Check if child jobs are still running
    if (@job_ids) {
      my $active   = 0;
      my $finished = 0;
      my $failed   = 0;

      for my $jid (@job_ids) {
        my $job_obj = $job->minion->job($jid);
        my $info    = $job_obj ? $job_obj->info : undef;

        unless ($info && $info->{state}) {
          $job->logger->debug("Job $jid: no info or state");
          next;
        }

        if ($info->{state} eq 'finished') {
          $finished++;
        }
        elsif ($info->{state} eq 'failed') {
          $failed++;
        }
        else {
          $active++;
        }
      }

      $job->logger->debug(sprintf(
        'Job status: active=%d, finished=%d, failed=%d',
        $active, $finished, $failed
      ));

      # If jobs still running, retry this coordinator job to check again later
      if ($active > 0) {
        $job->logger->info(sprintf(
          'Still waiting for %d child jobs - retrying in 5 seconds',
          $active));
        return $job->retry({ delay => $job->standard_delay });
      }

      # Fail if any child jobs failed
      if ($failed > 0) {
        my $errmsg =
          sprintf('LoadAll failed: %d child jobs failed, %d finished',
          $failed, $finished);
        $job->logger->error($errmsg);
        return $job->fail($errmsg);
      }

      $job->logger->info(sprintf(
        'All child jobs completed: %d finished, %d failed',
        $finished, $failed
      ));

      # Verify all data is actually in persistence before marking complete
      my $verified            = 0;
      my $max_verify_attempts = 10;

      for my $attempt (1 .. $max_verify_attempts) {
        my $all_in_persistence = 1;
        my $missing_count      = 0;

        foreach my $file (@files) {
          my $attr_name = $file->basename('.yaml', '.yml');
          # Query Redis directly to bypass state cache
          unless ($job->get_ascending_attribute($attr_name)) {
            $all_in_persistence = 0;
            $missing_count++;
          }
        }

        if ($all_in_persistence) {
          $job->logger->info(
            'All ascending attributes verified in persistence');
          $verified = 1;
          last;
        }

        $job->logger->debug(sprintf(
'Persistence verification attempt %d/%d: %d ascending attributes still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg =
'Failed to verify all ascending attributes in persistence after child jobs finished';
        $job->logger->error($errmsg);
        return $job->fail($errmsg);
      }
    }
    # If no jobs were enqueued (data already in persistence), we still succeeded
    elsif ($skipped_count > 0) {
      $job->logger->info(sprintf(
        'All data already in persistence - no jobs needed (skipped %d)',
        $skipped_count));
    }

    # Mark this job as completed in persistence (with run_id for isolation)
    # IMPORTANT: This must be OUTSIDE the if (@job_ids) block so jobs that
    # skip all work (because data exists) still mark themselves complete
    my $run_id = $job->info->{notes}->{prebuild_run_id};
    $job->mark_task_completed($job->task_name, $run_id);

    my $msg = 'load_all_ascending_attributes job completed';
    $job->logger->info($msg);
    $job->finish($msg);
  }
}

1;

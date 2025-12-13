use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  require Game::EvonyTKR::External::General::BuildIndexes;
  use Mojo::File;

  sub task_name {'load_all_generals'}

  sub register ($taskClass, $app, $conf = {}) {
    return 1 unless $taskClass->SUPER::register($app, $conf);
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
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_builtin_books', 'load_all_specialties',
        'load_all_ascending_attributes',
      ]
      ));

    $job->log_info('Starting LoadAll generals job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalDir = $collectionDir->child('generals');

    my @files =
      $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;

    $job->log_info(sprintf('Found %d general files to process', scalar @files));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    foreach my $file (@files) {
      # Extract general name from filename
      my $general_name = $file->basename('.yaml', '.yml');

      # Check if already in persistence
      if ($job->get_general($general_name)) {
        $job->log_debug(sprintf(
          'Skipping %s - already in persistence', $general_name));
        $skipped_count++;
        next;
      }

      # Check if job already exists for this file in current run
      my $normalized_path = $job->normalize($file->to_string);
      my $existing_jobs = $job->minion->jobs({
        tasks => ['load_general'],
        states => ['active', 'inactive']
      });

      my $job_exists = 0;
      while (my $existing = $existing_jobs->next) {
        if ($existing->{args} && $existing->{args}[0] && $existing->{args}[0] eq $normalized_path &&
            $existing->{notes} && $existing->{notes}->{prebuild_run_id} &&
            $existing->{notes}->{prebuild_run_id} eq $job->info->{notes}->{prebuild_run_id}) {
          $job_exists = 1;
          last;
        }
      }

      if ($job_exists) {
        $job->log_debug(sprintf(
          'Skipping %s - job already exists for current run', $general_name));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        'load_general' => [$job->normalize($file->to_string)] => {
          attempts => 3,
          delay    => rand(10),
          priority => 20,
          notes    => { prebuild_run_id => $job->info->{notes}->{prebuild_run_id} }
        }
      );
      $job->log_debug(sprintf(
        'Enqueued load_general job %s for file %s',
        $job_id, $file->basename
      ));
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->log_info(sprintf(
      'Enqueued %d load_general jobs, skipped %d already in persistence',
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
          $job->log_debug("Job $jid: no info or state");
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

      $job->log_debug(sprintf(
        'Job status: active=%d, finished=%d, failed=%d',
        $active, $finished, $failed
      ));

      # If jobs still running, retry this coordinator job to check again later
      if ($active > 0) {
        $job->log_info(sprintf(
          'Still waiting for %d child jobs - retrying in 5 seconds',
          $active));
        return $job->retry({ delay => $job->standard_delay });
      }

      $job->log_info(sprintf(
        'All child jobs completed: %d finished, %d failed',
        $finished, $failed
      ));

      # Verify all data is actually in persistence before marking complete
      # This ensures database transactions have committed
      my $verified            = 0;
      my $max_verify_attempts = 10;

      for my $attempt (1 .. $max_verify_attempts) {
        my $all_in_persistence = 1;
        my $missing_count      = 0;
        my @missing_generals   = ();

        foreach my $file (@files) {
          my $general_name = $file->basename('.yaml', '.yml');
          unless ($job->get_general($general_name)) {
            $all_in_persistence = 0;
            $missing_count++;
            push @missing_generals, $general_name;
          }
        }

        if ($all_in_persistence) {
          $job->log_info('All generals verified in persistence');
          $verified = 1;
          last;
        }

        # On final attempt or if DEBUG, show which generals are missing
        if ($attempt == $max_verify_attempts || $job->is_debug()) {
          $job->log_warn(sprintf(
            'Persistence verification attempt %d/%d: %d generals missing: %s',
            $attempt,       $max_verify_attempts,
            $missing_count, join(', ', @missing_generals)
          ));
        }
        else {
          $job->log_debug(sprintf(
            'Persistence verification attempt %d/%d: %d generals still missing',
            $attempt, $max_verify_attempts, $missing_count
          ));
        }
        sleep 1;
      }

      unless ($verified) {
        my $errmsg =
'Failed to verify all generals in persistence after child jobs finished';
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }
    }

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    $job->minion->enqueue(
      build_general_indexes => [] => {
        attempts => 3,
        priority => 30,
        notes    => { prebuild_run_id => $job->info->{notes}->{prebuild_run_id} }
      }
    );
    $job->note(generalCount => scalar(@files));
    my $message = 'LoadAll generals job completed';
    $job->log_info($message);
    return $job->finish($message);
  }
}

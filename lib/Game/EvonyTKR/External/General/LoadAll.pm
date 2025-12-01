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
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
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
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_builtin_books',
        'load_all_specialties',
        'load_all_ascending_attributes',
      ]
      ));

    $job->logger->info('Starting LoadAll generals job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalDir = $collectionDir->child('generals');

    my @files =
      $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;

    $job->logger->info(
      sprintf('Found %d general files to process', scalar @files));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    foreach my $file (@files) {
      # Extract general name from filename
      my $general_name = $file->basename('.yaml', '.yml');

      # Check if already in persistence
      if ($job->persistence->get_general($general_name)) {
        $job->logger->debug(sprintf(
          'Skipping %s - already in persistence', $general_name));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        'load_general' => [$job->normalize($file->to_string)] => {
          attempts => 3,
          delay    => rand(10),
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_general job %s for file %s',
        $job_id, $file->basename
      ));
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->logger->info(sprintf(
      'Enqueued %d load_general jobs, skipped %d already in persistence',
      $enqueued_count, $skipped_count
    ));

    # Wait for all child jobs to complete
    if (@job_ids) {
      $job->logger->info(
        sprintf('Waiting for %d child jobs to complete', scalar @job_ids));

      my $finished = 0;
      my $failed   = 0;

      while (1) {
        my $all_done = 1;
        $finished = 0;
        $failed   = 0;

        for my $jid (@job_ids) {
          my $info = $job->minion->job($jid);
          next unless ($info && $info->{state});

          if ($info->{state} eq 'finished') {
            $finished++;
          }
          elsif ($info->{state} eq 'failed') {
            $failed++;
          }
          else {
            $all_done = 0;
          }
        }

        last if $all_done;
        sleep 2;
      }

      $job->logger->info(sprintf(
        'Child jobs completed: %d finished, %d failed',
        $finished, $failed
      ));

      # Verify all data is actually in persistence before marking complete
      # This ensures database transactions have committed
      my $verified = 0;
      my $max_verify_attempts = 10;

      for my $attempt (1 .. $max_verify_attempts) {
        my $all_in_persistence = 1;
        my $missing_count = 0;

        foreach my $file (@files) {
          my $general_name = $file->basename('.yaml', '.yml');
          unless ($job->persistence->get_general($general_name)) {
            $all_in_persistence = 0;
            $missing_count++;
          }
        }

        if ($all_in_persistence) {
          $job->logger->info('All generals verified in persistence');
          $verified = 1;
          last;
        }

        $job->logger->debug(sprintf(
          'Persistence verification attempt %d/%d: %d generals still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg = 'Failed to verify all generals in persistence after child jobs finished';
        $job->logger->error($errmsg);
        return $job->fail($errmsg);
      }
    }

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    $job->minion->enqueue(
      build_general_indexes => [] => {
        attempts => 3,
        priority => 30,
      }
    );
    $job->note(generalCount => scalar(@files));
    my $message = 'LoadAll generals job completed';
    $job->logger->info($message);
    return $job->finish($message);
  }
}

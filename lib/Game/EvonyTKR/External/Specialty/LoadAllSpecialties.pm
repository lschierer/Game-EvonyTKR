use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Specialty::LoadAllSpecialties {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::File;

  sub task_name {'load_all_specialties'}

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
    $job->logger->info('Starting LoadAllSpecialties job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $specialtyDir = $collectionDir->child('specialties');

    my @files =
      $specialtyDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;

    $job->logger->info(
      sprintf('Found %d specialty files to process', scalar @files));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    foreach my $file (@files) {
      # Extract specialty name from filename
      my $specialty_name = $file->basename('.yaml', '.yml');

      # Check if already in persistence
      if ($job->get_specialty($specialty_name)) {
        $job->logger->debug(sprintf(
          'Skipping %s - already in persistence', $specialty_name));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        'load_specialty' => [$file->to_string] => {
          attempts => 3,
          delay    => rand(10),
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_specialty job %s for file %s',
        $job_id, $file->basename
      ));
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->logger->info(sprintf(
      'Enqueued %d load_specialty jobs, skipped %d already in persistence',
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
      my $verified            = 0;
      my $max_verify_attempts = 10;

      for my $attempt (1 .. $max_verify_attempts) {
        my $all_in_persistence = 1;
        my $missing_count      = 0;

        foreach my $file (@files) {
          my $specialty_name = $file->basename('.yaml', '.yml');
          unless ($job->get_specialty($specialty_name)) {
            $all_in_persistence = 0;
            $missing_count++;
          }
        }

        if ($all_in_persistence) {
          $job->logger->info('All specialties verified in persistence');
          $verified = 1;
          last;
        }

        $job->logger->debug(sprintf(
'Persistence verification attempt %d/%d: %d specialties still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg =
'Failed to verify all specialties in persistence after child jobs finished';
        $job->logger->error($errmsg);
        return $job->fail($errmsg);
      }
    }

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    $job->logger->info('LoadAllSpecialties job completed');
  }
}

1;

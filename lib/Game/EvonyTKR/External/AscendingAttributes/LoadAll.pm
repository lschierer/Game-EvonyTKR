use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::AscendingAttributes::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::File;

  sub task_name {'load_all_ascending_attributes'}

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
    $job->log_info('Starting load_all_ascending_attributes job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $ascendingAttributesDir = $collectionDir->child('ascending attributes');

    my @files =
      $ascendingAttributesDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->each;

    $job->log_info(
      sprintf('Found %d ascendingAttributes files to process', scalar @files));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    foreach my $file (@files) {
      # Extract ascending attribute name from filename
      my $attr_name = $file->basename('.yaml', '.yml');

      # Check if already in persistence
      if ($job->get_ascending_attribute($attr_name)) {
        $job->log_debug(sprintf(
          'Skipping %s - already in persistence', $attr_name));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        'load_ascending_attributes' => [$file->to_string] => {
          attempts => 3,
          delay    => rand(10),
          priority => 20,
        }
      );
      $job->log_debug(sprintf(
        'Enqueued load_ascending_attributes job %s for file %s',
        $job_id, $file->basename
      ));
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->log_info(sprintf(
'Enqueued %d load_ascending_attributes jobs, skipped %d already in persistence',
      $enqueued_count, $skipped_count
    ));

    # Wait for all child jobs to complete
    if (@job_ids) {
      $job->log_info(
        sprintf('Waiting for %d child jobs to complete', scalar @job_ids));

      # Debug: Log Minion backend info
      $job->log_debug(sprintf(
        'Minion backend: %s, SQLite DB: %s',
        ref($job->minion->backend),
        eval { $job->minion->backend->sqlite->db->dbh->sqlite_db_filename } // 'N/A'
      ));

      my $finished = 0;
      my $failed   = 0;

      while (1) {
        my $all_done = 1;
        $finished = 0;
        $failed   = 0;
        my $skipped = 0;

        for my $jid (@job_ids) {
          my $info = $job->minion->job($jid);

          unless ($info && $info->{state}) {
            $skipped++;
            $job->log_debug(sprintf(
              'Job %s: no info or state (info=%s)',
              $jid,
              defined($info) ? 'defined but no state' : 'undef'
            ));
            next;
          }

          $job->log_debug(sprintf('Job %s: state=%s', $jid, $info->{state}));

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

        $job->log_debug(sprintf(
          'Job status check: finished=%d, failed=%d, skipped=%d, all_done=%s',
          $finished, $failed, $skipped, $all_done ? 'YES' : 'NO'
        ));

        last if $all_done;
        sleep 2;
      }

      $job->log_info(sprintf(
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
          my $attr_name = $file->basename('.yaml', '.yml');
          # Query Redis directly to bypass state cache
          unless ($job->get_ascending_attribute($attr_name)) {
            $all_in_persistence = 0;
            $missing_count++;
          }
        }

        if ($all_in_persistence) {
          $job->log_info('All ascending attributes verified in persistence');
          $verified = 1;
          last;
        }

        $job->log_debug(sprintf(
'Persistence verification attempt %d/%d: %d ascending attributes still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg =
'Failed to verify all ascending attributes in persistence after child jobs finished';
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }
    }

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    my $msg = 'load_all_ascending_attributes job completed';
    $job->log_info($msg);
    $job->finish($msg);
  }
}

1;

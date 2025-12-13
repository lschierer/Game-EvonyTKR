use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

package Game::EvonyTKR::External::Book::LoadAllBuiltins {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use List::AllUtils qw( any none uniq all );
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  state $bookCache;

  sub task_name {'load_all_builtin_books'}

  sub register ($taskClass, $app, $conf = {}) {
    return 1 unless $taskClass->SUPER::register($app, $conf);
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $taskClass->log_error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $taskClass->log_error($errmessage);
      say $errmessage;
      return;
    }
    $taskClass->log_debug('Registering Book Loader workflow tasks');
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);

    return 1;
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    my $parent_notes = $job->info->{notes} || {};
    $job->prebuild_run_id($parent_notes->{prebuild_run_id} || '');
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
    my @list;
    push @list, $job->list_builtin_books()->@*;
    $job->log_info(sprintf('Found %d builtin books to process', scalar @list));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    my $maxIndex = scalar(@list) - 1;
    foreach my $index (0 .. $maxIndex) {
      my $entry = $list[$index];

      # Check if already in persistence
      if ($job->get_builtin_book($entry)) {
        $job->log_debug(sprintf(
          'Skipping %s - already in persistence', $entry));
        $skipped_count++;
        next;
      }

      # Check if job already exists for this book in current run
      my $existing_jobs = $job->minion->jobs({
        tasks => ['load_book'],
        states => ['active', 'inactive']
      });

      my $job_exists = 0;
      while (my $existing = $existing_jobs->next) {
        if ($existing->{args} && $existing->{args}[0] && $existing->{args}[0] eq $entry &&
            $existing->{notes} && $existing->{notes}->{prebuild_run_id} &&
            $existing->{notes}->{prebuild_run_id} eq $job->prebuild_run_id) {
          $job_exists = 1;
          last;
        }
      }

      if ($job_exists) {
        $job->log_debug(sprintf(
          'Skipping %s - job already exists for current run', $entry));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        load_book => [
          $entry,
          {
            index      => $index,
            is_generic => 0,
            is_builtin => 1,
            filebase   => $entry,
            suffixlist => ['.yaml', '.yml'],
          }
        ] => {
          attempts => 3,
          delay    => rand(10),
          notes    => { prebuild_run_id => $job->prebuild_run_id },
          priority => 22,
        }
      );
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->log_info(sprintf(
      'Enqueued %d load_book jobs, skipped %d already in persistence',
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

        foreach my $entry (@list) {
          unless ($job->get_builtin_book($entry)) {
            $all_in_persistence = 0;
            $missing_count++;
          }
        }

        if ($all_in_persistence) {
          $job->log_info('All builtin books verified in persistence');
          $verified = 1;
          last;
        }

        $job->log_debug(sprintf(
'Persistence verification attempt %d/%d: %d builtin books still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg =
'Failed to verify all builtin books in persistence after child jobs finished';
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }
    }

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);
  }
}
1;
__END__

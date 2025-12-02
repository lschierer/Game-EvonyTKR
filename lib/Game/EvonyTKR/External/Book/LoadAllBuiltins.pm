use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

require Game::EvonyTKR::External::Common;

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
    $taskClass->SUPER::register($app, $conf);
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $taskClass->logger->error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $taskClass->logger->error($errmessage);
      say $errmessage;
      return;
    }
    $taskClass->logger->debug('Registering Book Loader workflow tasks');
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);

    $taskClass->logger->info(sprintf('emitting signal for %s', __PACKAGE__));
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
    my @list;
    push @list, $job->list_builtin_books()->@*;
    $job->logger->info(
      sprintf('Found %d builtin books to process', scalar @list));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    my $maxIndex = scalar(@list) - 1;
    foreach my $index (0 .. $maxIndex) {
      my $entry = $list[$index];

      # Check if already in persistence
      if ($job->get_builtin_book($entry)) {
        $job->logger->debug(sprintf(
          'Skipping %s - already in persistence', $entry));
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
          priority => 20,
        }
      );
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->logger->info(sprintf(
      'Enqueued %d load_book jobs, skipped %d already in persistence',
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
          $job->logger->info('All builtin books verified in persistence');
          $verified = 1;
          last;
        }

        $job->logger->debug(sprintf(
'Persistence verification attempt %d/%d: %d builtin books still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg =
'Failed to verify all builtin books in persistence after child jobs finished';
        $job->logger->error($errmsg);
        return $job->fail($errmsg);
      }
    }

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);
  }
}
1;
__END__

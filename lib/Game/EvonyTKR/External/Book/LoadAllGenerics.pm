use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

package Game::EvonyTKR::External::Book::LoadAllGenerics {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  sub task_name {'load_all_generic_books'}

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

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my $in_progress = $job->info->{notes}->{in_progress} // {};

    foreach my $level (1 .. 4) {
      my $ll = $job->list_generic_books($level);
      $job->log_info(sprintf(
        'Found %d generic books to process at level %s',
        scalar @$ll, $level
      ));

      my $maxIndex = scalar(@$ll) - 1;
      foreach my $index (0 .. $maxIndex) {
        my $entry = $ll->[$index];

        if ($job->get_generic_book($entry, $level)) {
          $job->log_debug(sprintf(
            'Skipping %s level %d - already in persistence',
            $entry, $level
          ));
          $skipped_count++;
          next;
        }
        if(exists $in_progress->{$entry}->{$level}){
          my $bj = $job->minion->job($in_progress->{$entry}->{$level});
          if($bj) {
            if($bj->info->{notes}->{prebuild_run_id} eq $job->prebuild_run_id){
              if($bj->info->{state} eq 'finished'){
                $job->log_debug(sprintf(
                  'Skipping %s level %d - already in progress',
                  $entry, $level
                ));
                $skipped_count++;
                next;
              }elsif($bj->info->{state} eq 'failed'){
                $job->log_debug(sprintf(
                  'Skipping %s level %d - already in progress',
                  $entry, $level
                ));
                $skipped_count++;
                next;
              }elsif($bj->info->{state} eq 'active'){
                $job->log_debug(sprintf(
                  'Skipping %s level %d - already in progress',
                  $entry, $level
                ));
                $skipped_count++;
                next;
              }elsif($bj->info->{state} eq 'inactive'){
                $job->log_debug(sprintf(
                  'Skipping %s level %d - already in progress',
                  $entry, $level
                ));
                $skipped_count++;
                next;
              }
              # else it is a ghost job and we should ignore it.
            } else {
              $bj->remove();
            }
          }
          # else it is a ghost job and we should ignore it.
        }

        # Check if job already exists for this book in current run
        my $book_name     = sprintf('Level %s %s', $level, $entry);

        my $job_id = $job->minion->enqueue(
          load_book => [
            sprintf('Level %s %s', $level, $entry),
            {
              index      => $index,
              is_generic => 1,
              is_builtin => 0,
              filebase   => $entry,
              suffixlist => ['.yaml', '.yml'],
            }
          ] => {
            attempts => 3,
            delay    => rand(10),
            priority => 10,
            notes    => { prebuild_run_id => $job->prebuild_run_id },
          }
        );
        $in_progress->{$entry}->{$level} = $job_id;
        $enqueued_count++;
      }
    }
    $job->note(in_progress => $in_progress);

    $job->log_info(sprintf(
      'Enqueued %d generic load_book jobs, skipped %d already in persistence',
      $enqueued_count, $skipped_count
    ));


      # Verify all data is actually in persistence before marking complete
      # This ensures database transactions have committed
      my $verified            = 0;
      my $max_verify_attempts = 10;

      for my $attempt (1 .. $max_verify_attempts) {
        my $all_in_persistence = 1;
        my $missing_count      = 0;

        foreach my $level (1 .. 4) {
          my @list = $job->list_generic_books($level)->@*;
          foreach my $entry (@list) {
            unless ($job->get_generic_book($entry, $level)) {
              $job->log_warn(sprintf('attempt %s failed to find "Level %s %s" in persistence.',
                $attempt, $level, $entry));
              $all_in_persistence = 0;
              $missing_count++;
            }
          }
        }

        if ($all_in_persistence) {
          $job->log_info('All generic books verified in persistence');
          $verified = 1;
          last;
        }

        $job->log_debug(sprintf(
'Persistence verification attempt %d/%d: %d generic books still missing',
          $attempt, $max_verify_attempts, $missing_count
        ));
        sleep 1;
      }

      unless ($verified) {
        my $errmsg = 'Failed to verify all generic books '.
          'in persistence after child jobs finished';
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }
      # If no jobs were enqueued (data already in persistence), we still succeeded
      if ($skipped_count > 0) {
        $job->log_info(sprintf(
          'All data already in persistence - no jobs needed (skipped %d)',
          $skipped_count));
      }

      # Mark this job as completed in persistence (with run_id for isolation)
      # IMPORTANT: This must be OUTSIDE the if (@job_ids) block so jobs that
      # skip all work (because data exists) still mark themselves complete
      my $run_id = $job->info->{notes}->{prebuild_run_id};
      $job->mark_task_completed($job->task_name, $run_id);
    }

}
1;
__END__

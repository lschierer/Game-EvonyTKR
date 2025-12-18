
package Game::EvonyTKR::External::General::MonitorBuffCache;
use v5.42.0;
use utf8::all;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;

sub task_name {'monitor_general_buff_cache'}

sub register ($taskClass, $app, $conf = {}) {
  $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($job) {
  $job->SUPER::run();

  # Wait for generals and covenants to be loaded first
  return
    if $job->are_prereqs_outstanding($job->minion,
    ['load_all_generals', 'load_all_covenants']);

  $job->log_info("Monitoring buff cache job completion");

  my $run_id = $job->info->{notes}->{prebuild_run_id};

  unless (defined $run_id) {
    $job->log_error("No prebuild_run_id found in job notes");
    return $job->fail("Missing prebuild_run_id");
  }

  # Check for any active or inactive buff cache jobs from this run
  # Note: SQLite backend doesn't support filtering by notes in query,
  # so we query all jobs and filter in Perl
  my $all_pending = $job->minion->jobs({
    tasks  => ['compute_general_buff_cache'],
    states => ['inactive', 'active']
  });

  my $pending_count = 0;
  while (my $j = $all_pending->next) {
    if ( $j->{notes}
      && $j->{notes}->{prebuild_run_id}
      && $j->{notes}->{prebuild_run_id} eq $run_id) {
      $pending_count++;
    }
  }

  if ($pending_count > 0) {
    $job->log_debug(
      "Still waiting for $pending_count buff cache jobs to complete");
    return $job->retry({ delay => 10 });
  }

  # Check for any failed jobs
  my $all_failed = $job->minion->jobs({
    tasks  => ['compute_general_buff_cache'],
    states => ['failed']
  });

  my $failed_count = 0;
  while (my $j = $all_failed->next) {
    if ( $j->{notes}
      && $j->{notes}->{prebuild_run_id}
      && $j->{notes}->{prebuild_run_id} eq $run_id) {
      $failed_count++;
    }
  }

  if ($failed_count > 0) {
    my $errmsg = "Buff cache completion failed: $failed_count jobs failed";
    $job->log_error($errmsg);
    return $job->fail($errmsg);
  }

  # All buff cache jobs completed successfully
  require Game::EvonyTKR::WorkUnit::Tracker;
  my $tracker =
    Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $job->persistence);
  $tracker->mark_complete('general_buff_cache');

  $job->log_info("All general buff cache jobs completed successfully");
  return $job->finish("General buff cache monitoring complete");
}

1;
__END__

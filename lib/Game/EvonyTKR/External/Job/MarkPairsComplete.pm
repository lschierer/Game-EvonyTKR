package Game::EvonyTKR::External::Job::MarkPairsComplete;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use Game::EvonyTKR::WorkUnit::Tracker;

sub task_name {'mark_pairs_complete'}

sub register ($taskClass, $app, $conf = {}) {
  $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($job) {
  my $app = $job->app;

  # Wait for reduce_coordinator to complete
  my $jobs_needed =
    ['reduce_coordinator', 'reduce_batch', 'load_all_pair_builders'];
  my $job_pending = $app->minion->jobs({
    tasks  => $jobs_needed,
    states => ['inactive', 'active']
  })->total;

  if ($job_pending > 0) {
    $job->note(waiting_for => $jobs_needed);
    return $job->retry({ delay => 10 });
  }
  else {
    $app->minion->jobs({
      tasks  => ['reduce_coordinator', 'load_all_pair_builders'],
      states => ['finished',           'failed']
    })->each(sub {
      my $info = $_;
      next
        unless ($info->{notes}->{prebuild_run_id} eq
        $job->info->{notes}->{prebuild_run_id});
      $job->note($info->{task} => $info->{id});
      $job->note(waiting_for   => 'nothing');
    });
  }

  # Coordinator complete - mark pairs work unit as complete
  my $tracker =
    Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $job->persistence);
  $tracker->mark_complete('load_all_pair_builders');

  $job->app->log->info("Marked work unit 'load_all_pair_builders' as complete");
}

1;
__END__

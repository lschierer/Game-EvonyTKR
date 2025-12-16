package Game::EvonyTKR::External::Job::MarkPairsComplete;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use Game::EvonyTKR::WorkUnit::Tracker;

sub task_name {'mark_pairs_complete'}

sub register ($taskClass, $app, $conf = {}) {
  return 1 unless $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($self) {
  my $app = $self->app;

  # Wait for reduce_coordinator to complete
  my $coordinator_pending = $app->minion->jobs({
    tasks  => ['reduce_coordinator'],
    states => ['inactive', 'active']
  })->total;

  if ($coordinator_pending > 0) {
    $self->note(waiting_for => ['reduce_coordinator']);
    return $self->retry({ delay => 10 });
  }

  # Coordinator complete - mark pairs work unit as complete
  my $tracker = Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $self->persistence);
  $tracker->mark_complete('pairs');

  $self->app->log->info("Marked work unit 'pairs' as complete");
}

1;

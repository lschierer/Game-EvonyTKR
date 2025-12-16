package Game::EvonyTKR::External::Job::MarkCollectionComplete;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use Game::EvonyTKR::WorkUnit::Tracker;

sub task_name {'mark_collection_complete'}

sub register ($taskClass, $app, $conf = {}) {
  return 1 unless $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($self, $collection_type, $loader_task_names) {
  my $app = $self->app;

  # Wait for all loader jobs to complete
  my @outstanding = ();
  for my $task_name (@$loader_task_names) {
    my $pending_count = $app->minion->jobs({
      tasks  => [$task_name],
      states => ['inactive', 'active']
    })->total;

    if ($pending_count > 0) {
      push @outstanding, $task_name;
    }
  }

  if (@outstanding) {
    $self->note(waiting_for => \@outstanding);
    return $self->retry({ delay => 5 });
  }

  # All loaders complete - mark work unit as complete
  my $tracker = Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $self->persistence);
  $tracker->mark_complete($collection_type);

  $self->app->log->info("Marked work unit '$collection_type' as complete");
}

1;

package Game::EvonyTKR::External::Job::MarkCollectionComplete;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use Game::EvonyTKR::WorkUnit::Tracker;

sub task_name {'mark_collection_complete'}

sub register ($taskClass, $app, $conf = {}) {
  $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($self, $unit_name) {
  my $app = $self->app;

  # Wait for all loader jobs to complete
  my @outstanding = ();
  my $collection  = $unit_name;
  $collection =~ s/load_all_//;

  # special cases
  $collection = 'book' if ($collection =~ /book/i);
  $collection =~ s/s$// unless ($collection eq 'ascending_attributes');

  # build the individual task name from the collection
  my $task_name = "load_${collection}";

  my $pending_count = $app->minion->jobs({
    tasks  => [$unit_name, $task_name],
    states => ['inactive', 'active']
  })->total;

  if ($pending_count > 0) {
    push @outstanding, $task_name;
  }

  if (@outstanding) {
    $self->note(waiting_for => \@outstanding);
    return $self->retry({ delay => 5 });
  }

  # All loaders complete - mark work unit as complete
  my $tracker =
    Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $self->persistence);
  $tracker->mark_complete($unit_name);

  $self->app->log->info("Marked work unit '$unit_name' as complete");
}

1;

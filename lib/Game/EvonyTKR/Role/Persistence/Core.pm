package Game::EvonyTKR::Role::Persistence::Core;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use Game::EvonyTKR::Service::Persistence;
use DBI;

# Lazy-load mode-gated persistence service
# Use package-level singleton to share backend across all instances
our $persistence;

sub persistence ($self) {
  $persistence //= do {
    my $mode   = $ENV{MOJO_MODE} || 'development';
    my $config = {};

    # Get config from app (controllers/jobs have $self->app)
    if ($self->can('app') && defined($self->app)) {
      $config = $self->app->config || {};
    }

    # Debug: Log when singleton is lazily initialized (should be rare!)
    my ($package, $filename, $line) = caller(1);
    warn sprintf(
"[Persistence::Core] LAZY initialization from %s:%d (should be initialized by Game::EvonyTKR::_init_persistence instead!)\n",
      $package, $line);
    warn sprintf("[Persistence::Core] Config keys at lazy init: %s\n",
      join(', ', sort keys %$config));

    Game::EvonyTKR::Service::Persistence->new(mode => $mode, config => $config);
  };
}

##############################################################################
# Convenience methods for job tracking
##############################################################################

sub mark_task_completed ($self, $task_name, $run_id = undef) {
  $self->log_info(sprintf(
"[Persistence::Core] mark_task_completed called: task=%s, run_id=%s, backend=%s",
    $task_name,
    $run_id // 'none',
    ref($self->persistence->backend)
  ));
  my $result = $self->persistence->mark_job_completed($task_name, $run_id);
  $self->log_info(sprintf("[Persistence::Core] mark_job_completed returned: %s",
    $result ? 'success' : 'failure'));
  return $result;
}

sub is_task_completed ($self, $task_name, $run_id = undef) {
  return $self->persistence->is_job_completed($task_name, $run_id);
}

sub harvest_job_completions ($self, $run_id) {
  return $self->persistence->harvest_job_completions($run_id);
}

sub get_current_prebuild_run_id ($self) {
  my $metadata = $self->persistence->get_metadata('current_prebuild_run_id');
  return $metadata ? $metadata->{run_id} : undef;
}

sub set_metadata ($self, $key, $value) {
  $self->log_info(sprintf(
    "[Persistence::Core] set_metadata called: key=%s, backend=%s",
    $key, ref($self->persistence->backend)
  ));
  my $result = $self->persistence->set_metadata($key, $value);
  $self->log_info(sprintf("[Persistence::Core] set_metadata returned: %s",
    $result ? 'success' : 'failure'));
  return $result;
}

sub get_metadata ($self, $key) {
  return $self->persistence->get_metadata($key);
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Core - Core persistence infrastructure

=head1 DESCRIPTION

Provides the persistence service attribute and job tracking methods.
All other Persistence::* roles compose this role.

=cut

package Game::EvonyTKR::Role::Persistence::Core;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use Game::EvonyTKR::Service::Persistence;

# Lazy-load mode-gated persistence service
has persistence => sub ($self) {
  my $mode = ref($self) && $self->can('mode') ? $self->mode : ($ENV{MOJO_MODE} || 'development');
  return Game::EvonyTKR::Service::Persistence->new(mode => $mode);
};

##############################################################################
# Convenience methods for job tracking
##############################################################################

sub mark_task_completed ($self, $task_name, $notes = undef) {
  return $self->persistence->mark_job_completed($task_name, $notes);
}

sub is_task_completed ($self, $task_name) {
  return $self->persistence->is_job_completed($task_name);
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Core - Core persistence infrastructure

=head1 DESCRIPTION

Provides the persistence service attribute and job tracking methods.
All other Persistence::* roles compose this role.

=cut

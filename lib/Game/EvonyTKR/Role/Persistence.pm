package Game::EvonyTKR::Role::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;

# Compose all granular persistence roles
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Generals',            -role;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Books',               -role;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Covenants',           -role;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Specialties',         -role;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::AscendingAttributes', -role;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Pairs',               -role;

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence - Convenience role composing all persistence operations

=head1 SYNOPSIS

  package Game::EvonyTKR::Controller::Generals;
  use Mojo::Base 'Mojolicious::Controller', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;

  sub list ($self) {
    my $generals = $self->list_generals();
    $self->render(json => $generals);
  }

=head1 DESCRIPTION

This role composes all granular persistence roles for convenience.
Most controllers and jobs should use this role to get access to all
persistence operations.

For specialized use cases that only need specific collections, you can
compose individual roles instead:

  use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Books',    -role;

=head1 COMPOSED ROLES

=over 4

=item * Game::EvonyTKR::Role::Persistence::Core

Provides persistence service and job tracking methods.

=item * Game::EvonyTKR::Role::Persistence::Generals

General CRUD operations.

=item * Game::EvonyTKR::Role::Persistence::Books

Builtin and generic book operations.

=item * Game::EvonyTKR::Role::Persistence::Covenants

Covenant operations.

=item * Game::EvonyTKR::Role::Persistence::Specialties

Specialty operations.

=item * Game::EvonyTKR::Role::Persistence::AscendingAttributes

Ascending attribute operations.

=item * Game::EvonyTKR::Role::Persistence::Pairs

Pair and conflict detection operations.

=back

=cut

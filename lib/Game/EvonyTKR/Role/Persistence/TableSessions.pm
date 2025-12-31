package Game::EvonyTKR::Role::Persistence::TableSessions;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;

require Game::EvonyTKR::Role::Persistence::Core;

=head1 NAME

Game::EvonyTKR::Role::Persistence::TableSessions - Persist table session state for idempotency across workers

=head1 DESCRIPTION

This role provides session state persistence for table routes. Since hypnotoad workers don't
share memory, session state must be persisted in SQLite to ensure idempotent behavior when
different workers handle different requests in the same client session.

=head1 SYNOPSIS

  # Store session state
  $c->store_table_session($session_id, {
    generalType    => 'Ground Specialists',
    buffActivation => 'PvP',
    primaries      => ['Leonidas', 'Trajan'],  # or pairs list
    timestamp      => time(),
  });

  # Retrieve session state (idempotent across workers)
  my $session_data = $c->get_table_session($session_id);

  # Clean up old sessions
  $c->expire_table_sessions(3600);  # Expire after 1 hour

=cut

with 'Game::EvonyTKR::Role::Persistence::Core';

=head1 METHODS

=head2 init_table_sessions_db

No-op method for backwards compatibility. Table creation is handled by PostgreSQL migrations.

=cut

sub init_table_sessions_db ($self) {
  # Table is created automatically by PostgreSQL migrations
  # This method exists for backwards compatibility with code that calls it
  return 1;
}

=head2 store_table_session

Stores session state in PostgreSQL for idempotent access across workers.

  $c->store_table_session($session_id, {
    generalType    => 'Ground Specialists',
    buffActivation => 'PvP',
    items          => ['Leonidas', 'Trajan'],  # generals or pair keys
    ttl            => 3600,  # Optional, defaults to 1 hour
  });

Arguments:
  $session_id - UUID session identifier
  $data       - HashRef with generalType, buffActivation, items, optional ttl

Returns:
  Boolean - true on success

=cut

sub store_table_session ($self, $session_id, $data) {
  my $result = $self->persistence->store_table_session($session_id, $data);

  if ($result) {
    my $items = $data->{items} // [];
    my $ttl   = $data->{ttl}   // 3600;
    $self->log_debug(sprintf(
      'Stored table session %s (%s/%s) with %d items, expires in %d seconds',
      $session_id,     $data->{generalType}, $data->{buffActivation},
      scalar(@$items), $ttl
    ));
  }

  return $result;
}

=head2 get_table_session

Retrieves session state from PostgreSQL. Works across all hypnotoad workers.

  my $session_data = $c->get_table_session($session_id);

  # Returns:
  # {
  #   session_id      => 'uuid...',
  #   generalType     => 'Ground Specialists',
  #   buffActivation  => 'PvP',
  #   items           => ['Leonidas', 'Trajan'],
  #   created_at      => 1234567890,
  #   expires_at      => 1234571490,
  # }

Arguments:
  $session_id - UUID session identifier

Returns:
  HashRef - Session data, or undef if not found or expired

=cut

sub get_table_session ($self, $session_id) {
  return $self->persistence->get_table_session($session_id);
}

=head2 expire_table_sessions

Removes expired sessions from the database.

  $c->expire_table_sessions();         # Remove all expired
  $c->expire_table_sessions($max_age); # Remove sessions older than max_age seconds

Arguments:
  $max_age - Optional, expire sessions older than this (in seconds)

Returns:
  Integer - Number of sessions removed

=cut

sub expire_table_sessions ($self, $max_age = undef) {
  my $deleted = $self->persistence->expire_table_sessions($max_age);
  $self->log_debug("Expired $deleted table sessions") if $deleted > 0;
  return $deleted;
}

=head2 delete_table_session

Removes a specific session from the database.

  $c->delete_table_session($session_id);

Arguments:
  $session_id - UUID session identifier

Returns:
  Boolean - true if session was deleted

=cut

sub delete_table_session ($self, $session_id) {
  return $self->persistence->delete_table_session($session_id) > 0;
}

1;

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 SEE ALSO

L<Game::EvonyTKR::Role::Persistence::Core>

=cut

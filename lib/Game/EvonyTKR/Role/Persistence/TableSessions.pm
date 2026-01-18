package Game::EvonyTKR::Role::Persistence::TableSessions;
use v5.42.0;
use utf8::all;
use Moo::Role;

=head1 NAME

Game::EvonyTKR::Role::Persistence::TableSessions - In-memory table session state for Thunderhorse

=head1 DESCRIPTION

This role provides session state storage for table routes using in-memory hash storage.
Thunderhorse runs as a single process, so in-memory storage is sufficient for session
state that doesn't need to survive restarts.

=head1 SYNOPSIS

  # Store session state
  $c->store_table_session($session_id, {
    generalType    => 'Ground Specialists',
    buffActivation => 'PvP',
    items          => ['Leonidas', 'Trajan'],  # generals or pair keys
    timestamp      => time(),
  });

  # Retrieve session state
  my $session_data = $c->get_table_session($session_id);

  # Clean up old sessions
  $c->expire_table_sessions(3600);  # Expire after 1 hour

=cut

# In-memory session storage (package-level singleton)
our %SESSION_STORE;

=head1 METHODS

=head2 init_table_sessions_db

No-op method for backwards compatibility. In-memory storage needs no initialization.

=cut

sub init_table_sessions_db ($self) {
  # In-memory storage needs no initialization
  return 1;
}

=head2 store_table_session

Stores session state in memory.

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
  my $items      = $data->{items} // [];
  my $ttl        = $data->{ttl}   // 3600;
  my $now        = time();
  my $expires_at = $now + $ttl;

  $SESSION_STORE{$session_id} = {
    session_id     => $session_id,
    generalType    => $data->{generalType},
    buffActivation => $data->{buffActivation},
    items          => $items,
    created_at     => $now,
    expires_at     => $expires_at,
  };

  $self->logger->debug(sprintf(
    'Stored table session %s (%s/%s) with %d items, expires in %d seconds',
    $session_id,
    $data->{generalType}    // 'unknown',
    $data->{buffActivation} // 'unknown',
    scalar(@$items), $ttl
  ));

  return 1;
}

=head2 get_table_session

Retrieves session state from memory.

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
  $self->logger->debug(sprintf(
    'get_table_session: looking for session %s (store has %d sessions)',
    $session_id, scalar(keys %SESSION_STORE)
  ));

  # Log all session IDs in store for debugging
  if (scalar(keys %SESSION_STORE) > 0) {
    $self->logger->debug(
      'Sessions in store: ' . join(', ', keys %SESSION_STORE));
  }

  my $session = $SESSION_STORE{$session_id};
  unless ($session) {
    $self->logger->debug("Session $session_id NOT found in store");
    return undef;
  }

  # Check if expired
  if ($session->{expires_at} < time()) {
    $self->logger->debug("Session $session_id expired");
    delete $SESSION_STORE{$session_id};
    return undef;
  }

  $self->logger->debug("Session $session_id found and valid");
  return $session;
}

=head2 expire_table_sessions

Removes expired sessions from memory.

  $c->expire_table_sessions();         # Remove all expired
  $c->expire_table_sessions($max_age); # Remove sessions older than max_age seconds

Arguments:
  $max_age - Optional, expire sessions older than this (in seconds)

Returns:
  Integer - Number of sessions removed

=cut

sub expire_table_sessions ($self, $max_age = undef) {
  my $now     = time();
  my $deleted = 0;

  for my $session_id (keys %SESSION_STORE) {
    my $session    = $SESSION_STORE{$session_id};
    my $is_expired = $session->{expires_at} < $now;
    my $is_old     = defined($max_age)
      && ($now - $session->{created_at}) > $max_age;

    if ($is_expired || $is_old) {
      delete $SESSION_STORE{$session_id};
      $deleted++;
    }
  }

  $self->logger->debug("Expired $deleted table sessions") if $deleted > 0;
  return $deleted;
}

=head2 delete_table_session

Removes a specific session from memory.

  $c->delete_table_session($session_id);

Arguments:
  $session_id - UUID session identifier

Returns:
  Boolean - true if session was deleted

=cut

sub delete_table_session ($self, $session_id) {
  return delete $SESSION_STORE{$session_id} ? 1 : 0;
}

1;

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 NOTE

This is a Thunderhorse-specific implementation using in-memory storage.
Sessions do not persist across server restarts.

=cut

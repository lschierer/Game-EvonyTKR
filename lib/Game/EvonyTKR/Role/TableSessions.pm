package Game::EvonyTKR::Role::TableSessions;
use v5.42.0;
use utf8::all;
use Moo::Role;

# In-memory session storage (package-level singleton)
our %SESSION_STORE;

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

  return 1;
}

sub get_table_session ($self, $session_id) {
  my $session = $SESSION_STORE{$session_id};
  return undef unless $session;

  if ($session->{expires_at} < time()) {
    delete $SESSION_STORE{$session_id};
    return undef;
  }

  return $session;
}

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

  return $deleted;
}

sub delete_table_session ($self, $session_id) {
  return delete $SESSION_STORE{$session_id} ? 1 : 0;
}

1;

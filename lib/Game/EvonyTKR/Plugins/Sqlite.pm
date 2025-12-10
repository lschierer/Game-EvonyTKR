use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Plugins::Sqlite {
  use Mojo::Base 'Mojolicious::Plugin', -strict, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
  use POSIX 'strftime';
  use Time::HiRes 'time';
  use Carp;

  sub register ($self, $app, $config) {

    my $mh =
      Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
    my $dbPath = $mh->child('var/minion.db');

    $app->plugin(
      Minion => {
        SQLite =>
"sqlite:$dbPath?sqlite_use_immediate_transaction=1&busy_timeout=30000",
      }
    );
    my $db = $app->minion->backend->sqlite->db;
    ensure_lock_table_sqlite($db);

    $app->helper(
      ensure_lock_table_sqlite => sub {
        my $self = shift;
        return ensure_lock_table_sqlite(@_);
      }
    );

    $app->helper(
      refresh_lock_sqlite => sub {
        my $self = shift;
        return refresh_lock_sqlite(@_);
      }
    );

    $app->helper(
      try_acquire_lock_sqlite => sub {
        my $self = shift;
        return try_acquire_lock_sqlite(@_);
      }
    );

    $app->helper(
      release_lock_sqlite => sub {
        my $self = shift;
        return release_lock_sqlite(@_);
      }
    );
  }

  sub ensure_lock_table_sqlite ($db) {
    $db->query(q{
      CREATE TABLE IF NOT EXISTS app_locks (
        name        TEXT PRIMARY KEY,
        owner       TEXT NOT NULL,
        expires_at  INTEGER NOT NULL,
        updated_at  INTEGER NOT NULL
      )
    });
    $db->query(q{
      CREATE INDEX IF NOT EXISTS app_locks_expires_idx
      ON app_locks (expires_at)
    });
  }

  sub now_epoch () { int(time()) }

  sub refresh_lock_sqlite ($db, $name, $owner, $ttl_s) {
    my $now = now_epoch();
    my $exp = $now + $ttl_s;
    # Only the current owner may refresh
    my $res = $db->query(
      'UPDATE app_locks
          SET expires_at = ?, updated_at = ?
        WHERE name = ? AND owner = ?',
      $exp, $now, $name, $owner
    );
    return $res->rows > 0;
  }

  sub try_acquire_lock_sqlite ($db, $name, $owner, $ttl_s) {
    my $now = now_epoch();
    my $exp = $now + $ttl_s;

    # 1) Try to insert new lock
    my $res = eval {
      $db->query(
        'INSERT OR IGNORE INTO app_locks(name, owner, expires_at, updated_at)
         VALUES (?, ?, ?, ?)',
        $name, $owner, $exp, $now
      );
      1;
    };
    return 1 if $res && $db->dbh->rows;    # inserted → we own it

    # 2) If exists, try to take over only if stale
    $res = $db->query(
      'UPDATE app_locks
         SET owner = ?, expires_at = ?, updated_at = ?
       WHERE name = ? AND expires_at < ?',
      $owner, $exp, $now, $name, $now
    );
    return $res->rows > 0;                 # true if we stole a stale lock
  }

  sub release_lock_sqlite ($db, $name, $owner) {
    my $res = $db->query('DELETE FROM app_locks WHERE name = ? AND owner = ?',
      $name, $owner);
    return $res->rows > 0;
  }

}
1;
__END__

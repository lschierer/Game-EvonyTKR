package Game::EvonyTKR::Service::PostgreSQLPersistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base,                           -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::JSON',    -role;
with 'WebFramework::Role::Logger';
use Mojo::Pg;

use Carp;
use Time::HiRes 'time';

has 'config';    # Config hash from NotYAMLConfig

has 'dsn' => sub ($self) {
  my $config = $self->config || {};
  return
       $config->{postgresql_dsn}
    || $ENV{POSTGRESQL_DSN}
    || 'postgresql:///evonytkr_app_data';
};

has 'pg' => sub ($self) {
  my $dsn = $self->dsn;
  $self->logger->info("[PostgreSQL] Connecting with DSN: $dsn")
    if $self->can('logger->info');

  my $pg = Mojo::Pg->new($dsn);

# Configure connection pool - important for Minion job concurrency and web traffic
# Set to 40 per worker: 4 workers × 40 = 160 connections (under PostgreSQL's 200 limit)
# Leaves 40 connections for Minion workers and admin tools
# PostgreSQL configured via user-data.yaml for max_connections=200
  $pg->max_connections(50);

# Set inactivity timeout to 30 seconds - prevents stale connections from holding pool slots
  $pg->options->{inactivity_timeout} = 30;

  # Test connection before proceeding
  eval {
    my $test_db = $pg->db;
    my $result =
      $test_db->query('SELECT current_database(), current_user')->hash;
    if ($self->can('logger->info')) {
      $self->logger->info(sprintf(
        "[PostgreSQL] Connected to database '%s' as user '%s'",
        $result->{current_database},
        $result->{current_user}
      ));
    }
  };
  if ($@) {
    warn "[PostgreSQL] Connection test failed: $@";
    die "Failed to connect to PostgreSQL: $@";
  }

  # Create tables - run migrations with error handling
  eval { $pg->migrations->name('evonytkr')->from_data->migrate; };
  if ($@) {
    # If migration fails due to version conflict, it's likely already migrated
    warn "Migration warning (likely harmless): $@"
      if $@ !~ /greater than.*latest version/;
  }

  $self->logger->info("[PostgreSQL] Initialization complete")
    if $self->can('logger->info');
  return $pg;
};

# Get a database handle from the connection pool
# DO NOT cache - always get fresh connection from pool to avoid stale connections
sub db ($self) {
  return $self->pg->db;
}

has 'lifecycle_id' => sub ($self) {
  my $stored = $self->get_metadata('lifecycle_id');
  unless ($stored) {
    $stored = time . '_' . $$;
    $self->set_metadata('lifecycle_id', $stored);
  }
  return $stored;
};

# Metadata operations
sub get_metadata ($self, $key) {
  my $result = $self->db->select('metadata', ['value'], { key => $key })->hash;
  return $result ? $self->decode($result->{value}) : undef;
}

sub set_metadata ($self, $key, $value) {
  $self->db->insert(
    'metadata',
    {
      key        => $key,
      value      => $self->encode($value),
      updated_at => time()
    },
    {
      on_conflict => \[
        '(key) do update set value = ?, updated_at = ?',
        $self->encode($value), time()
      ]
    }
  );
}

sub get_current_prebuild_run_id ($self) {
  my $metadata = $self->get_metadata('current_prebuild_run_id');
  return $metadata ? $metadata->{run_id} : undef;
}

# Job completion tracking
sub mark_job_completed ($self, $job_name, $run_id = undef) {
  my $key = $run_id ? "${run_id}:${job_name}" : $job_name;
  $self->db->insert(
    'job_completed',
    {
      job_name     => $key,
      completed_at => time()
    },
    { on_conflict => \['(job_name) do update set completed_at = ?', time()] }
  );
}

sub is_job_completed ($self, $job_name, $run_id = undef) {
  my $key = $run_id ? "${run_id}:${job_name}" : $job_name;
  my $result =
    $self->db->select('job_completed', ['job_name'], { job_name => $key })
    ->hash;

  # If run-scoped lookup failed, try legacy key for backward compatibility
  if (!$result && $run_id) {
    $result = $self->db->select('job_completed', ['job_name'],
      { job_name => $job_name })->hash;
  }

  return $result ? 1 : 0;
}

# Harvest (clean up) job completion records from previous runs
sub harvest_job_completions ($self, $current_run_id) {
  unless ($current_run_id) {
    warn "[PostgreSQL] Cannot harvest without current_run_id";
    return 0;
  }

  # First count what we're about to delete
  my $count = $self->db->query(
    'SELECT COUNT(*) FROM job_completed WHERE job_name NOT LIKE $1',
    "${current_run_id}:%")->array->[0];

  # Delete all records that don't start with current_run_id
  # This includes legacy records (no run_id prefix) and old run_ids
  eval {
    $self->db->query('DELETE FROM job_completed WHERE job_name NOT LIKE $1',
      "${current_run_id}:%");
  };

  if ($@) {
    warn "[PostgreSQL] Failed to harvest job_completed records: $@\n";
    return 0;
  }

  warn sprintf("[PostgreSQL] Harvested %d stale job_completed records\n",
    $count || 0);
  return $count || 0;
}

# Data versioning - track which git-commit the data was built from
sub get_data_version ($self) {
  return $self->get_metadata('data_version');
}

sub set_data_version ($self, $version) {
  return $self->set_metadata('data_version', $version);
}

# Generic storage operations
sub store_data ($self, $table, $key, $data) {
  eval {
    $self->db->insert(
      $table,
      {
        name       => $key,
        data       => $self->encode($data),
        updated_at => time()
      },
      {
        on_conflict => \[
          '(name) do update set data = ?, updated_at = ?',
          $self->encode($data), time()
        ]
      }
    );
  };
  if ($@) {
    warn "Failed to store data in $table for key $key: $@";
    return 0;
  }
  return 1;
}

sub get_data ($self, $table, $key) {
  my $result =
    eval { $self->db->select($table, ['data'], { name => $key })->hash };
  if ($@) {
    warn "Failed to get data from $table for key $key: $@";
    return undef;
  }
  return unless $result && defined($result->{data});

  my $decoded = eval { $self->decode($result->{data}) };
  if ($@) {
    warn sprintf("[PostgreSQL] JSON decode failed for table=%s, key=%s: %s\n",
      $table, $key, $@);
    warn sprintf("[PostgreSQL] Raw data (first 200 chars): %s\n",
      substr($result->{data}, 0, 200));
    return;
  }

  return $decoded;
}

sub get_all_data ($self, $table) {
  my $results = $self->db->select($table, ['name', 'data'])->hashes;
  my $data    = {};
  for my $row (@$results) {
    $data->{ $row->{name} } = $self->decode($row->{data});
  }
  return $data;
}

# Specific data type methods (delegate to generic methods)
sub store_general ($self, $key, $general_data) {
  return $self->store_data('generals', $key, $general_data);
}

sub get_general ($self, $name) {
  return $self->get_data('generals', $name);
}

sub get_all_generals ($self) {
  return $self->get_all_data('generals');
}

sub count_generals ($self) {
  return $self->db->query('SELECT COUNT(*) FROM generals')->array->[0];
}

# Ascending Attributes
sub store_ascending_attribute ($self, $key, $data) {
  return $self->store_data('ascending_attributes', $key, $data);
}

sub get_ascending_attribute ($self, $name) {
  return $self->get_data('ascending_attributes', $name);
}

sub get_all_ascending_attributes ($self) {
  return $self->get_all_data('ascending_attributes');
}

sub count_ascending_attributes ($self) {
  return $self->db->query('SELECT COUNT(*) FROM ascending_attributes')
    ->array->[0];
}

# Books
sub store_book ($self, $key, $book_data) {
  my $name  = $book_data->{name} or croak "Book must have name";
  my $type  = $book_data->{type} || 'generic';
  my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
  return $self->store_data($table, $key, $book_data);
}

sub get_book ($self, $name, $type = 'generic') {
  my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
  return $self->get_data($table, $name);
}

sub get_all_books ($self, $type = 'generic') {
  my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
  return $self->get_all_data($table);
}

# Legacy book methods
sub get_generic_book ($self, $key) {
  $self->get_book($key, 'generic');
}

sub get_builtin_book ($self, $key) {
  $self->get_book($key, 'builtin');
}
sub get_all_generic_books ($self) { $self->get_all_books('generic') }
sub get_all_builtin_books ($self) { $self->get_all_books('builtin') }

sub store_generic_book ($self, $key, $data) {
  $data->{type} = 'generic';
  return $self->store_book($key, $data);
}

sub store_builtin_book ($self, $key, $data) {
  $data->{type} = 'builtin';
  return $self->store_book($key, $data);
}

sub count_generic_books ($self) {
  return $self->db->query('SELECT COUNT(*) FROM generic_books')->array->[0];
}

sub count_builtin_books ($self) {
  return $self->db->query('SELECT COUNT(*) FROM builtin_books')->array->[0];
}

# Specialties
sub store_specialty ($self, $key, $data) {
  return $self->store_data('specialties', $key, $data);
}

sub get_specialty ($self, $name) {
  my $result = $self->get_data('specialties', $name);
  return $result;
}
sub get_all_specialties ($self) { $self->get_all_data('specialties') }

sub count_specialties ($self) {
  $self->db->query('SELECT COUNT(*) FROM specialties')->array->[0];
}

# Covenants
sub store_covenant ($self, $key, $covenant_data) {
  return $self->store_data('covenants', $key, $covenant_data);
}

sub get_covenant      ($self, $name) { $self->get_data('covenants', $name) }
sub get_all_covenants ($self)        { $self->get_all_data('covenants') }

sub count_covenants ($self) {
  $self->db->query('SELECT COUNT(*) FROM covenants')->array->[0];
}

# Glossary terms
sub store_glossary_term ($self, $name, $data) {
  $self->store_data('glossary_terms', $name, $data);
}

sub get_glossary_term ($self, $name) {
  return $self->get_data('glossary_terms', $name);
}
sub get_all_glossary_terms ($self) { $self->get_all_data('glossary_terms') }

sub count_glossary_terms ($self) {
  $self->db->query('SELECT COUNT(*) FROM glossary_terms')->array->[0];
}

# Conflicts
sub store_conflict ($self, $g1, $g2, $conflicts) {
  ($g1, $g2) = sort ($g1, $g2);
  my $key = "$g1:$g2";
  $self->db->insert(
    'general_conflicts',
    {
      pair_key   => $key,
      conflicts  => $conflicts ? 1 : 0,
      updated_at => time()
    },
    {
      on_conflict => \[
        '(pair_key) do update set conflicts = ?, updated_at = ?',
        $conflicts ? 1 : 0, time()
      ]
    }
  );
  return 1;
}

# Batch write conflicts - much more efficient for bulk updates
sub store_conflicts_batch ($self, $conflicts_hash) {
  # Deduplicate first - conflicts are bidirectional so we might have
  # both conflicts{A}{B} and conflicts{B}{A} which map to same key
  my %unique_conflicts;

  foreach my $g1 (keys %$conflicts_hash) {
    foreach my $g2 (keys %{ $conflicts_hash->{$g1} }) {
      my ($sorted_g1, $sorted_g2) = sort ($g1, $g2);
      my $key = "$sorted_g1:$sorted_g2";

      # Skip if we've already processed this conflict pair
      next if exists $unique_conflicts{$key};

      $unique_conflicts{$key} = $conflicts_hash->{$g1}{$g2} ? 1 : 0;
    }
  }

  my $tx    = $self->db->begin;
  my $count = 0;

  eval {
    foreach my $key (keys %unique_conflicts) {
      my $conflicts = $unique_conflicts{$key};

      $self->db->insert(
        'general_conflicts',
        {
          pair_key   => $key,
          conflicts  => $conflicts,
          updated_at => time()
        },
        {
          on_conflict => \[
            '(pair_key) do update set conflicts = ?, updated_at = ?',
            $conflicts, time()
          ]
        }
      );
      $count++;
    }
    $tx->commit;
    1;
  } or do {
    my $error = $@ || 'unknown error';
    $self->logger->error(
      sprintf("[PostgreSQL] Batch conflict write failed: %s", $error));
    $tx->rollback;
    return 0;
  };

  $self->logger->info(
    sprintf("[PostgreSQL] Batch wrote %d conflict items", $count));
  return $count;
}

sub get_conflict ($self, $g1, $g2) {
  ($g1, $g2) = sort ($g1, $g2);
  my $key = "$g1:$g2";
  my $result =
    $self->db->select('general_conflicts', ['conflicts'], { pair_key => $key })
    ->hash;
  return unless $result;
  return $result->{conflicts} ? 1 : 0;
}

sub load_all_conflicts ($self) {
  my $results =
    $self->db->select('general_conflicts', ['pair_key', 'conflicts'])->hashes;
  my $conflicts = {};

  for my $row (@$results) {
    my ($g1, $g2) = split ':', $row->{pair_key};
    $conflicts->{$g1}{$g2} = $row->{conflicts} ? 1 : 0;
    $conflicts->{$g2}{$g1} = $row->{conflicts} ? 1 : 0;
  }

  return $conflicts;
}

# Pairs (simplified)
sub store_pairs ($self, $type, $pairs) {
  $self->store_data('pairs', $type, { pairs => $pairs });
}

sub get_pairs_by_type ($self, $type) {
  my $result = $self->get_data('pairs', $type);
  return $result ? $result->{pairs} : [];
}

sub store_pair ($self, $key, $data) {
  # Validate that data is a hash ref (wire_pair format)
  unless (ref($data) eq 'HASH') {
    warn sprintf(
"[PostgreSQL] Refusing to store invalid pair data for %s: expected HASH, got %s\n",
      $key, ref($data) || 'scalar');
    return 0;
  }
  $self->store_data('pairs_individual', $key, $data);
}
sub get_pair ($self, $key) { $self->get_data('pairs_individual', $key) }

sub get_all_pair_types ($self) {
  # Query pairs_individual to find all unique types
  # Name format is "type/primary/secondary"
  my $results = $self->db->select('pairs_individual', ['name'])->arrays;

  my %types_seen;
  for my $row (@$results) {
    my $name = $row->[0];
    if ($name =~ /^([^\/]+)\//) {
      $types_seen{$1} = 1;
    }
  }

  return [sort keys %types_seen];
}

sub list_pairs_by_type ($self, $type) {
  # Query pairs_individual table for all pairs of this type
  # The name field has format "type/primary/secondary"
  my $results = $self->db->query(
    'SELECT name, data FROM pairs_individual WHERE name LIKE $1', "$type/%")
    ->hashes;

  my @type_pairs;
  for my $row (@$results) {
    my $wire_pair = eval { $self->decode($row->{data}) };
    if ($@) {
      warn sprintf("[PostgreSQL] Failed to decode pair %s: %s\n",
        $row->{name}, $@);
      next;
    }

    # Skip invalid data (must be a hash ref, not array ref or other types)
    unless (ref($wire_pair) eq 'HASH') {
      warn sprintf(
        "[PostgreSQL] Invalid pair data for %s: expected HASH, got %s\n",
        $row->{name}, ref($wire_pair) || 'scalar');
      next;
    }

    push @type_pairs, $wire_pair;
  }

  return \@type_pairs;
}

# Batch version: returns inflated Pair objects directly (avoids N+1 queries)
# Options:
#   skip_generic_books => 1  # Skip expensive generic book precomputation (for diagnostics)
sub get_pairs_for_type_batch ($self, $type, $opts = {}) {
  require Game::EvonyTKR::Model::General::Pair;

  my $pairs = [];

  eval {
    my $type_pairs = $self->list_pairs_by_type($type);
    foreach my $wp (
      sort {
        my $key_a = sprintf('%s/%s/%s',
          $a->{type},
          lc($a->{primary}),
          lc($a->{secondary}));
        my $key_b = sprintf('%s/%s/%s',
          $b->{type},
          lc($b->{primary}),
          lc($b->{secondary}));
        $key_a cmp $key_b;
      } @$type_pairs
    ) {
      # Pass options down to from_wire_hash (e.g., populateGenericBooks => 0)
      my $pair_obj;
      if ($opts->{skip_generic_books}) {
        $pair_obj = Game::EvonyTKR::Model::General::Pair->from_wire_hash($wp,
          populateGenericBooks => 0);
      }
      else {
        $pair_obj = Game::EvonyTKR::Model::General::Pair->from_wire_hash($wp);
      }

      push @$pairs, $pair_obj if $pair_obj;
    }
  };
  if ($@) {
    warn "Failed to load pairs for type $type: $@";
  }

  return $pairs;
}

# Table session management for idempotency across workers
sub store_table_session ($self, $session_id, $data) {
  my $general_type    = $data->{generalType} // die 'generalType required';
  my $buff_activation = $data->{buffActivation}
    // die 'buffActivation required';
  my $items = $data->{items} // [];
  my $ttl   = $data->{ttl}   // 3600;

  my $now        = time();
  my $expires_at = $now + $ttl;

  # Encode items as JSON
  my $item_list_json = $self->encode($items);

  eval {
    $self->db->insert(
      'table_sessions',
      {
        session_id      => $session_id,
        general_type    => $general_type,
        buff_activation => $buff_activation,
        item_list       => $item_list_json,
        created_at      => $now,
        expires_at      => $expires_at,
      },
      {
        on_conflict => \[
'(session_id) DO UPDATE SET general_type = ?, buff_activation = ?, item_list = ?, created_at = ?, expires_at = ?',
          $general_type, $buff_activation, $item_list_json,
          $now,          $expires_at
        ]
      }
    );
  };

  if ($@) {
    warn "Failed to store table session $session_id: $@";
    return 0;
  }

  return 1;
}

sub get_table_session ($self, $session_id) {
  my $result = eval {
    $self->db->select(
      'table_sessions',
      [
        'session_id', 'general_type', 'buff_activation', 'item_list',
        'created_at', 'expires_at'
      ],
      { session_id => $session_id }
    )->hash;
  };

  return undef unless $result;

  # Check if expired
  return undef if $result->{expires_at} <= time();

  # Decode items from JSON
  my $items = eval { $self->decode($result->{item_list}) };
  if ($@) {
    warn "Failed to decode item_list for session $session_id: $@";
    return undef;
  }

  return {
    session_id     => $result->{session_id},
    generalType    => $result->{general_type},
    buffActivation => $result->{buff_activation},
    items          => $items,
    created_at     => $result->{created_at},
    expires_at     => $result->{expires_at},
  };
}

sub delete_table_session ($self, $session_id) {
  my $result =
    eval { $self->db->delete('table_sessions', { session_id => $session_id }); };

  return $result ? $result->rows : 0;
}

sub expire_table_sessions ($self, $max_age = undef) {
  my $cutoff_time = defined($max_age) ? time() - $max_age : time();

  my $result = eval {
    $self->db->delete('table_sessions', \['expires_at <= ?', $cutoff_time]);
  };

  return $result ? $result->rows : 0;
}

# Clear all data
sub clear_all_data ($self) {
  my @tables = qw(
    metadata generals ascending_attributes builtin_books generic_books
    specialties covenants general_conflicts glossary_terms pairs pairs_individual
    job_completed work_units general_buff_cache table_sessions
  );

  for my $table (@tables) {
    eval { $self->db->delete($table) };
  }
  return 1;
}

1;

__DATA__

@@ evonytkr
-- 1 up
CREATE TABLE IF NOT EXISTS metadata (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS job_completed (
  job_name TEXT PRIMARY KEY,
  completed_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS generals (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS ascending_attributes (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS builtin_books (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS generic_books (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS specialties (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS covenants (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS glossary_terms (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS general_conflicts (
  pair_key TEXT PRIMARY KEY,
  conflicts INTEGER NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS pairs (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS pairs_individual (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS work_units (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS general_buff_cache (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at DOUBLE PRECISION NOT NULL
);

-- Add indexes for common queries
CREATE INDEX IF NOT EXISTS idx_pairs_individual_type
  ON pairs_individual (name text_pattern_ops);

CREATE INDEX IF NOT EXISTS idx_conflicts_pair_key
  ON general_conflicts (pair_key);

-- 1 down
DROP TABLE IF EXISTS general_buff_cache;
DROP TABLE IF EXISTS work_units;
DROP TABLE IF EXISTS pairs_individual;
DROP TABLE IF EXISTS pairs;
DROP TABLE IF EXISTS general_conflicts;
DROP TABLE IF EXISTS glossary_terms;
DROP TABLE IF EXISTS covenants;
DROP TABLE IF EXISTS specialties;
DROP TABLE IF EXISTS generic_books;
DROP TABLE IF EXISTS builtin_books;
DROP TABLE IF EXISTS ascending_attributes;
DROP TABLE IF EXISTS generals;
DROP TABLE IF EXISTS job_completed;
DROP TABLE IF EXISTS metadata;

-- 2 up
CREATE TABLE IF NOT EXISTS table_sessions (
  session_id TEXT PRIMARY KEY,
  general_type TEXT NOT NULL,
  buff_activation TEXT NOT NULL,
  item_list TEXT NOT NULL,
  created_at DOUBLE PRECISION NOT NULL,
  expires_at DOUBLE PRECISION NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_table_sessions_expires
  ON table_sessions (expires_at);

-- 2 down
DROP TABLE IF EXISTS table_sessions;

__END__

=head1 NAME

Game::EvonyTKR::Service::PostgreSQLPersistence - PostgreSQL persistence backend

=head1 DESCRIPTION

PostgreSQL implementation for both development and production use. Provides the same
interface as SQLitePersistence and DynamoDBPersistence but uses local or networked
PostgreSQL database.

This uses the same PostgreSQL instance as Minion, but with a separate database
for application data to keep concerns separated.

=head1 CONFIGURATION

  # In config file:
  persistence:
    backend: postgresql
    postgresql_dsn: 'postgresql:///evonytkr_app_data'

  # Or via environment:
  export POSTGRESQL_DSN='postgresql:///evonytkr_app_data'

=head1 ADVANTAGES OVER SQLITE

- Zero network latency (local connection via Unix socket)
- Better concurrency (MVCC, row-level locking)
- No corruption issues under load
- Same database engine for dev and production (dev/prod parity)
- Native JSON support (can optimize later with JSONB)

=cut

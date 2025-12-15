package Game::EvonyTKR::Service::SQLitePersistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base,                        -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::JSON', -role;
use Mojo::SQLite;

use Carp;
use Time::HiRes 'time';

has 'config';    # Config hash from NotYAMLConfig

has 'db_path' => sub ($self) {
  my $config = $self->config || {};
  return $config->{sqlite_db_path} || $ENV{SQLITE_DB_PATH} || './evonytkr.db';
};

has 'sqlite' => sub ($self) {
  my $sqlite = Mojo::SQLite->new('sqlite:' . $self->db_path);

  # Apply optimizations
  $sqlite->on(
    connection => sub ($sqlite, $dbh) {
      $dbh->do('PRAGMA journal_mode=WAL');
      $dbh->do('PRAGMA synchronous=NORMAL');
      $dbh->do('PRAGMA busy_timeout=30000');
    }
  );

  # Create tables - run migrations immediately
  $sqlite->migrations->name('evonytkr')->from_data->migrate;

  return $sqlite;
};

has 'db' => sub ($self) { $self->sqlite->db };

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
    warn "[SQLite] Cannot harvest without current_run_id";
    return 0;
  }

  # First count what we're about to delete
  my $count = $self->db->select('job_completed',
    [\'COUNT(*)'], \["job_name NOT LIKE ?", "${current_run_id}:%"])->array->[0];

  # Delete all records that don't start with current_run_id
  # This includes legacy records (no run_id prefix) and old run_ids
  eval {
    $self->db->delete('job_completed',
      \["job_name NOT LIKE ?", "${current_run_id}:%"]);
  };

  if ($@) {
    warn "[SQLite] Failed to harvest job_completed records: $@\n";
    return 0;
  }

  warn
    sprintf("[SQLite] Harvested %d stale job_completed records\n", $count || 0);
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
    warn sprintf("[SQLite] JSON decode failed for table=%s, key=%s: %s\n",
      $table, $key, $@);
    warn sprintf("[SQLite] Raw data (first 200 chars): %s\n",
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
  $self->get_data('glossary_terms', $name);
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
  my $tx    = $self->db->begin;
  my $count = 0;

  eval {
    foreach my $g1 (keys %$conflicts_hash) {
      foreach my $g2 (keys %{ $conflicts_hash->{$g1} }) {
        my ($sorted_g1, $sorted_g2) = sort ($g1, $g2);
        my $key       = "$sorted_g1:$sorted_g2";
        my $conflicts = $conflicts_hash->{$g1}{$g2} ? 1 : 0;

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
    }
    $tx->commit;
    1;
  } or do {
    my $error = $@ || 'unknown error';
    $self->log_error(
      sprintf("[SQLite] Batch conflict write failed: %s", $error));
    $tx->rollback;
    return 0;
  };

  $self->log_info(sprintf("[SQLite] Batch wrote %d conflict items", $count));
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
  $self->store_data('pairs_individual', $key, $data);
}
sub get_pair ($self, $key) { $self->get_data('pairs_individual', $key) }

sub get_all_pair_types ($self) {
  my $results = $self->db->select('pairs', ['name'])->arrays;
  return [map { $_->[0] } @$results];
}

sub list_pairs_by_type ($self, $type) { $self->get_pairs_by_type($type) }

# Clear all data
sub clear_all_data ($self) {
  my @tables = qw(
    metadata generals ascending_attributes builtin_books generic_books
    specialties covenants general_conflicts glossary_terms pairs pairs_individual
    job_completed
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
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS job_completed (
  job_name TEXT PRIMARY KEY,
  completed_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS generals (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS ascending_attributes (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS builtin_books (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS generic_books (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS specialties (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS covenants (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS glossary_terms (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS general_conflicts (
  pair_key TEXT PRIMARY KEY,
  conflicts INTEGER NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS pairs (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS pairs_individual (
  name TEXT PRIMARY KEY,
  data TEXT NOT NULL,
  updated_at REAL NOT NULL
);

__END__

=head1 NAME

Game::EvonyTKR::Service::SQLitePersistence - SQLite persistence backend

=head1 DESCRIPTION

SQLite implementation for development use. Provides the same interface
as DynamoDBPersistence but uses local SQLite database.

=cut

package Game::EvonyTKR::Service::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base,                           -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
use Mojo::Base 'Game::EvonyTKR::Role::Common',  -role;
use Mojo::File;
use Mojo::SQLite;
use JSON::PP qw( decode_json);
use Carp;

# Schema version - increment when schema changes
our $SCHEMA_VERSION = 3;

has 'db_path' => sub {
  my $home = Mojo::Home->new->detect('Game::EvonyTKR');
  return $home->child('var', 'persistence.db')->to_string;
};

has 'sqlite' => sub ($self) {
  my $db_path = $self->db_path;

  # Ensure directory exists
  Mojo::File->new($db_path)->dirname->make_path;

  # Check database integrity and recover if corrupted
  my $db_ok = 0;
  if (-f $db_path) {
    $db_ok = eval {
      my $test_sqlite = Mojo::SQLite->new('file:' . $db_path);
      my $test_db = $test_sqlite->db;
      my $integrity = $test_db->query('PRAGMA integrity_check')->hash;
      return $integrity->{integrity_check} eq 'ok';
    };

    unless ($db_ok) {
      $self->log_error("Persistence database integrity check failed: $@");
      $self->log_warn("Attempting to recover by recreating database...");

      # Backup corrupted database
      my $backup_dir = Mojo::File->new($db_path)->dirname->child('backup');
      $backup_dir->make_path;
      my $timestamp = time();
      my $backup_path = $backup_dir->child("persistence.db.corrupt.$timestamp");

      eval {
        require File::Copy;
        File::Copy::copy($db_path, $backup_path);
        $self->log_info("Backed up corrupted database to $backup_path");
      };

      # Remove corrupted database files
      for my $suffix ('', '-shm', '-wal') {
        my $file = $db_path . $suffix;
        unlink $file if -f $file;
      }

      $self->log_info("Recreated persistence database at $db_path");
    }
  }

  my $sqlite = Mojo::SQLite->new('file:' . $db_path);

  # Enable WAL mode and EBS-compatible settings
  $sqlite->db->query('PRAGMA journal_mode = WAL');
  $sqlite->db->query('PRAGMA synchronous = NORMAL');
  $sqlite->db->query('PRAGMA busy_timeout = 5000');
  # Disable memory-mapped I/O for compatibility with EBS volumes
  $sqlite->db->query('PRAGMA mmap_size = 0');
  # Ensure normal locking mode (not exclusive)
  $sqlite->db->query('PRAGMA locking_mode = NORMAL');
  # Increase cache size for better performance
  $sqlite->db->query('PRAGMA cache_size = -32000');  # 32MB cache

  # Set pragmas for all future connections
  $sqlite->on(
    connection => sub ($sqlite, $dbh) {
      $dbh->do('PRAGMA journal_mode = WAL');
      $dbh->do('PRAGMA synchronous = NORMAL');
      $dbh->do('PRAGMA busy_timeout = 5000');
      $dbh->do('PRAGMA mmap_size = 0');
      $dbh->do('PRAGMA locking_mode = NORMAL');
      $dbh->do('PRAGMA cache_size = -32000');
    }
  );

  $self->_initialize_schema($sqlite);
  return $sqlite;
};

has encoder => sub {
  return JSON::PP->new->utf8->allow_blessed->convert_blessed;
};

has 'lifecycle_id' => sub ($self) {
  # Ensure database is initialized first (this triggers _initialize_schema)
  my $db = $self->sqlite;

# Now read lifecycle_id from metadata to ensure consistency across worker processes
  my $stored = $self->get_metadata('lifecycle_id');

# If not in metadata (shouldn't happen after initialization), generate and store one
  unless ($stored) {
    $stored = time . '_' . $$;
    $self->set_metadata('lifecycle_id', $stored);
  }

  return $stored;
};

sub _initialize_schema ($self, $sqlite) {
  my $db = $sqlite->db;

  # Check if metadata table exists
  my $tables = $db->query(q{
    SELECT name FROM sqlite_master WHERE type='table' AND name='metadata'
  })->hash;

  my $current_version = 0;
  if ($tables) {
    my $version_row =
      $db->query('SELECT value FROM metadata WHERE key = ?', 'schema_version')
      ->hash;
    $current_version = $version_row ? $version_row->{value} : 0;
  }

  if ($current_version == 0) {
    $self->log_info('Initializing persistence database schema');
    $self->_create_schema_v1($db);
    $self->_set_metadata_direct($db, 'schema_version', $SCHEMA_VERSION);

    # Generate and store lifecycle_id for this app instance
    my $new_lifecycle_id = time . '_' . $$;
    $self->_set_metadata_direct($db, 'lifecycle_id', $new_lifecycle_id);
  }
  elsif ($current_version < $SCHEMA_VERSION) {
    $self->log_info(sprintf(
      'Migrating persistence schema from v%d to v%d',
      $current_version, $SCHEMA_VERSION
    ));
    $self->_migrate_schema($db, $current_version, $SCHEMA_VERSION);
    $self->_set_metadata_direct($db, 'schema_version', $SCHEMA_VERSION);
  }

  return 1;
}

sub _create_schema_v1 ($self, $db) {
  # Metadata table for app-level state
  $db->query(q{
    CREATE TABLE IF NOT EXISTS metadata (
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL,
      updated_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Job completion tracking
  $db->query(q{
    CREATE TABLE IF NOT EXISTS job_completions (
      task_name TEXT PRIMARY KEY,
      completed_at INTEGER NOT NULL,
      lifecycle_id TEXT NOT NULL,
      notes TEXT
    )
  });

  # Generals
  $db->query(q{
    CREATE TABLE IF NOT EXISTS generals (
      name TEXT PRIMARY KEY,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Builtin books (skill books tied to generals)
  $db->query(q{
    CREATE TABLE IF NOT EXISTS builtin_books (
      name TEXT PRIMARY KEY,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Generic books (skill books players can equip)
  $db->query(q{
    CREATE TABLE IF NOT EXISTS generic_books (
      name TEXT NOT NULL,
      level INTEGER NOT NULL,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now')),
      PRIMARY KEY (name, level)
    )
  });

  # Covenants
  $db->query(q{
    CREATE TABLE IF NOT EXISTS covenants (
      name TEXT PRIMARY KEY,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Specialties
  $db->query(q{
    CREATE TABLE IF NOT EXISTS specialties (
      name TEXT PRIMARY KEY,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Ascending Attributes
  $db->query(q{
    CREATE TABLE IF NOT EXISTS ascending_attributes (
      name TEXT PRIMARY KEY,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Glossary Terms
  $db->query(q{
    CREATE TABLE IF NOT EXISTS glossary_terms (
      term TEXT PRIMARY KEY,
      data_json TEXT NOT NULL,
      loaded_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # General Conflicts (bidirectional conflict tracking)
  $db->query(q{
    CREATE TABLE IF NOT EXISTS general_conflicts (
      general1_name TEXT NOT NULL,
      general2_name TEXT NOT NULL,
      conflicts INTEGER NOT NULL DEFAULT 1,
      detected_at INTEGER DEFAULT (strftime('%s', 'now')),
      PRIMARY KEY (general1_name, general2_name)
    )
  });

  # Pairs (general pairings by type)
  $db->query(q{
    CREATE TABLE IF NOT EXISTS pairs (
      pair_key TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      primary_name TEXT NOT NULL,
      secondary_name TEXT NOT NULL,
      data_json TEXT NOT NULL,
      lifecycle_id TEXT NOT NULL,
      created_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Indices for common queries
  $db->query(
'CREATE INDEX IF NOT EXISTS idx_job_completions_lifecycle ON job_completions(lifecycle_id)'
  );
  $db->query(
    'CREATE INDEX IF NOT EXISTS idx_generals_loaded_at ON generals(loaded_at)');
  $db->query(
'CREATE INDEX IF NOT EXISTS idx_conflicts_general1 ON general_conflicts(general1_name)'
  );
  $db->query(
'CREATE INDEX IF NOT EXISTS idx_conflicts_general2 ON general_conflicts(general2_name)'
  );
  $db->query('CREATE INDEX IF NOT EXISTS idx_pairs_type ON pairs(type)');
  $db->query(
    'CREATE INDEX IF NOT EXISTS idx_pairs_lifecycle ON pairs(lifecycle_id)');

  $self->log_info('Schema v1 created successfully');
}

sub _migrate_schema ($self, $db, $from_version, $to_version) {
  for my $version ($from_version + 1 .. $to_version) {
    if ($version == 2) {
      $self->_migrate_to_v2($db);
    }
    elsif ($version == 3) {
      $self->_migrate_to_v3($db);
    }
  }
}

sub _migrate_to_v2 ($self, $db) {
  $self->log_info('Migrating to schema v2: adding pairs table');

  # Add pairs table
  $db->query(q{
    CREATE TABLE IF NOT EXISTS pairs (
      pair_key TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      primary_name TEXT NOT NULL,
      secondary_name TEXT NOT NULL,
      data_json TEXT NOT NULL,
      lifecycle_id TEXT NOT NULL,
      created_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  });

  # Add indices
  $db->query('CREATE INDEX IF NOT EXISTS idx_pairs_type ON pairs(type)');
  $db->query(
    'CREATE INDEX IF NOT EXISTS idx_pairs_lifecycle ON pairs(lifecycle_id)');

  $self->log_info('Schema v2 migration completed');
}

sub _migrate_to_v3 ($self, $db) {
  $self->log_info(
    'Migrating to schema v3: adding conflicts column to general_conflicts');

  # Add conflicts column (default 1 for existing rows which are all conflicts)
  $db->query(q{
    ALTER TABLE general_conflicts ADD COLUMN conflicts INTEGER NOT NULL DEFAULT 1
  });

  $self->log_info('Schema v3 migration completed');
}

##############################################################################
# Metadata methods
##############################################################################

# Internal helper to avoid recursion during initialization
sub _set_metadata_direct ($self, $db, $key, $value) {
  $db->query(
q{INSERT OR REPLACE INTO metadata (key, value, updated_at) VALUES (?, ?, strftime('%s', 'now'))},
    $key, $value
  );
  return 1;
}

sub get_metadata ($self, $key) {
  my $db = $self->sqlite->db;
  my $result =
    $db->query('SELECT value FROM metadata WHERE key = ?', $key)->hash;

  return $result ? $result->{value} : undef;
}

sub set_metadata ($self, $key, $value) {
  my $db = $self->sqlite->db;
  return $self->_set_metadata_direct($db, $key, $value);
}

sub is_initialized ($self) {
  my $version = $self->get_metadata('schema_version');
  return defined($version) && $version > 0;
}

sub needs_rebuild ($self) {
  # Check if schema version matches
  my $stored_version = $self->get_metadata('schema_version') // 0;
  return 1 if $stored_version < $SCHEMA_VERSION;

  # Check if we have any data
  my $db = $self->sqlite->db;
  my $general_count =
    $db->query('SELECT COUNT(*) as count FROM generals')->hash->{count};
  return 1 if $general_count == 0;

  return 0;
}

##############################################################################
# Job completion tracking
##############################################################################

sub mark_job_completed ($self, $task_name, $notes = undef) {
  my $db = $self->sqlite->db;

  $db->query(
    q{
    INSERT OR REPLACE INTO job_completions (task_name, completed_at, lifecycle_id, notes)
    VALUES (?, strftime('%s', 'now'), ?, ?)
  }, $task_name, $self->lifecycle_id, $notes
  );

  $self->log_debug("Marked job as completed: $task_name");
  return 1;
}

sub is_job_completed ($self, $task_name) {
  my $db = $self->sqlite->db;

  my $result = $db->query(
    q{
    SELECT completed_at FROM job_completions
    WHERE task_name = ? AND lifecycle_id = ?
  }, $task_name, $self->lifecycle_id
  )->hash;

  return defined($result);
}

sub get_job_completion_time ($self, $task_name) {
  my $db = $self->sqlite->db;

  my $result = $db->query(
    q{
    SELECT completed_at FROM job_completions
    WHERE task_name = ? AND lifecycle_id = ?
  }, $task_name, $self->lifecycle_id
  )->hash;

  return $result ? $result->{completed_at} : undef;
}

sub clear_lifecycle_jobs ($self) {
  my $db = $self->sqlite->db;

  # Clear jobs from previous lifecycles
  $db->query(
    q{
    DELETE FROM job_completions WHERE lifecycle_id != ?
  }, $self->lifecycle_id
  );

  $self->log_info('Cleared job completions from previous lifecycles');
  return 1;
}

##############################################################################
# Data storage methods - Generals
##############################################################################

sub store_general ($self, $name, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO generals (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $normalized_name, $json
  );

  return 1;
}

sub get_general ($self, $name) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $result = $db->query('SELECT data_json FROM generals WHERE name = ?',
    $normalized_name)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_generals ($self) {
  my $db = $self->sqlite->db;

  my @generals;
  my $results =
    $db->query('SELECT name, data_json FROM generals ORDER BY name');

  while (my $row = $results->hash) {
    push @generals, decode_json($row->{data_json});
  }

  return \@generals;
}

sub count_generals ($self) {
  my $db = $self->sqlite->db;
  return $db->query('SELECT COUNT(*) as count FROM generals')->hash->{count};
}

##############################################################################
# Data storage methods - Builtin Books
##############################################################################

sub store_builtin_book ($self, $name, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO builtin_books (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $normalized_name, $json
  );

  return 1;
}

sub get_builtin_book ($self, $name) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $result = $db->query('SELECT data_json FROM builtin_books WHERE name = ?',
    $normalized_name)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_builtin_books ($self) {
  my $db = $self->sqlite->db;

  my @books;
  my $results =
    $db->query('SELECT name, data_json FROM builtin_books ORDER BY name');

  while (my $row = $results->hash) {
    push @books, decode_json($row->{data_json});
  }

  return \@books;
}

##############################################################################
# Data storage methods - Generic Books
##############################################################################

sub store_generic_book ($self, $name, $level, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO generic_books (name, level, data_json, loaded_at)
    VALUES (?, ?, ?, strftime('%s', 'now'))
  }, $normalized_name, $level, $json
  );

  return 1;
}

sub get_generic_book ($self, $name, $level) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $result = $db->query(
    'SELECT data_json FROM generic_books WHERE name = ? AND level = ?',
    $normalized_name, $level)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_generic_books ($self) {
  my $db = $self->sqlite->db;

  my @books;
  my $results = $db->query(
    'SELECT name, level, data_json FROM generic_books ORDER BY name, level');

  while (my $row = $results->hash) {
    push @books, decode_json($row->{data_json});
  }

  return \@books;
}

##############################################################################
# Data storage methods - Covenants
##############################################################################

sub store_covenant ($self, $name, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO covenants (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $normalized_name, $json
  );

  return 1;
}

sub get_covenant ($self, $name) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $result = $db->query('SELECT data_json FROM covenants WHERE name = ?',
    $normalized_name)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_covenants ($self) {
  my $db = $self->sqlite->db;

  my @covenants;
  my $results =
    $db->query('SELECT name, data_json FROM covenants ORDER BY name');

  while (my $row = $results->hash) {
    push @covenants, decode_json($row->{data_json});
  }

  return \@covenants;
}

##############################################################################
# Data storage methods - Specialties
##############################################################################

sub store_specialty ($self, $name, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO specialties (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $normalized_name, $json
  );

  return 1;
}

sub get_specialty ($self, $name) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $result = $db->query('SELECT data_json FROM specialties WHERE name = ?',
    $normalized_name)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_specialties ($self) {
  my $db = $self->sqlite->db;

  my @specialties;
  my $results =
    $db->query('SELECT name, data_json FROM specialties ORDER BY name');

  while (my $row = $results->hash) {
    push @specialties, decode_json($row->{data_json});
  }

  return \@specialties;
}

##############################################################################
# Data storage methods - Ascending Attributes
##############################################################################

sub store_ascending_attribute ($self, $name, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO ascending_attributes (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $normalized_name, $json
  );

  return 1;
}

sub get_ascending_attribute ($self, $name) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($name);

  my $result =
    $db->query('SELECT data_json FROM ascending_attributes WHERE name = ?',
    $normalized_name)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_ascending_attributes ($self) {
  my $db = $self->sqlite->db;

  my @attrs;
  my $results = $db->query(
    'SELECT name, data_json FROM ascending_attributes ORDER BY name');

  while (my $row = $results->hash) {
    push @attrs, decode_json($row->{data_json});
  }

  return \@attrs;
}

##############################################################################
# Glossary Terms methods
##############################################################################

sub store_glossary_term ($self, $term, $data_hash) {
  my $db              = $self->sqlite->db;
  my $normalized_term = lc($self->normalize($term));
  $normalized_term =~ s/ /_/g;

  my $json = $self->encoder->encode($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO glossary_terms (term, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $normalized_term, $json
  );

  return 1;
}

sub get_glossary_term ($self, $term) {
  my $db              = $self->sqlite->db;
  my $normalized_term = lc($self->normalize($term));
  $normalized_term =~ s/ /_/g;

  my $result = $db->query('SELECT data_json FROM glossary_terms WHERE term = ?',
    $normalized_term)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_glossary_terms ($self) {
  my $db = $self->sqlite->db;

  my @terms;
  my $results =
    $db->query('SELECT term, data_json FROM glossary_terms ORDER BY term');

  while (my $row = $results->hash) {
    push @terms, decode_json($row->{data_json});
  }

  return \@terms;
}

##############################################################################
# Data storage methods - General Conflicts
##############################################################################

sub store_conflict ($self, $general1_name, $general2_name, $conflicts) {
  my $db = $self->sqlite->db;

  # Normalize names for consistent storage
  my $norm1 = $self->normalize($general1_name);
  my $norm2 = $self->normalize($general2_name);

  # Always store in alphabetical order to avoid duplicates
  my ($name1, $name2) = sort ($norm1, $norm2);

  $db->query(
    q{
    INSERT OR IGNORE INTO general_conflicts (general1_name, general2_name, conflicts, detected_at)
    VALUES (?, ?, ?, strftime('%s', 'now'))
  }, $name1, $name2, $conflicts
  );

  return 1;
}

sub get_conflict ($self, $general1_name, $general2_name) {
  my $db = $self->sqlite->db;

  my $norm1 = $self->normalize($general1_name);
  my $norm2 = $self->normalize($general2_name);

  # Always query in alphabetical order since that's how we store
  my ($name1, $name2) = sort ($norm1, $norm2);

  my $result = $db->query(
'SELECT conflicts FROM general_conflicts WHERE general1_name = ? AND general2_name = ?',
    $name1, $name2
  )->hash;

  return $result ? $result->{conflicts} : undef;
}

sub get_conflicts_for_general ($self, $general_name) {
  my $db              = $self->sqlite->db;
  my $normalized_name = $self->normalize($general_name);

  my @conflicts;

  # Find conflicts where this general is first
  my $results1 = $db->query(
    'SELECT general2_name FROM general_conflicts WHERE general1_name = ?',
    $normalized_name);
  while (my $row = $results1->hash) {
    push @conflicts, $row->{general2_name};
  }

  # Find conflicts where this general is second
  my $results2 = $db->query(
    'SELECT general1_name FROM general_conflicts WHERE general2_name = ?',
    $normalized_name);
  while (my $row = $results2->hash) {
    push @conflicts, $row->{general1_name};
  }

  return \@conflicts;
}

sub load_all_conflicts ($self) {
  my $db = $self->sqlite->db;

  my %by_general;
  my $results =
    $db->query(
    'SELECT general1_name, general2_name, conflicts FROM general_conflicts');

  while (my $row = $results->hash) {
    my $g1        = $row->{general1_name};
    my $g2        = $row->{general2_name};
    my $conflicts = $row->{conflicts};

    # Store bidirectional mapping with conflict status
    $by_general{$g1}{$g2} = $conflicts;
    $by_general{$g2}{$g1} = $conflicts;
  }

  return \%by_general;
}

sub count_conflicts ($self) {
  my $db = $self->sqlite->db;
  return $db->query('SELECT COUNT(*) as count FROM general_conflicts')
    ->hash->{count};
}

##############################################################################
# Pairs methods
##############################################################################

sub store_pair ($self, $key, $wire_pair) {
  my $db = $self->sqlite->db;

  # Normalize the key (already normalized in caller, but be safe)
  my $normalized_key = $self->normalize($key);
  $normalized_key =~ s/ /_/g;

  my $json = $self->encoder->encode($wire_pair);

  $db->query(
    q{
    INSERT OR REPLACE INTO pairs (pair_key, type, primary_name, secondary_name, data_json, lifecycle_id, created_at)
    VALUES (?, ?, ?, ?, ?, ?, strftime('%s', 'now'))
  },
    $normalized_key,
    $wire_pair->{type},
    $self->normalize($wire_pair->{primary}),
    $self->normalize($wire_pair->{secondary}),
    $json,
    $self->lifecycle_id
  );

  return 1;
}

sub get_pair ($self, $key) {
  my $db = $self->sqlite->db;

  # Normalize the key to match storage format
  my $normalized_key = $self->normalize($key);
  $normalized_key =~ s/ /_/g;

  my $result = $db->query('SELECT data_json FROM pairs WHERE pair_key = ?',
    $normalized_key)->hash;

  return $result ? decode_json($result->{data_json}) : undef;
}

sub list_pairs_by_type ($self, $type) {
  my $db           = $self->sqlite->db;
  my $lifecycle_id = $self->lifecycle_id;

  my $results = $db->query(
    'SELECT data_json FROM pairs WHERE type = ? AND lifecycle_id = ?',
    $type, $lifecycle_id);

  my @pairs;
  while (my $row = $results->hash) {
    push @pairs, decode_json($row->{data_json});
  }

  return \@pairs;
}

sub get_all_pair_types ($self) {
  my $db           = $self->sqlite->db;
  my $lifecycle_id = $self->lifecycle_id;

  my $results =
    $db->query('SELECT DISTINCT type FROM pairs WHERE lifecycle_id = ?',
    $lifecycle_id);

  my @types;
  while (my $row = $results->hash) {
    push @types, $row->{type};
  }

  return \@types;
}

sub count_pairs_by_type ($self, $type = undef) {
  my $db           = $self->sqlite->db;
  my $lifecycle_id = $self->lifecycle_id;

  if (defined $type) {
    return $db->query(
      'SELECT COUNT(*) as count FROM pairs WHERE type = ? AND lifecycle_id = ?',
      $type, $lifecycle_id
    )->hash->{count};
  }
  else {
    return $db->query(
      'SELECT COUNT(*) as count FROM pairs WHERE lifecycle_id = ?',
      $lifecycle_id)->hash->{count};
  }
}

sub set_ml_conflicts ($self, $conflicts) {
  my $json = $self->encoder->encode($conflicts);
  return $self->set_metadata('ml_conflicts', $json);
}

sub get_ml_conflicts ($self) {
  # Read ML predictions from general_conflicts table
  # This is the same as load_all_conflicts but returns the full structure
  return $self->load_all_conflicts();
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Service::Persistence - SQLite persistence layer for EvonyTKR data

=head1 SYNOPSIS

  my $persist = Game::EvonyTKR::Service::Persistence->new;

  # Check initialization state
  if (!$persist->is_initialized() || $persist->needs_rebuild()) {
    # Trigger db-build job
  }

  # Job tracking
  $persist->mark_job_completed('load_all_generals');
  if ($persist->is_job_completed('load_all_generals')) {
    # Job already ran in this lifecycle
  }

  # Store data
  $persist->store_general('Aethelflaed', $general_hash);

  # Retrieve data
  my $general = $persist->get_general('Aethelflaed');
  my $all_generals = $persist->list_generals();

=head1 DESCRIPTION

This service provides SQLite-backed persistence for EvonyTKR game data,
replacing memcached as the source of truth. Data is stored as JSON blobs
in the wire format used by the application.

Key features:
- Schema versioning and migrations
- Lifecycle-based job tracking (survives hypnotoad restarts)
- Efficient storage of generals, books, covenants, specialties, etc.
- Memcached remains as a caching layer

=head1 SCHEMA VERSION

Current schema version: 1

When incrementing SCHEMA_VERSION, implement migration in _migrate_schema().

=cut

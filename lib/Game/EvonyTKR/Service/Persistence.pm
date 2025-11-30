package Game::EvonyTKR::Service::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base,                           -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
use Mojo::Base 'Game::EvonyTKR::Role::Common',  -role;
use Mojo::File;
use Mojo::SQLite;
use Mojo::JSON qw(encode_json decode_json);
use Carp;

# Schema version - increment when schema changes
our $SCHEMA_VERSION = 1;

has 'db_path' => sub {
  my $home = Mojo::Home->new->detect('Game::EvonyTKR');
  return $home->child('var', 'persistence.db')->to_string;
};

has 'sqlite' => sub ($self) {
  my $db_path = $self->db_path;

  # Ensure directory exists
  Mojo::File->new($db_path)->dirname->make_path;

  my $sqlite = Mojo::SQLite->new('file:' . $db_path);
  $self->_initialize_schema($sqlite);
  return $sqlite;
};

has 'lifecycle_id' => sub {
  # Generate unique ID for this app lifecycle
  return time . '_' . $$;
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
    $self->logger->info('Initializing persistence database schema');
    $self->_create_schema_v1($db);
    $self->_set_metadata_direct($db, 'schema_version', $SCHEMA_VERSION);
    $self->_set_metadata_direct($db, 'lifecycle_id',   $self->lifecycle_id);
  }
  elsif ($current_version < $SCHEMA_VERSION) {
    $self->logger->info(sprintf(
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

  # General Conflicts (bidirectional conflict tracking)
  $db->query(q{
    CREATE TABLE IF NOT EXISTS general_conflicts (
      general1_name TEXT NOT NULL,
      general2_name TEXT NOT NULL,
      detected_at INTEGER DEFAULT (strftime('%s', 'now')),
      PRIMARY KEY (general1_name, general2_name)
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

  $self->logger->info('Schema v1 created successfully');
}

sub _migrate_schema ($self, $db, $from_version, $to_version) {
  # Future migrations go here
  # for my $version ($from_version + 1 .. $to_version) {
  #   if ($version == 2) {
  #     $self->_migrate_to_v2($db);
  #   }
  # }

  $self->logger->warn('No migration path implemented yet');
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

  $self->logger->debug("Marked job as completed: $task_name");
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

  $self->logger->info('Cleared job completions from previous lifecycles');
  return 1;
}

##############################################################################
# Data storage methods - Generals
##############################################################################

sub store_general ($self, $name, $data_hash) {
  my $db = $self->sqlite->db;

  my $json = encode_json($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO generals (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $name, $json
  );

  return 1;
}

sub get_general ($self, $name) {
  my $db = $self->sqlite->db;

  my $result =
    $db->query('SELECT data_json FROM generals WHERE name = ?', $name)->hash;

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
  my $db = $self->sqlite->db;

  my $json = encode_json($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO builtin_books (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $name, $json
  );

  return 1;
}

sub get_builtin_book ($self, $name) {
  my $db = $self->sqlite->db;

  my $result =
    $db->query('SELECT data_json FROM builtin_books WHERE name = ?', $name)
    ->hash;

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
  my $db = $self->sqlite->db;

  my $json = encode_json($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO generic_books (name, level, data_json, loaded_at)
    VALUES (?, ?, ?, strftime('%s', 'now'))
  }, $name, $level, $json
  );

  return 1;
}

sub get_generic_book ($self, $name, $level) {
  my $db = $self->sqlite->db;

  my $result = $db->query(
    'SELECT data_json FROM generic_books WHERE name = ? AND level = ?',
    $name, $level)->hash;

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
  my $db = $self->sqlite->db;

  my $json = encode_json($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO covenants (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $name, $json
  );

  return 1;
}

sub get_covenant ($self, $name) {
  my $db = $self->sqlite->db;

  my $result =
    $db->query('SELECT data_json FROM covenants WHERE name = ?', $name)->hash;

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
  my $db = $self->sqlite->db;

  my $json = encode_json($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO specialties (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $name, $json
  );

  return 1;
}

sub get_specialty ($self, $name) {
  my $db = $self->sqlite->db;

  my $result =
    $db->query('SELECT data_json FROM specialties WHERE name = ?', $name)->hash;

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
  my $db = $self->sqlite->db;

  my $json = encode_json($data_hash);
  $db->query(
    q{
    INSERT OR REPLACE INTO ascending_attributes (name, data_json, loaded_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $name, $json
  );

  return 1;
}

sub get_ascending_attribute ($self, $name) {
  my $db = $self->sqlite->db;

  my $result =
    $db->query('SELECT data_json FROM ascending_attributes WHERE name = ?',
    $name)->hash;

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
# Data storage methods - General Conflicts
##############################################################################

sub store_conflict ($self, $general1_name, $general2_name) {
  my $db = $self->sqlite->db;

  # Always store in alphabetical order to avoid duplicates
  my ($name1, $name2) = sort ($general1_name, $general2_name);

  $db->query(
    q{
    INSERT OR IGNORE INTO general_conflicts (general1_name, general2_name, detected_at)
    VALUES (?, ?, strftime('%s', 'now'))
  }, $name1, $name2
  );

  return 1;
}

sub get_conflicts_for_general ($self, $general_name) {
  my $db = $self->sqlite->db;

  my @conflicts;

  # Find conflicts where this general is first
  my $results1 = $db->query(
    'SELECT general2_name FROM general_conflicts WHERE general1_name = ?',
    $general_name);
  while (my $row = $results1->hash) {
    push @conflicts, $row->{general2_name};
  }

  # Find conflicts where this general is second
  my $results2 = $db->query(
    'SELECT general1_name FROM general_conflicts WHERE general2_name = ?',
    $general_name);
  while (my $row = $results2->hash) {
    push @conflicts, $row->{general1_name};
  }

  return \@conflicts;
}

sub load_all_conflicts ($self) {
  my $db = $self->sqlite->db;

  my %by_general;
  my $results =
    $db->query('SELECT general1_name, general2_name FROM general_conflicts');

  while (my $row = $results->hash) {
    my $g1 = $row->{general1_name};
    my $g2 = $row->{general2_name};

    # Store bidirectional mapping
    $by_general{$g1}{$g2} = 1;
    $by_general{$g2}{$g1} = 1;
  }

  return \%by_general;
}

sub count_conflicts ($self) {
  my $db = $self->sqlite->db;
  return $db->query('SELECT COUNT(*) as count FROM general_conflicts')
    ->hash->{count};
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

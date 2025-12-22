#!/usr/bin/env perl
use v5.42.0;
use strict;
use warnings;

# Script to fix PostgreSQL connection pooling in PostgreSQLPersistence.pm
# Replaces cached 'db' attribute with fresh connection getter

my $file = 'lib/Game/EvonyTKR/Service/PostgreSQLPersistence.pm';

open my $fh, '<', $file or die "Can't open $file: $!";
my @lines = <$fh>;
close $fh;

my $content = join '', @lines;

# 1. Replace cached db attribute with method
$content =~ s/has 'db' => sub \(\$self\) \{ \$self->pg->db \};/# Get a database handle from the connection pool
# DO NOT cache this - always get fresh connection from pool
sub db (\$self) {
  return \$self->pg->db;
}/;

# 2. Add connection test and better config to pg attribute
$content =~ s/(has 'pg' => sub \(\$self\) \{
  my \$pg = Mojo::Pg->new\(\$self->dsn\);

  # Create tables - run migrations with error handling)/(has 'pg' => sub (\$self) {
  my \$dsn = \$self->dsn;
  \$self->log_info("[PostgreSQL] Connecting with DSN: \$dsn");

  my \$pg = Mojo::Pg->new(\$dsn);

  # Set reasonable connection pool parameters
  \$pg->max_connections(10);

  # Test connection before proceeding
  eval {
    my \$test_db = \$pg->db;
    my \$result = \$test_db->query('SELECT current_database(), current_user')->hash;
    \$self->log_info(sprintf(
      "[PostgreSQL] Connected to database '%s' as user '%s'",
      \$result->{current_database}, \$result->{current_user}
    ));
  };
  if (\$@) {
    \$self->log_error("[PostgreSQL] Connection test failed: \$@");
    die "Failed to connect to PostgreSQL: \$@";
  }

  # Create tables - run migrations with error handling)/;

# 3. Add log completion message
$content =~ s/(  return \$pg;
\};)/(
  \$self->log_info("[PostgreSQL] Initialization complete");
  return \$pg;
};)/;

# Write back
open my $out, '>', $file or die "Can't write $file: $!";
print $out $content;
close $out;

print "Fixed PostgreSQL connection handling in $file\n";

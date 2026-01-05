#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use Path::Tiny;
use File::Slurp qw(read_file write_file);

# Safer script to convert only use statements, leave 'has' alone

my $dry_run = $ARGV[0] && $ARGV[0] eq '--dry-run';

my @files = split /\n/,
  `find lib/Game/EvonyTKR/Role lib/Game/EvonyTKR/Model lib/Game/EvonyTKR/Service -name "*.pm" -type f 2>/dev/null`;

say "Found " . scalar(@files) . " files to check";
say "DRY RUN MODE" if $dry_run;
say "";

my $converted_count = 0;

for my $file (@files) {
  my $content = read_file($file);
  my $original = $content;

  # Skip if already using Moo
  if ($content =~ /^use Moo(?:;|::Role;)/m) {
    next;
  }

  # Skip if not using Mojo::Base
  unless ($content =~ /use Mojo::Base/) {
    next;
  }

  say "CONVERTING: $file";

  # Conversion 1: Mojo::Base -role, -signatures → Moo::Role
  $content =~ s/^(\s*)use Mojo::Base -role(?:, -signatures)?;/$1use Moo::Role;/gm;

  # Conversion 2: Mojo::Base 'RoleName', -role → with 'RoleName'
  while ($content =~ s/^(\s*)use Mojo::Base '([^']+)', -role;/$1with '$2';/gm) {
    # Keep applying until no more matches
  }

  # Conversion 3: Mojo::Base 'BaseClass' (for classes) → use Moo; extends 'BaseClass'
  $content =~ s/^(\s*)use Mojo::Base '([^']+)'(?:, -signatures)?;/$1use Moo;\n$1extends '$2';/gm;

  # Conversion 4: Mojo::Base -base → use Moo
  $content =~ s/^(\s*)use Mojo::Base -base(?:, -signatures)?;/$1use Moo;/gm;

  # Check if changes were made
  if ($content ne $original) {
    $converted_count++;
    if ($dry_run) {
      say "  Would convert: $file";
    }
    else {
      write_file($file, $content);
      say "  ✓ Converted: $file";
    }
  }
}

say "";
say "=" x 60;
say "Conversion complete!";
say "Files converted: $converted_count";
say "DRY RUN - no files modified" if $dry_run;
say "";
say "Note: This script only converts 'use' statements.";
say "'has' declarations are left as-is since most are compatible.";
say "";
say "Next steps:";
say "1. Review changes with: git diff";
say "2. Run syntax check: perl -c lib/Game/EvonyTKR/Model/Specialty.pm";
say "3. Fix any 'has' declarations that need Moo format";

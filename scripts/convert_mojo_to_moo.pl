#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use Path::Tiny;
use File::Slurp qw(read_file write_file);

# Script to convert Mojo::Base to Moo/Moo::Role

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
    say "SKIP: $file (already using Moo)";
    next;
  }

  # Skip if not using Mojo::Base
  unless ($content =~ /use Mojo::Base/) {
    say "SKIP: $file (not using Mojo::Base)";
    next;
  }

  say "CONVERTING: $file";

  # Conversion 1: Mojo::Base -role, -signatures → Moo::Role (v5.42 has signatures)
  $content =~ s/^(\s*)use Mojo::Base -role(?:, -signatures)?;/$1use Moo::Role;/gm;

  # Conversion 2: Mojo::Base 'RoleName', -role → with 'RoleName'
  while ($content =~ s/^(\s*)use Mojo::Base '([^']+)', -role;/$1with '$2';/gm) {
    # Keep applying until no more matches
  }

  # Conversion 3: Mojo::Base 'BaseClass' (for classes) → use Moo; extends 'BaseClass'
  # But be careful not to match role compositions
  $content =~ s/^(\s*)use Mojo::Base '([^']+)'(?:, -signatures)?;/$1use Moo;\n$1extends '$2';/gm;

  # Conversion 4: Mojo::Base -base → use Moo
  $content =~ s/^(\s*)use Mojo::Base -base(?:, -signatures)?;/$1use Moo;/gm;

  # Conversion 5: Fix 'has' declarations to Moo format
  # Mojo::Base allows: has 'attr' or has attr => 'default' or has attr => sub {}
  # Moo requires: has attr => (is => 'rw', ...)

  # Pattern: has 'attr' => 'literal_default';
  $content =~ s/^(\s*)has\s+(\w+)\s+=>\s+'([^']+)';/$1has $2 => (is => 'rw', default => '$3');/gm;
  $content =~ s/^(\s*)has\s+(\w+)\s+=>\s+"([^"]+)";/$1has $2 => (is => 'rw', default => "$3");/gm;
  $content =~ s/^(\s*)has\s+(\w+)\s+=>\s+(\d+);/$1has $2 => (is => 'rw', default => $3);/gm;

  # Pattern: has 'attr' => sub { ... };
  # This one is trickier - find sub blocks
  $content =~ s/^(\s*)has\s+(\w+)\s+=>\s+(sub\s*\{[^}]*\});/$1has $2 => (is => 'rw', default => $3);/gm;
  $content =~ s/^(\s*)has\s+(\w+)\s+=>\s+(sub\s+\([^)]*\)\s*\{[^}]*\});/$1has $2 => (is => 'rw', lazy => 1, default => $3);/gm;

  # Pattern: has 'attr'; (no default)
  $content =~ s/^(\s*)has\s+'(\w+)';/$1has $2 => (is => 'rw');/gm;

  # Pattern: has ['attr1', 'attr2'] => 'default'; (multiple attrs)
  # Moo doesn't support this, need to expand
  # For now, leave as-is and handle manually if needed

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
  else {
    say "  No changes needed";
  }
}

say "";
say "=" x 60;
say "Conversion complete!";
say "Files converted: $converted_count";
say "DRY RUN - no files modified" if $dry_run;
say "";
say "Next steps:";
say "1. Review changes with: git diff";
say "2. Run syntax check: find lib -name '*.pm' -exec perl -c {} \\;";
say "3. Run tests to validate";

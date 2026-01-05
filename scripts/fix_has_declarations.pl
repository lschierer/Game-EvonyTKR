#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use File::Slurp qw(read_file write_file);

my $file = 'lib/Game/EvonyTKR/Role/Constants/BuffConstants.pm';
my $content = read_file($file);

# Pattern: has 'attr' => sub { ... }; or has attr => sub { ... };
# Need to handle multi-line subs

# Strategy: Process line by line, track state
my @lines = split /\n/, $content;
my @output;
my $in_has = 0;
my $has_attr_name = '';
my $brace_count = 0;
my @has_lines;

for my $line (@lines) {
  # Start of has declaration
  if ($line =~ /^(\s+)has\s+'?(\w+)'?\s+=>\s+sub/) {
    $in_has = 1;
    $has_attr_name = $2;
    my $indent = $1;

    # Count opening braces on same line
    my $open = () = $line =~ /{/g;
    my $close = () = $line =~ /}/g;
    $brace_count = $open - $close;

    # Start new has declaration in Moo format
    push @output, "${indent}has $has_attr_name => (";
    push @output, "${indent}  is => 'ro',";
    push @output, "${indent}  lazy => 1,";

    # Check if sub has parameters
    if ($line =~ /sub\s+\(/) {
      push @output, "${indent}  default => " . ($line =~ s/.*?(sub\s+\([^)]*\)\s+{.*)/$1/r);
    } else {
      push @output, "${indent}  default => sub {";
    }

    next;
  }

  # Inside has declaration
  if ($in_has) {
    my $open = () = $line =~ /{/g;
    my $close = () = $line =~ /}/g;
    $brace_count += $open - $close;

    # Check if this closes the sub
    if ($brace_count == 0 && $line =~ /^\s+};/) {
      # Close with Moo format
      push @output, $line =~ s/};$/}/r;
      push @output, $line =~ s/}[^}]*$/  )/r;
      $in_has = 0;
      next;
    }

    push @output, $line;
    next;
  }

  # Normal line
  push @output, $line;
}

write_file($file, join("\n", @output) . "\n");
say "Fixed $file";

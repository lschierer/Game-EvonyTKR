#!/usr/bin/env perl
use v5.40;
use strict;
use warnings;
use Path::Tiny;

# Find all External/*.pm files that emit signals
my @files = path('lib/Game/EvonyTKR/External')->visit(
  sub {
    my ($path) = @_;
    return unless $path->is_file && $path =~ /\.pm$/;

    my $content = $path->slurp_utf8;

    # Skip if already has guard
    return if $content =~ /state \$registered\s*=/;

    # Skip if doesn't emit signal
    return unless $content =~ /plugins->emit/;

    say "Processing: $path";

    # Add state variable after package declaration
    if ($content =~ /(package\s+\S+\s*\{[^\}]*?)(sub\s+task_name)/s) {
      my $before = $1;
      my $after = $2;

      # Check if there are already state variables
      if ($before =~ /\n\s*state\s+/s) {
        # Add after existing state variables
        $content =~ s/(state\s+\$\w+[^;]*;)(?!\s*state)/$1\n  state \$registered = 0;/;
      } else {
        # Add before task_name
        $content =~ s/(sub\s+task_name)/  state \$registered = 0;\n\n  $1/;
      }
    }

    # Add guard at start of register method
    $content =~ s/(sub\s+register\s*\([^)]*\)\s*\{)/$1\n    return if \$registered;  # Prevent multiple registrations\n/;

    # Add $registered = 1 before the closing brace of register method
    # Find the register method and add before its last statement (usually emit or return)
    if ($content =~ /(sub\s+register.*?plugins->emit[^;]*;)/s) {
      $content =~ s/(plugins->emit[^;]*;)/$1\n\n    \$registered = 1;/;
    }

    $path->spew_utf8($content);
    say "  Updated: $path";
  },
  { recurse => 1 }
);

say "Done!";

#!/usr/bin/env perl
use v5.42.0;
use strict;
use warnings;
use File::Find;
use File::Slurp qw(read_file write_file);

my @files;
find(sub {
    push @files, $File::Find::name if /\.pm$/ && -f;
}, 'lib');

my $total_changes = 0;

foreach my $file (@files) {
    my $content = read_file($file);
    my $original = $content;

    # Convert logger method calls to log_ prefix methods
    # Patterns to match various invocant names: $self, $c, $app, $plugin, $job, etc.
    $content =~ s/(\$\w+)->logger->(debug|info|warn|error|fatal|logcroak)\(/$1->log_$2(/g;

    if ($content ne $original) {
        write_file($file, $content);
        my $changes = () = $original =~ /->logger->(debug|info|warn|error|fatal|logcroak)\(/g;
        say "Updated $file ($changes changes)";
        $total_changes += $changes;
    }
}

say "\nTotal: $total_changes logger calls converted across " . scalar(@files) . " files";

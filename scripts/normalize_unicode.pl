#!/usr/bin/env perl
use v5.42.0;
use strict;
use warnings;
use utf8;
use Unicode::Normalize qw(NFC NFD);
use File::Find;
use File::Slurp qw(read_file write_file);
use File::Copy;
use Encode qw(decode encode);

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my @dirs = qw(
  share/collections/data/generals
  share/collections/data/covenants
  share/collections/data/ascending\ attributes
);

my @files_to_process;

foreach my $dir (@dirs) {
    next unless -d $dir;
    find(sub {
        return unless /\.ya?ml$/;
        return unless -f;
        push @files_to_process, $File::Find::name;
    }, $dir);
}

say "Found " . scalar(@files_to_process) . " YAML files to check";

my $renamed_count = 0;
my $content_changed_count = 0;

foreach my $file (@files_to_process) {
    next unless -f $file;  # Skip if file doesn't exist

    # Read content first (before any renaming)
    my $content = eval { read_file($file, binmode => ':utf8') };
    if ($@) {
        warn "  ERROR reading $file: $@\n";
        next;
    }

    my $normalized_content = NFC($content);

    # Check if filename needs normalization
    my $filename = decode('UTF-8', $file, Encode::FB_CROAK);
    my $normalized_filename = NFC($filename);

    my $content_changed = 0;
    my $file_renamed = 0;

    # Update content if needed
    if ($content ne $normalized_content) {
        write_file($file, { binmode => ':utf8' }, $normalized_content);
        say "  Content normalized: $file";
        $content_changed = 1;
        $content_changed_count++;
    }

    # Rename file if needed
    if ($filename ne $normalized_filename) {
        my $new_path = encode('UTF-8', $normalized_filename, Encode::FB_CROAK);
        if (-e $new_path) {
            warn "  WARN: Target file already exists: $new_path\n";
        } else {
            rename($file, $new_path) or die "Failed to rename $file to $new_path: $!";
            say "  File renamed: $file -> $new_path";
            $file_renamed = 1;
            $renamed_count++;
        }
    }

    if (!$content_changed && !$file_renamed) {
        # Check if file has any decomposed characters (even if already normalized)
        if ($content =~ /\p{Mn}/) {
            say "  (Already normalized, had combining marks): $file";
        }
    }
}

say "\nSummary:";
say "  Files with content normalized: $content_changed_count";
say "  Files renamed: $renamed_count";

#!/usr/bin/env perl
use v5.40.0;
use utf8::all;
use File::Slurp qw(read_file append_file edit_file_lines);
use List::AllUtils qw (extract_by first);

my $filename = shift or die "no arg given";
say "$filename";


my @file = read_file($filename);

my @res = (
qr/^.*book:.*$/,
qr/^.*display:.*$/,
);
for my $re (@res) {
  my @match = extract_by {$_ =~ /$re/ } @file;
  edit_file_lines sub {$_ = '' if /$re/ }, $filename;

  foreach my $m (@match) {
    append_file($filename, $m);
  }
}

my %basic_attributes = ();

my @ba_keys = qw(attack attack_increment defense defense_increment leadership leadership_increment);

for my $p (@ba_keys) {
  my $re = qr"^.*$p:(.*)$";
  my $firstLine = first {$_ =~ /$re/ } @file;
  ($basic_attributes{$p}) = $firstLine =~ m/$re/;
  edit_file_lines sub {$_ = '' if /$re/ }, $filename;
}

append_file($filename, "  basic_attributes:\n");
append_file($filename, "    attack:\n");
append_file($filename, "      base: $basic_attributes{'attack'}\n");
append_file($filename, "      increment: $basic_attributes{'attack_increment'}\n")

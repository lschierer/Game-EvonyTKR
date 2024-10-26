#!/usr/bin/env perl
use v5.40.0;
use utf8::all;
use File::Slurp qw(read_file append_file edit_file edit_file_lines);
use List::AllUtils qw (extract_by first);
use YAML::PP;
use YAML::PP::Common qw(PRESERVE_ALL PRESERVE_ORDER );

my $filename = shift or die "no arg given";
say "$filename";

#this should effectively sort the keys.
my $preserveValue = PRESERVE_ALL ^^ PRESERVE_ORDER;
my $ypp = YAML::PP->new(
  indent    => 2,
  boolean   => 'JSON::PP',
  preserve  => $preserveValue,
);

my $yamlData = $ypp->load_file($filename);
$ypp->dump_file($filename, $yamlData);

my @file = read_file($filename);

my $levelRE = qr(^\s+level:.*$);
edit_file_lines sub{$_ = '' if /$levelRE/}, $filename;

edit_file { s/5Red/red5/ } $filename;
edit_file { s/5Purple/purple5/ } $filename;

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

my @ba_keys = qw(
  attack
  attack_increment
  defense
  defense_increment
  leadership
  leadership_increment
  politics
  politics_increment
  );

for my $p (@ba_keys) {
  my $re = qr"^.*$p:(.*)$";
  my $firstLine = first {$_ =~ /$re/ } @file;
  ($basic_attributes{$p}) = $firstLine =~ m/$re/;
  edit_file_lines sub {$_ = '' if /$re/ }, $filename;
}

append_file($filename, "  basic_attributes:\n");
append_file($filename, "    attack:\n");
append_file($filename, "      base: $basic_attributes{'attack'}\n");
append_file($filename, "      increment: $basic_attributes{'attack_increment'}\n");
append_file($filename, "    defense:\n");
append_file($filename, "      base: $basic_attributes{'defense'}\n");
append_file($filename, "      increment: $basic_attributes{'defense_increment'}\n");
append_file($filename, "    leadership:\n");
append_file($filename, "      base: $basic_attributes{'leadership'}\n");
append_file($filename, "      increment: $basic_attributes{'leadership_increment'}\n");
append_file($filename, "    politics:\n");
append_file($filename, "      base: $basic_attributes{'politics'}\n");
append_file($filename, "      increment: $basic_attributes{'politics_increment'}\n");

$yamlData = $ypp->load_file($filename);
$ypp->dump_file($filename, $yamlData);

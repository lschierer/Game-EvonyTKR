#!/usr/bin/env perl
use v5.40.0;
use utf8::all;
use File::Slurp qw(read_file append_file edit_file edit_file_lines);
use List::AllUtils qw (extract_by first);
use YAML::PP;
use YAML::PP::Common qw(PRESERVE_ALL PRESERVE_ORDER );
use Data::Printer;

my $filename = shift or die "no arg given";
say "$filename";

#this should effectively sort the keys.
my $preserveValue = PRESERVE_ALL ^^ PRESERVE_ORDER;
my $ypp = YAML::PP->new(
  indent    => 2,
  boolean   => 'JSON::PP',
  preserve  => $preserveValue,
);

# normalize the data
my $yamlData = $ypp->load_file($filename);
$ypp->dump_file($filename, $yamlData);

for my $level (qw(green blue purple orange gold)) {
  edit_file
    { s/(level: $level)/\L$1\E/i }
      $filename;
}

$yamlData = $ypp->load_file($filename);

for my $level (@{$yamlData->{levels}}) {
  for my $buff (@{$level->{buff}}) {
  my @conditions = ();
    if(exists $buff->{condition}) {
      my @ocs = @{$buff->{condition}};
      for my $oc (@ocs) {
        $oc =~ s/\s+/_/g;
        $oc = lc($oc);
        push @conditions, $oc;
      }
    }
    if(scalar @conditions > 0) {
      $buff->{condition} = [];
      for my $c (@conditions) {
        push @{$buff->{condition}}, $c;
      }
    }
  }
}

$ypp->dump_file($filename, $yamlData);

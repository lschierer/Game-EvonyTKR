#!/usr/bin/env perl
# Regenerates share/test_data/golden_buff_expectations.yaml from the current
# PDL Compiler/Runtime output (matrix level: mask × matrix, no basic-attribute
# buffs). Prints an old-vs-new diff so the values can be domain-reviewed
# before committing. The goldens pin current behavior against regressions;
# they are not an independent oracle.
use v5.42.0;
use utf8;
use strict;
use warnings;
use experimental 'signatures';
use lib 'lib';
use lib '../PAGI-WebServer/lib';

use YAML::XS qw(LoadFile DumpFile);
use Game::EvonyTKR::Service::PDL::Compiler;
use Game::EvonyTKR::Service::PDL::Runtime;

my $golden_path = 'share/test_data/golden_buff_expectations.yaml';
my $golden_data = LoadFile($golden_path);

my $compiler =
  Game::EvonyTKR::Service::PDL::Compiler->new(
  data_dir => 'share/collections/data');
my $runtime =
  Game::EvonyTKR::Service::PDL::Runtime->new(
  data_dir => 'share/collections/data');

my %troop_suffix = (
  'Ground Troops'  => 'ground',
  'Mounted Troops' => 'mounted',
  'Ranged Troops'  => 'ranged',
  'Siege Machines' => 'siege',
);

my $changes = 0;

for my $test_case (@{ $golden_data->{test_cases} }) {
  my $name = $test_case->{name};
  say "=" x 70;
  say $name;

  my $compiled =
    $compiler->compile_general($test_case->{general},
    $test_case->{activationType});
  my $mask = $runtime->build_filter_mask($compiled, $test_case->{filters});
  my $vector = $runtime->matrix_multiply($compiled, $mask);

  my %col_map = map { $compiled->{buff_columns}[$_] => $_ }
    0 .. $#{ $compiled->{buff_columns} };
  my $col = sub ($key) {
    exists $col_map{$key} ? $vector->at($col_map{$key}) : 0;
  };

  for my $troop_type (sort keys %troop_suffix) {
    my $suffix = $troop_suffix{$troop_type};

    my %new_buffs = (
      'March Size' => 0 + $col->('march_size'),
      map { $_ => 0 + ($col->("\L$_\E_${suffix}") + $col->("\L$_\E_all")) }
        ('Attack', 'Defense', 'HP'),
    );
    my %new_debuffs = (
      map {
        $_ => 0 +
          ($col->("enemy_\L$_\E_${suffix}") + $col->("enemy_\L$_\E_all"))
      } ('Attack', 'Defense', 'HP'),
    );

    for my $attr (sort keys %new_buffs) {
      my $old = $test_case->{expected_buffs}{$troop_type}{$attr};
      my $new = $new_buffs{$attr};
      if (!defined $old || $old != $new) {
        printf "  buff   %-16s %-12s %s -> %s\n", $troop_type, $attr,
          ($old // 'undef'), $new;
        $changes++;
      }
    }
    for my $attr (sort keys %new_debuffs) {
      my $old = $test_case->{expected_debuffs}{$troop_type}{$attr};
      my $new = $new_debuffs{$attr};
      if ((($old // 0) != $new)) {
        printf "  debuff %-16s %-12s %s -> %s\n", $troop_type, $attr,
          ($old // 'undef'), $new;
        $changes++;
      }
    }

    $test_case->{expected_buffs}{$troop_type}   = \%new_buffs;
    $test_case->{expected_debuffs}{$troop_type} = \%new_debuffs;
  }
}

DumpFile($golden_path, $golden_data);

# DumpFile drops comments; restore a header explaining provenance.
my $yaml = do { local (@ARGV, $/) = ($golden_path); <> };
my $header = <<'EOF';
# Golden dataset pinning PDL compiler output (matrix level: mask x matrix,
# no basic-attribute buffs) against regressions.
# Regenerate with: perl scripts/regenerate_golden_expectations.pl
# Values reviewed against in-game semantics: 3 generic books per general,
# conflict-aware selection, debuffs stack across sources.
EOF
open my $fh, '>', $golden_path or die $!;
print {$fh} $header, $yaml;
close $fh;

say "=" x 70;
say "$changes value(s) changed; $golden_path updated.";

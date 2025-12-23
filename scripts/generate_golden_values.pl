#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use YAML::XS qw(LoadFile DumpFile);
use Game::EvonyTKR::Service::PDL::Runtime;

my $runtime = Game::EvonyTKR::Service::PDL::Runtime->new(
  data_dir => 'share/collections/data'
);

# Load golden dataset
my $golden_data = LoadFile('share/test_data/golden_buff_expectations.yaml');

for my $test_case (@{$golden_data->{test_cases}}) {
  my $name = $test_case->{name};
  my $general_name = $test_case->{general};
  my $activation = $test_case->{activationType};
  my $filters = $test_case->{filters};

  say "=" x 80;
  say "Test: $name";
  say "=" x 80;

  # Compute actual buffs
  my $summary = $runtime->get_buff_summary(
    general    => $general_name,
    activation => $activation,
    filters    => $filters,
  );

  say "\nBuffs:";
  for my $troop_type (sort keys %{$summary->{buffValues}}) {
    say "  $troop_type:";
    for my $stat (sort keys %{$summary->{buffValues}->{$troop_type}}) {
      my $val = $summary->{buffValues}->{$troop_type}->{$stat};
      say sprintf("    %-15s: %d", $stat, $val);
    }
  }

  say "\nDebuffs:";
  for my $troop_type (sort keys %{$summary->{debuffValues}}) {
    my $has_debuffs = 0;
    for my $stat (keys %{$summary->{debuffValues}->{$troop_type}}) {
      $has_debuffs = 1 if $summary->{debuffValues}->{$troop_type}->{$stat} > 0;
    }
    next unless $has_debuffs;

    say "  $troop_type:";
    for my $stat (sort keys %{$summary->{debuffValues}->{$troop_type}}) {
      my $val = $summary->{debuffValues}->{$troop_type}->{$stat};
      next if $val == 0;
      say sprintf("    %-15s: %d", $stat, $val);
    }
  }

  say "";
}

1;

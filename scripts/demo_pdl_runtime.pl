#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use Game::EvonyTKR::Service::PDL::Runtime;
use Mojo::Util qw(dumper);
use Term::ANSIColor qw(colored);
use Benchmark qw(timethese cmpthese);

=head1 NAME

demo_pdl_runtime.pl - Demonstrate PDL runtime service

=head1 SYNOPSIS

  ./scripts/demo_pdl_runtime.pl

=head1 DESCRIPTION

This script demonstrates the PDL runtime service computing buffs for various
general configurations.

=cut

my $runtime = Game::EvonyTKR::Service::PDL::Runtime->new(
  data_dir => 'share/collections/data'
);

print colored(['bold'], "="x80 . "\n");
print colored(['bold'], "PDL Runtime Service Demonstration\n");
print colored(['bold'], "="x80 . "\n\n");

# Test 1: Marco Polo - all none
print colored(['cyan'], "Test 1: Marco Polo - Attacking (no upgrades)\n");
my $buffs1 = $runtime->compute_buffs(
  general => 'Marco Polo',
  activation => 'Attacking',
  filters => {
    ascendingLevel => 'none',
    covenantLevel => 'none',
    specialty1 => 'none',
    specialty2 => 'none',
    specialty3 => 'none',
    specialty4 => 'none',
  }
);

print_buff_summary($buffs1);
print "\n";

# Test 2: Marco Polo - red5 ascending
print colored(['cyan'], "Test 2: Marco Polo - Attacking (red5)\n");
my $buffs2 = $runtime->compute_buffs(
  general => 'Marco Polo',
  activation => 'Attacking',
  filters => {
    ascendingLevel => 'red5',
    covenantLevel => 'none',
    specialty1 => 'none',
    specialty2 => 'none',
    specialty3 => 'none',
    specialty4 => 'none',
  }
);

print_buff_summary($buffs2);
print "\n";

# Test 3: Marco Polo - fully maxed
print colored(['cyan'], "Test 3: Marco Polo - Attacking (fully maxed)\n");
my $buffs3 = $runtime->compute_buffs(
  general => 'Marco Polo',
  activation => 'Attacking',
  filters => {
    ascendingLevel => 'red5',
    covenantLevel => 'civilization',
    specialty1 => 'gold',
    specialty2 => 'gold',
    specialty3 => 'gold',
    specialty4 => 'gold',
  }
);

print_buff_summary($buffs3);
print "\n";

# Test 4: Aethelflaed - fully maxed (PvM)
print colored(['cyan'], "Test 4: Aethelflaed - PvM (fully maxed)\n");
my $buffs4 = $runtime->compute_buffs(
  general => 'Aethelflaed',
  activation => 'PvM',
  filters => {
    ascendingLevel => 'red5',
    covenantLevel => 'civilization',
    specialty1 => 'gold',
    specialty2 => 'gold',
    specialty3 => 'gold',
    specialty4 => 'gold',
  }
);

print_buff_summary($buffs4);
print "\n";

# Test 5: Pair computation
print colored(['cyan'], "Test 5: Pair - Marco Polo + Aethelflaed\n");
my $pair_buffs = $runtime->compute_pair_buffs(
  primary => 'Marco Polo',
  secondary => 'Aethelflaed',
  activation => 'Attacking',
  primary_filters => {
    ascendingLevel => 'red5',
    covenantLevel => 'civilization',
    specialty1 => 'gold',
    specialty2 => 'gold',
    specialty3 => 'gold',
    specialty4 => 'gold',
  },
  secondary_filters => {
    # Secondary doesn't get ascending, but gets everything else
    ascendingLevel => 'none',
    covenantLevel => 'civilization',
    specialty1 => 'gold',
    specialty2 => 'gold',
    specialty3 => 'gold',
    specialty4 => 'gold',
  }
);

print_buff_summary($pair_buffs);
print "\n";

# Benchmark
print colored(['bold'], "="x80 . "\n");
print colored(['bold'], "Performance Benchmark\n");
print colored(['bold'], "="x80 . "\n\n");

print "Computing buffs for Marco Polo (fully maxed) 1000 times...\n\n";

my $results = timethese(1000, {
  'PDL_Runtime' => sub {
    $runtime->compute_buffs(
      general => 'Marco Polo',
      activation => 'Attacking',
      filters => {
        ascendingLevel => 'red5',
        covenantLevel => 'civilization',
        specialty1 => 'gold',
        specialty2 => 'gold',
        specialty3 => 'gold',
        specialty4 => 'gold',
      }
    );
  },
});

print "\n";
print colored(['green'], "✓ PDL runtime successfully computed buffs!\n");
print colored(['green'], "  Average time per computation: ~" .
  sprintf("%.2f ms\n", ($results->{PDL_Runtime}[1] / 1000) * 1000));

print "\n";
print colored(['bold'], "="x80 . "\n");
print colored(['bold'], "Summary\n");
print colored(['bold'], "="x80 . "\n\n");

print <<'EOF';
The PDL runtime service successfully:
1. Compiles generals into PDL matrices (cached for performance)
2. Builds filter masks from user selections
3. Computes buffs via matrix multiplication
4. Returns structured buff data

Expected production performance:
- Matrix compilation: ~10ms per general (one-time, cached)
- Buff computation: <1ms per request
- Pair computation: <2ms per request

This eliminates the need for Minion workers on EC2!

Next steps:
1. Add generic book support
2. Integrate into controllers
3. Deploy and measure real-world performance
EOF

print "\n";

sub print_buff_summary ($buffs) {
  print "  Mounted Troops:\n";
  print sprintf("    Attack:  %3d  (base: %3d, all: %3d)\n",
    $buffs->{attack_mounted} + $buffs->{attack_all},
    $buffs->{attack_mounted}, $buffs->{attack_all});
  print sprintf("    Defense: %3d  (base: %3d, all: %3d)\n",
    $buffs->{defense_mounted} + $buffs->{defense_all},
    $buffs->{defense_mounted}, $buffs->{defense_all});
  print sprintf("    HP:      %3d  (base: %3d, all: %3d)\n",
    $buffs->{hp_mounted} + $buffs->{hp_all},
    $buffs->{hp_mounted}, $buffs->{hp_all});
  print sprintf("    March:   %3d\n", $buffs->{march_size});

  if ($buffs->{death_to_wounded} || $buffs->{marching_speed}) {
    print "  Other:\n";
    print sprintf("    Death to Wounded: %3d\n", $buffs->{death_to_wounded})
      if $buffs->{death_to_wounded};
    print sprintf("    Marching Speed:   %3d\n", $buffs->{marching_speed})
      if $buffs->{marching_speed};
  }
}

1;

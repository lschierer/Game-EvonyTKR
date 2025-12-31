#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use PDL;
use PDL::NiceSlice;
use Game::EvonyTKR::Service::PDL::Compiler;
use Game::EvonyTKR::Service::PDL::Runtime;
use Mojo::Util qw(dumper);

my $compiler = Game::EvonyTKR::Service::PDL::Compiler->new(
  data_dir => 'share/collections/data'
);

my $runtime = Game::EvonyTKR::Service::PDL::Runtime->new(
  data_dir => 'share/collections/data'
);

# Compile Alessandra for Attacking
say "=" x 80;
say "Compiling Alessandra for Attacking";
say "=" x 80;

my $compiled = $compiler->compile_general('Alessandra', 'Attacking');

say "\nRow labels:";
for my $i (0 .. $#{$compiled->{row_labels}}) {
  say sprintf("  [%2d] %s", $i, $compiled->{row_labels}[$i]);
}

say "\nBuff columns:";
for my $i (0 .. $#{$compiled->{buff_columns}}) {
  say sprintf("  [%2d] %s", $i, $compiled->{buff_columns}[$i]);
}

say "\nMatrix dimensions: " . join(" x ", $compiled->{matrix}->dims);

# Show attack_ground column (index 1)
my $attack_ground_idx = 1;
say "\nattack_ground column (index $attack_ground_idx):";
for my $i (0 .. $#{$compiled->{row_labels}}) {
  my $val = $compiled->{matrix}->at($i, $attack_ground_idx);
  next if $val == 0;
  say sprintf("  [%2d] %-20s = %d", $i, $compiled->{row_labels}[$i], $val);
}

# Now compute with default filters
say "\n" . "=" x 80;
say "Computing with default filters (red5, civilization, all gold)";
say "=" x 80;

my $buffs = $runtime->compute_buffs(
  general    => 'Alessandra',
  activation => 'Attacking',
  filters    => {
    ascendingLevel => 'red5',
    covenantLevel  => 'civilization',
    specialty1     => 'gold',
    specialty2     => 'gold',
    specialty3     => 'gold',
    specialty4     => 'gold',
    generic1       => 'level4',
  },
);

say "\nGround Attack buffs:";
say "  attack_ground: " . $buffs->{attack_ground};
say "  attack_all: " . $buffs->{attack_all};
say "  Total: " . ($buffs->{attack_ground} + $buffs->{attack_all});

say "\nAll buffs:";
for my $key (sort keys %$buffs) {
  next if $buffs->{$key} == 0;
  say sprintf("  %-30s = %d", $key, $buffs->{$key});
}

# Now let's manually trace what should be active
say "\n" . "=" x 80;
say "Manual filter trace";
say "=" x 80;

my $mask = $runtime->build_filter_mask($compiled, {
  ascendingLevel => 'red5',
  covenantLevel  => 'civilization',
  specialty1     => 'gold',
  specialty2     => 'gold',
  specialty3     => 'gold',
  specialty4     => 'gold',
  generic1       => 'level4',
});

say "\nActive rows (mask=1):";
my $mask_flat = $mask->flat;
my $total_attack = 0;
for my $i (0 .. $#{$compiled->{row_labels}}) {
  if ($mask_flat->at($i)) {
    my $attack_val = $compiled->{matrix}->at($i, $attack_ground_idx);
    say sprintf("  [%2d] %-20s attack_ground=%d",
      $i, $compiled->{row_labels}[$i], $attack_val);
    $total_attack += $attack_val;
  }
}
say "\nTotal from active rows: $total_attack";

1;

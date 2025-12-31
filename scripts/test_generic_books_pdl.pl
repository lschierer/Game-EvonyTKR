#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use Game::EvonyTKR::Service::PDL::Compiler;
use Game::EvonyTKR::Service::PDL::Runtime;
use PDL;
use PDL::NiceSlice;

my $compiler = Game::EvonyTKR::Service::PDL::Compiler->new(
  data_dir => 'share/collections/data'
);

my $runtime = Game::EvonyTKR::Service::PDL::Runtime->new(
  data_dir => 'share/collections/data'
);

# Manually populate generic books for testing (since cache may not have been reloaded)
say "Manually populating generic books for Marco Polo and Washington Prime...";
if ($compiler->cache_helper) {
  for my $gen_name ('Marco Polo', 'Washington Prime') {
    my $gen = eval { $compiler->cache_helper->get_general($gen_name) };
    if ($gen && $gen->can('populateGenericBooks')) {
      say "Populating generic books for $gen_name...";
      $gen->populateGenericBooks();

      # Debug: Show what was populated
      my $gbuffs = $gen->genericBookBuffs;
      if ($gbuffs && ref($gbuffs) eq 'HASH') {
        say "  Activations populated: " . join(', ', keys %$gbuffs);
        if (exists $gbuffs->{Attacking}) {
          say "  Attacking levels: " . join(', ', keys %{$gbuffs->{Attacking}});
          if (exists $gbuffs->{Attacking}{level3}) {
            my $l3 = $gbuffs->{Attacking}{level3};
            say "  level3 buffs: " . join(', ', map { "$_=$l3->{$_}" } keys %$l3);
          }
        }
      } else {
        say "  WARNING: genericBookBuffs is empty or not a hash!";
      }
    } else {
      say "Warning: Could not populate generic books for $gen_name";
    }
  }
}

# Test: Marco Polo + Washington Prime (mounted specialists, no conflicts)
# Expected values from built-in books + 6 best generic books:
# - March: 27%
# - Attack: 110%
# - Defense: 65%
# - HP: 105%

say "=" x 80;
say "Testing Marco Polo + Washington Prime Pair";
say "=" x 80;

# Compile both generals
my $mp_compiled = $compiler->compile_general('Marco Polo', 'Attacking');
my $wp_compiled = $compiler->compile_general('Washington Prime', 'Attacking');

# Helper to compute buffs with mask
sub compute_buffs_with_mask {
  my ($compiled, $active_rows) = @_;

  my $row_labels = $compiled->{row_labels};
  my @mask;
  for my $i (0 .. $#{$row_labels}) {
    my $label = $row_labels->[$i];
    push @mask, (grep { $_ eq $label } @$active_rows) ? 1 : 0;
  }

  my $mask_pdl = pdl(@mask);
  my $matrix = $compiled->{matrix};
  my $mask_col = $mask_pdl->reshape($mask_pdl->nelem, 1);
  my $masked_matrix = $matrix * $mask_col;
  return sumover($masked_matrix);
}

say "\nComputing buffs for Marco Polo (book + generic_level6)...";
my $mp_result = compute_buffs_with_mask($mp_compiled, ['book', 'generic_level6']);

say "Computing buffs for Washington Prime (book only, no generics)...";
my $wp_result = compute_buffs_with_mask($wp_compiled, ['book']);

# Add the two PDL vectors
my $total_result = $mp_result + $wp_result;

# Convert to hash
my $columns = $mp_compiled->{buff_columns};
my %buffs;
for my $col_idx (0 .. $#{$columns}) {
  my $col_name = $columns->[$col_idx];
  my $value = $total_result->at($col_idx);
  $buffs{$col_name} = $value if $value != 0;
}

say "\nTotal Pair Buffs (2 built-in books + 6 shared generic books):";
for my $key (sort keys %buffs) {
  say sprintf("  %-25s = %d", $key, $buffs{$key});
}

# Check expected values
my %expected = (
  march_size      => 27,
  attack_mounted  => 110,
  defense_mounted => 65,
  hp_mounted      => 105,
);

say "\n" . "=" x 80;
say "Validation Results:";
say "=" x 80;

my $all_pass = 1;
for my $key (sort keys %expected) {
  my $actual = $buffs{$key} || 0;
  my $expected = $expected{$key};
  my $status = ($actual == $expected) ? "✓ PASS" : "✗ FAIL";

  if ($actual != $expected) {
    $all_pass = 0;
    say sprintf("%-20s Expected: %3d  Actual: %3d  [%s]",
      $key, $expected, $actual, $status);
  } else {
    say sprintf("%-20s Expected: %3d  Actual: %3d  [%s]",
      $key, $expected, $actual, $status);
  }
}

say "\n" . "=" x 80;
if ($all_pass) {
  say "✓ All tests PASSED!";
} else {
  say "✗ Some tests FAILED - values don't match expected";
}
say "=" x 80;

1;

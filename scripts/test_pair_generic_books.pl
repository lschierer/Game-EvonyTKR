#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use Game::EvonyTKR::Model::Base;

my $helper = Game::EvonyTKR::Model::Base->new();

# Test: Get a known pair and check if it has genericBookBuffs
my $pair_key = 'mounted/marco_polo/washington_prime';

say "=" x 80;
say "Testing Pair Generic Book Population";
say "=" x 80;

my $pair = eval { $helper->get_pair($pair_key) };

if ($@) {
  say "Error getting pair: $@";
  exit 1;
}

unless ($pair) {
  say "Pair not found: $pair_key";
  say "This is normal if pairs haven't been precompiled yet.";
  say "Run the prebuild job first to create pairs.";
  exit 0;
}

say "Found pair: " . $pair->primary->name . " + " . $pair->secondary->name;
say "Type: " . $pair->type;

# Check if genericBookBuffs is populated
my $gbuffs = $pair->genericBookBuffs;

if ($gbuffs && ref($gbuffs) eq 'HASH' && keys %$gbuffs) {
  say "\n✓ Pair HAS genericBookBuffs populated!";
  say "Activations: " . join(', ', keys %$gbuffs);

  # Check Attacking activation
  if (exists $gbuffs->{Attacking}) {
    say "\nAttacking levels: " . join(', ', keys %{$gbuffs->{Attacking}});

    # Check level3 (single general's 3 books)
    if (exists $gbuffs->{Attacking}{level3}) {
      my $l3 = $gbuffs->{Attacking}{level3};
      say "\nLevel3 (3 books) buffs:";
      for my $key (sort keys %$l3) {
        say sprintf("  %-25s = %d", $key, $l3->{$key});
      }
    }

    # Check level6 (both generals' 6 books)
    if (exists $gbuffs->{Attacking}{level6}) {
      my $l6 = $gbuffs->{Attacking}{level6};
      say "\nLevel6 (6 books) buffs:";
      for my $key (sort keys %$l6) {
        say sprintf("  %-25s = %d", $key, $l6->{$key});
      }

      # Validate expected values for Marco Polo + Washington Prime
      my %expected = (
        march_size      => 12,
        attack_mounted  => 25,
        hp_mounted      => 25,
      );

      say "\n" . "=" x 80;
      say "Validation (level6 should match test_generic_books_pdl.pl values):";
      say "=" x 80;

      my $all_pass = 1;
      for my $key (sort keys %expected) {
        my $actual = $l6->{$key} || 0;
        my $expected = $expected{$key};
        my $status = ($actual == $expected) ? "✓ PASS" : "✗ FAIL";

        if ($actual != $expected) {
          $all_pass = 0;
        }

        say sprintf("%-20s Expected: %3d  Actual: %3d  [%s]",
          $key, $expected, $actual, $status);
      }

      say "\n" . "=" x 80;
      if ($all_pass) {
        say "✓ All validations PASSED!";
      } else {
        say "✗ Some validations FAILED";
      }
      say "=" x 80;
    }
  }
} else {
  say "\n✗ Pair does NOT have genericBookBuffs populated";
  say "This suggests from_wire_hash is not calling populateGenericBooks()";
}

1;

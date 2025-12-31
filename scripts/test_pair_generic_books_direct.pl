#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use Game::EvonyTKR::Model::Base;
use Game::EvonyTKR::Model::General::Pair;

my $helper = Game::EvonyTKR::Model::Base->new();

say "=" x 80;
say "Testing Pair Generic Book Population (Direct Creation)";
say "=" x 80;

# Get generals from cache
my $marco = $helper->get_general('Marco Polo');
my $washington = $helper->get_general('Washington Prime');

unless ($marco && $washington) {
  say "Error: Could not load generals from cache";
  say "Marco Polo: " . ($marco ? "found" : "NOT FOUND");
  say "Washington Prime: " . ($washington ? "found" : "NOT FOUND");
  exit 1;
}

say "Loaded generals:";
say "  Primary: " . $marco->name;
say "  Secondary: " . $washington->name;

# Create a pair directly
my $pair = Game::EvonyTKR::Model::General::Pair->new(
  primary   => $marco,
  secondary => $washington,
  type      => 'mounted',
);

say "\nCreated pair: " . $pair->primary->name . " + " . $pair->secondary->name;
say "Type: " . $pair->type;

# Manually call populateGenericBooks
say "\nCalling populateGenericBooks()...";
my $result = $pair->populateGenericBooks();

unless ($result) {
  say "✗ populateGenericBooks() returned false";
  exit 1;
}

say "✓ populateGenericBooks() returned true";

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

      # Validate expected values
      # Level6 should have the same values as the generic books themselves
      my %expected = (
        march_size      => 12,
        attack_mounted  => 25,
        hp_mounted      => 25,
      );

      say "\n" . "=" x 80;
      say "Validation Results:";
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
        say "✓ All tests PASSED!";
      } else {
        say "✗ Some tests FAILED";
      }
      say "=" x 80;
    }
  }
} else {
  say "\n✗ Pair does NOT have genericBookBuffs populated";
  say "genericBookBuffs: " . (defined $gbuffs ? ref($gbuffs) : 'undef');
}

1;

#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test::More;
use lib 'lib';
use Log::Log4perl qw(:easy);

# Initialize logging to see debug output
Log::Log4perl->easy_init($DEBUG);

# File::Share requires the main module first
require Game::EvonyTKR;

require_ok('Game::EvonyTKR::Model::Buff');
require_ok('Game::EvonyTKR::Model::Buff::Matcher');
require_ok('Game::EvonyTKR::Model::Buff::Value');

# Create a debuff that should match
my $debuff_value = Game::EvonyTKR::Model::Buff::Value->new(
    number => 10,
    unit   => 'percentage'
);

my $debuff = Game::EvonyTKR::Model::Buff->new(
    attribute        => 'Attack',
    value           => $debuff_value,
    buffConditions  => ['Against Monsters'],  # Debuff has buff conditions
    debuffConditions => ['Enemy'],            # And debuff conditions
    targetedType    => ''
);

diag("=== TESTING DEBUFF MATCHING ===");
diag("Debuff buffConditions: " . join(', ', @{$debuff->buffConditions}));
diag("Debuff debuffConditions: " . join(', ', @{$debuff->debuffConditions}));

# Test the failing case: empty buff conditions
diag("\n=== TEST: Empty buff conditions (should this work?) ===");
my $matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $debuff);

my $result = $matcher->match(
    'Attack',           # test_attribute
    '',                 # test_tt
    [],                 # testBuffs (empty - this is what causes failure)
    ['Enemy'],          # testDebuffs
    'DETAILED_TEST'     # logID
);

if ($result) {
    diag("PASSED: Debuff matched with empty buff conditions");
} else {
    diag("FAILED: Debuff did not match with empty buff conditions");
}

# Test the working case: providing buff conditions
diag("\n=== TEST: With buff conditions (should work) ===");
$result = $matcher->match(
    'Attack',                # test_attribute
    '',                      # test_tt
    ['Against Monsters'],    # testBuffs (matching debuff's buff conditions)
    ['Enemy'],               # testDebuffs
    'DETAILED_TEST_2'        # logID
);

if ($result) {
    diag("PASSED: Debuff matched with matching buff conditions");
} else {
    diag("FAILED: Debuff did not match even with matching buff conditions");
}

done_testing();

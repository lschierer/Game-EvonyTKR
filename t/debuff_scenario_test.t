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

# Test the scenario that works: Specialty debuffs
# Based on the test results, specialty debuffs work correctly
diag("=== TESTING WORKING SCENARIO: Specialty-style debuff ===");

my $working_debuff_value = Game::EvonyTKR::Model::Buff::Value->new(
    number => 10,
    unit   => 'percentage'
);

# Create a debuff that should work like specialty debuffs do
my $working_debuff = Game::EvonyTKR::Model::Buff->new(
    attribute        => 'HP',
    value           => $working_debuff_value,
    buffConditions  => ['Attacking'],     # Simple condition that should match
    debuffConditions => ['Enemy'],        # Standard debuff condition
    targetedType    => 'Ground Troops'
);

diag("Working debuff - buffConditions: " . join(', ', @{$working_debuff->buffConditions}));
diag("Working debuff - debuffConditions: " . join(', ', @{$working_debuff->debuffConditions}));

my $working_matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $working_debuff);

# Test with the conditions that the collection classes should be passing
my $working_result = $working_matcher->match(
    'HP',                # test_attribute
    'Ground Troops',     # test_tt
    ['Attacking'],       # testBuffs (matching the debuff's buff conditions)
    ['Enemy'],           # testDebuffs (matching the debuff's debuff conditions)
    'WORKING_TEST'       # logID
);

if ($working_result) {
    diag("WORKING: Specialty-style debuff matched correctly");
} else {
    diag("BROKEN: Specialty-style debuff failed to match");
}

# Test the scenario that fails: Ascending Attributes debuffs
diag("\n=== TESTING FAILING SCENARIO: Ascending Attributes-style debuff ===");

my $failing_debuff_value = Game::EvonyTKR::Model::Buff::Value->new(
    number => 10,
    unit   => 'percentage'
);

# Create a debuff that might be like ascending attributes debuffs
my $failing_debuff = Game::EvonyTKR::Model::Buff->new(
    attribute        => 'HP',
    value           => $failing_debuff_value,
    buffConditions  => ['Against Monsters'],  # Different condition that might not match
    debuffConditions => ['Enemy'],            # Same debuff condition
    targetedType    => 'Ground Troops'
);

diag("Failing debuff - buffConditions: " . join(', ', @{$failing_debuff->buffConditions}));
diag("Failing debuff - debuffConditions: " . join(', ', @{$failing_debuff->debuffConditions}));

my $failing_matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $failing_debuff);

# Test with the same conditions that work for specialty
my $failing_result = $failing_matcher->match(
    'HP',                # test_attribute
    'Ground Troops',     # test_tt
    ['Attacking'],       # testBuffs (this might not match "Against Monsters")
    ['Enemy'],           # testDebuffs
    'FAILING_TEST'       # logID
);

if ($failing_result) {
    diag("UNEXPECTED: Ascending-style debuff matched");
} else {
    diag("EXPECTED: Ascending-style debuff failed to match (condition mismatch)");
}

# Test with the correct conditions for the failing debuff
my $corrected_result = $failing_matcher->match(
    'HP',                    # test_attribute
    'Ground Troops',         # test_tt
    ['Against Monsters'],    # testBuffs (matching the debuff's actual buff conditions)
    ['Enemy'],               # testDebuffs
    'CORRECTED_TEST'         # logID
);

if ($corrected_result) {
    diag("CORRECTED: Ascending-style debuff matched with correct conditions");
} else {
    diag("STILL BROKEN: Ascending-style debuff failed even with correct conditions");
}

done_testing();

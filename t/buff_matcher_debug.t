#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test::More;
use Test::Deep;
use lib 'lib';

# File::Share requires the main module first
require Game::EvonyTKR;

require_ok('Game::EvonyTKR::Model::Buff');
require_ok('Game::EvonyTKR::Model::Buff::Matcher');
require_ok('Game::EvonyTKR::Model::Buff::Value');

# Test 1: Create a simple buff
my $buff_value = Game::EvonyTKR::Model::Buff::Value->new(
    number => 20,
    unit   => 'percentage'
);

my $buff = Game::EvonyTKR::Model::Buff->new(
    attribute        => 'Attack',
    value           => $buff_value,
    buffConditions  => ['Attacking'],
    debuffConditions => [],
    targetedType    => 'Ground Troops'
);

# Test 2: Create a simple debuff  
my $debuff_value = Game::EvonyTKR::Model::Buff::Value->new(
    number => 10,
    unit   => 'percentage'
);

my $debuff = Game::EvonyTKR::Model::Buff->new(
    attribute        => 'Attack',
    value           => $debuff_value,
    buffConditions  => ['Against Monsters'],
    debuffConditions => ['Enemy'],
    targetedType    => ''
);

# Test 3: Test buff matching
subtest 'Buff Matching' => sub {
    my $matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $buff);
    
    # Should match when looking for buffs
    my $result = $matcher->match(
        'Attack',           # test_attribute
        'Ground Troops',    # test_tt  
        ['Attacking'],      # testBuffs
        [],                 # testDebuffs
        'TEST_BUFF'         # logID
    );
    
    ok($result, 'Buff should match when looking for buffs');
    
    # Should NOT match when looking for debuffs
    $result = $matcher->match(
        'Attack',           # test_attribute
        'Ground Troops',    # test_tt
        [],                 # testBuffs (empty for debuff search)
        ['Enemy'],          # testDebuffs
        'TEST_BUFF_AS_DEBUFF' # logID
    );
    
    ok(!$result, 'Buff should NOT match when looking for debuffs');
};

# Test 4: Test debuff matching
subtest 'Debuff Matching' => sub {
    my $matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $debuff);
    
    # Should NOT match when looking for buffs only
    my $result = $matcher->match(
        'Attack',           # test_attribute
        '',                 # test_tt
        ['Attacking'],      # testBuffs
        [],                 # testDebuffs (empty)
        'TEST_DEBUFF_AS_BUFF' # logID
    );
    
    ok(!$result, 'Debuff should NOT match when looking for buffs only');
    
    # Should match when looking for debuffs
    $result = $matcher->match(
        'Attack',                # test_attribute
        '',                      # test_tt
        ['Against Monsters'],    # testBuffs (debuff's buff conditions)
        ['Enemy'],               # testDebuffs (debuff's debuff conditions)
        'TEST_DEBUFF'            # logID
    );
    
    ok($result, 'Debuff should match when looking for debuffs');
    
    # Test with empty buff conditions (what collection should pass for debuff matching)
    $result = $matcher->match(
        'Attack',           # test_attribute
        '',                 # test_tt
        [],                 # testBuffs (empty for debuff search)
        ['Enemy'],          # testDebuffs
        'TEST_DEBUFF_EMPTY_BUFFS' # logID
    );
    
    ok($result, 'Debuff should match even with empty buff conditions');
};

# Test 5: Debug what conditions are actually set
subtest 'Condition Inspection' => sub {
    diag("Buff conditions: " . join(', ', @{$buff->buffConditions}));
    diag("Buff debuff conditions: " . join(', ', @{$buff->debuffConditions}));
    diag("Debuff conditions: " . join(', ', @{$debuff->buffConditions}));
    diag("Debuff debuff conditions: " . join(', ', @{$debuff->debuffConditions}));
};

done_testing();

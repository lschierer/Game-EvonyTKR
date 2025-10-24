#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test2::V0;
use Test2::Plugin::ExitSummary;

# File::Share requires the main module first
require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;

my $logger = Game::EvonyTKR::Log::Config->logger('Test');
$logger->info('Test script logging configured');

require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Matcher;
require Game::EvonyTKR::Model::Buff::Value;

my $buff_value = Game::EvonyTKR::Model::Buff::Value->new(
  number => 20,
  unit   => 'percentage'
);

my $buff = Game::EvonyTKR::Model::Buff->new(
  attribute        => 'Attack',
  value            => $buff_value,
  buffConditions   => ['Attacking'],
  debuffConditions => [],
  targetedType     => 'Ground Troops'
);

my $debuff_value = Game::EvonyTKR::Model::Buff::Value->new(
  number => 10,
  unit   => 'percentage'
);

my $debuff = Game::EvonyTKR::Model::Buff->new(
  attribute        => 'Attack',
  value            => $debuff_value,
  buffConditions   => ['Against Monsters'],
  debuffConditions => ['Enemy'],
  targetedType     => ''
);
my $empty_debuff = Game::EvonyTKR::Model::Buff->new(
  attribute        => 'Attack',
  value            => $debuff_value,
  buffConditions   => [],
  debuffConditions => ['Enemy'],
  targetedType     => ''
);

diag("Buff conditions: " . join(', ', @{ $buff->buffConditions }));
diag("Buff debuff conditions: " . join(', ', @{ $buff->debuffConditions }));
diag("Debuff conditions: " . join(', ', @{ $debuff->buffConditions }));
diag("Debuff debuff conditions: " . join(', ', @{ $debuff->debuffConditions }));

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
    'Attack',                # test_attribute
    'Ground Troops',         # test_tt
    [],                      # testBuffs (empty for debuff search)
    ['Enemy'],               # testDebuffs
    'TEST_BUFF_AS_DEBUFF'    # logID
  );

  ok(!$result, 'Buff should NOT match when looking for debuffs');
};

subtest 'Debuff Matching' => sub {
  my $matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $debuff);
  my $empty_matcher =
    Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $empty_debuff);

  # Should NOT match when looking for buffs only
  my $result = $matcher->match(
    'Attack',                # test_attribute
    '',                      # test_tt
    ['Attacking'],           # testBuffs
    [],                      # testDebuffs (empty)
    'TEST_DEBUFF_AS_BUFF'    # logID
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
  $result = $empty_matcher->match(
    'Attack',                    # test_attribute
    '',                          # test_tt
    [],                          # testBuffs (empty for debuff search)
    ['Enemy'],                   # testDebuffs
    'TEST_DEBUFF_EMPTY_BUFFS'    # logID
  );

  ok($result, 'Debuff should match even with empty buff conditions');
};

done_testing();

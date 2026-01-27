#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use Test2::V0;
use File::FindLib 'lib';

use Game::EvonyTKR::Loader::Monsters;
use Game::EvonyTKR::Loader::MonsterSimulatorData;
use Game::EvonyTKR::Service::MonsterSimulator;

# Test Monster Loader
subtest 'Monster Loader' => sub {
  my $loader = Game::EvonyTKR::Loader::Monsters->new(
    data_file => 'share/collections/data/monsters/monsters.yaml',
  );

  my $count = $loader->load_all();
  ok($count > 0, "Loaded $count monsters");

  # Test getting a monster by order
  my $robber = $loader->get_by_order(1);
  ok($robber, 'Got monster with order 1');
  is($robber->name, 'Robber', 'Monster name is Robber');
  is($robber->level, 1, 'Monster level is 1');

  # Test search
  my $results = $loader->search('Dragon');
  ok(ref($results) eq 'ARRAY', 'Search returns array ref');

  # Test listing unique names
  my $names = $loader->list_unique_names();
  ok(ref($names) eq 'ARRAY', 'list_unique_names returns array ref');
  ok(scalar(@$names) > 0, 'Has monster names');

  # Test getting levels for a name
  my $levels = $loader->get_levels_for_name('Robber');
  ok(ref($levels) eq 'ARRAY', 'get_levels_for_name returns array ref');
  ok(scalar(@$levels) > 0, 'Robber has multiple levels');
};

# Test Reference Data Loader
subtest 'Monster Simulator Reference Data' => sub {
  my $loader = Game::EvonyTKR::Loader::MonsterSimulatorData->new(
    data_file => 'share/collections/data/monster_simulator/reference_tables.yaml',
  );

  ok($loader->load_all(), 'Loaded reference tables');

  # Test base stats
  my $attack = $loader->get_base_attack('T15', 'mounted');
  ok($attack > 0, "T15 mounted base attack is $attack");

  my $defense = $loader->get_base_defense('T15', 'mounted');
  ok($defense > 0, "T15 mounted base defense is $defense");

  my $hp = $loader->get_base_hp('T15', 'mounted');
  ok($hp > 0, "T15 mounted base HP is $hp");

  # Test tier modifiers
  my $mod = $loader->get_tier_modifier_vs_boss('T15', 'mounted');
  ok($mod > 0, "T15 mounted tier modifier vs boss is $mod");

  # Test world boss modifiers
  my $wb_mod = $loader->get_world_boss_modifier('Lord of Lava', 'ground');
  ok($wb_mod > 0, "Lord of Lava ground modifier is $wb_mod");
};

# Test Simulator Service
subtest 'Monster Simulator Service' => sub {
  my $monsters_loader = Game::EvonyTKR::Loader::Monsters->new(
    data_file => 'share/collections/data/monsters/monsters.yaml',
  );
  $monsters_loader->load_all();

  my $ref_data = Game::EvonyTKR::Loader::MonsterSimulatorData->new(
    data_file => 'share/collections/data/monster_simulator/reference_tables.yaml',
  );
  $ref_data->load_all();

  my $simulator = Game::EvonyTKR::Service::MonsterSimulator->new(
    monsters_loader => $monsters_loader,
    reference_data  => $ref_data,
  );

  # Run a simple simulation
  my $result = $simulator->simulate({
    monster_order => 1,    # Robber Lv1
    tier          => 'T15',
    troop_type    => 'mounted',
    march_type    => 'solo',
    troop_count   => 500_000,
    buffs         => {
      attack => { basic => 0.5 },    # 50% attack buff
    },
  });

  ok(!$result->{error}, 'Simulation completed without error');
  ok($result->{player_stats}, 'Has player stats');
  ok($result->{monster_stats}, 'Has monster stats');
  ok($result->{troop_modifier}, 'Has troop modifier');

  # Verify the calculation makes sense
  # T15 mounted base attack is 7540, with 50% buff = 11310
  my $expected_attack = 7540 * 1.5;
  my $actual_attack   = $result->{player_stats}{attack};
  ok(abs($actual_attack - $expected_attack) < 1,
    "Attack calculation correct: $actual_attack (expected ~$expected_attack)");

  # Test error case
  my $bad_result = $simulator->simulate({
    monster_order => 999999,    # Non-existent
    tier          => 'T15',
    troop_type    => 'mounted',
  });

  ok($bad_result->{error}, 'Returns error for non-existent monster');
};

done_testing();

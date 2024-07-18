package TestsFor::Game::EvonyTKR::General::Siege;
use v5.40.0;

use Test::Most; 
use Test::File::ShareDir::Object::Dist;

use Game::EvonyTKR::General::Siege;
use Game::EvonyTKR::SkillBook::Special;

my $sb = Game::EvonyTKR::SkillBook::Special->new(
  name  => 'Test Book 1'
);

my $g = Game::EvonyTKR::General::Siege->new(
  name                  => 'Test General',
  leadership            => 1.0,
  leadership_increment  => 0.1,
  attack                => 1.0,
  attack_increment      => 0.1,
  defense               => 1.0,
  defense_increment     => 0.1,
  politics              => 1.0,
  politics_increment    => 0.1,
  builtInBook           => $sb,
);
isa_ok($g, 'Game::EvonyTKR::General::Siege');
ok($g->BuffMultipliers->SiegeAttack() == 2.87400, "Correct Siege Attack Multiplier");
ok($g->BuffMultipliers->SiegeRangeIncrease() == 0.5000, "Correct Siege Range Increase Multiplier");

my $ea = $g->effective_attack();
ok($ea == 5.5, "Effective Attack computes");


done_testing();

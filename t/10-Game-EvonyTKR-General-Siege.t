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
);
isa_ok($g, 'Game::EvonyTKR::General::Siege');

done_testing();

package TestsFor::Game::EvonyTKR::SkillBook::Special;
use v5.40.0;

use Test::Most; 
use Test::File::ShareDir::Object::Dist;

use Game::EvonyTKR::SkillBook::Special;

my $sb = Game::EvonyTKR::SkillBook::Special->new(
  name  => 'Test Book 1'
);
isa_ok($sb, 'Game::EvonyTKR::SkillBook::Special');


done_testing();
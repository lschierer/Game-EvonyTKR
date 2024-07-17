package TestsFor::Game::EvonyTKR::SkillBook;
use 5.40.0;

use Test::Most; 
use Test::File::ShareDir::Object::Dist;

use Game::EvonyTKR::SkillBook;

my $sb = Game::EvonyTKR::SkillBook->new(
  name  => 'Test Book 1'
);
isa_ok($sb, 'Game::EvonyTKR::SkillBook');


done_testing();
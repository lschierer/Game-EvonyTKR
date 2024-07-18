package TestsFor::Game::EvonyTKR::General;
use v5.40.0;

use Test::Most; 
use Test::File::ShareDir::Object::Dist;

use Game::EvonyTKR::General;
use Game::EvonyTKR::SkillBook;

my $sb = Game::EvonyTKR::SkillBook->new(
  name  => 'Test Book 1'
);

my $g = Game::EvonyTKR::General->new(
  name                  => 'Test General',
  leadership            => 1.0,
  leadership_increment  => 0.1,
  attack                => 1.0,
  attack_increment      => 0.1,
  defense               => 1.0,
  defense_increment     => 0.1,
  politics              => 1.0,
  politics_increment    => 0.1,
  builtInBook           => $sb
);
isa_ok($g, 'Game::EvonyTKR::General');

my $ea = $g->effective_attack();
ok($ea == 5.5, "Effective Attack computes");



dies_ok (
  sub {
    $g = Game::EvonyTKR::General->new(
      name                  => 'Test General',
      leadership            => -1.0,
      leadership_increment  => 0.1,
      attack                => 1.0,
      attack_increment      => 0.1,
      defense               => 1.0,
      defense_increment     => 0.1,
      politics              => 1.0,
      politics_increment    => 0.1,
      level                 => 50,
      builtInBook           => 10
    );
  }, 'Creating General with invalid level fails'
);

dies_ok (
  sub {
    $g = Game::EvonyTKR::General->new(
      name                  => 'Test General',
      leadership            => -5,
      leadership_increment  => 0.1,
      attack                => 1.0,
      attack_increment      => 0.1,
      defense               => 1.0,
      defense_increment     => 0.1,
      politics              => 1.0,
      politics_increment    => 0.1,
      builtInBook           => $sb
    );
  }, 'Creating General with negative leadership fails'
);

done_testing();

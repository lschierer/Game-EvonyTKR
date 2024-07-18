package TestsFor::Game::EvonyTKR::Buff::Data;
use v5.40.0;

use Test::Most; 
use Test::File::ShareDir::Object::Dist;

use Game::EvonyTKR::Buff::Data;

my $d = Game::EvonyTKR::Buff::Data->new();
isa_ok($d, 'Game::EvonyTKR::Buff::Data');

$d->set_BuffAttributes();
my @attributes = $d->BuffAttributes();
ok(scalar @attributes >= 20, "Attribute Initialization");

my $attack = grep {/^Attack$/} @attributes;
ok($attack == 1, "Attack is in the Attribute list");

done_testing();

package TestsFor::Game::EvonyTKR::Data;
use v5.40.0;
use File::FindLib 'lib';

use Test::Most;
use Test::File::ShareDir::Object::Dist;

use Game::EvonyTKR::Data;

my $d = Game::EvonyTKR::Data->new();
isa_ok($d, 'Game::EvonyTKR::Data');

$d->set_BuffAttributes();
my @attributes = $d->BuffAttributes();
ok(scalar @attributes >= 20, "Attribute Initialization");

my $attack = grep {/^Attack$/} @attributes;
ok($attack == 1, "Attack is in the Attribute list");

done_testing();

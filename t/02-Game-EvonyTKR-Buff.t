package TestsFor::Game::EvonyTKR::Buff;
use 5.40.0;

use Test::Most; 
use Test::File::ShareDir::Object::Dist;
use Game::EvonyTKR::Buff;
use Game::EvonyTKR::Buff::Data;
use Game::EvonyTKR::Buff::Value;

my $obj = Test::File::ShareDir::Object::Dist->new( 
  dists => { "Game-EvonyTKR" => "share/" } 
  );
$obj->install_all_dists;
$obj->register;

my $value = Game::EvonyTKR::Buff::Value->new (
    number  => 15,
    unit    => 'percentage'
  );
my @conditions = ('Attacking');
my $buff = Game::EvonyTKR::Buff->new(
  attribute => 'Attack',
);
isa_ok($buff, 'Game::EvonyTKR::Buff');

$obj->clear;

done_testing;

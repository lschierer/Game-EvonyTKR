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
my $buff = Game::EvonyTKR::Buff->new(
  attribute => 'Attack',
  value => $value
);
isa_ok($buff, 'Game::EvonyTKR::Buff');

$buff->set_condition('Attacking');
ok($buff->has_condition(), "1 Buff Condition Set");

$buff->set_condition('Attacking');
ok(scalar $buff->condition() == 1, "Duplicate Condition rejected");

$buff->set_condition('Enemy');
ok(scalar $buff->condition() == 2, "Second Condition did not overwrite");

$obj->clear;

done_testing;

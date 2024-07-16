use strict;
use warnings;
use v5.38.0;
use Test::Most 'die', tests => 7;
use Test::File::ShareDir::Object::Dist;
use Game::EvonyTKR::Buff;

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
  condition => ('Attacking'),
  value     => $value
);
explain 'I was just created:  ', $buff;
isa_ok($buff, 'Game::EvonyTKR::Buff');

$obj->clear;
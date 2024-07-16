use strict;
use warnings;
use v5.38.0;

use Test::Most 'die', tests => 7; 

use Game::EvonyTKR::Buff::Value;

 my $v = Game::EvonyTKR::Buff::Value->new();
 isa_ok($v, 'Game::EvonyTKR::Buff::Value');

 my $value = Game::EvonyTKR::Buff::Value->new (
    number  => 15,
    unit    => 'percentage'
  );
explain 'I was just created:  ', $value;
isa_ok($value, 'Game::EvonyTKR::Buff::Value');

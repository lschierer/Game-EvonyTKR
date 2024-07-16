use strict;
use warnings;
use v5.38.0;

use Test::More tests => 1;
 
 use Game::EvonyTKR::Buff::Value;

 my $v = Game::EvonyTKR::Buff::Value->new();
 isa_ok($v, 'Game::EvonyTKR::Buff::Value');

 
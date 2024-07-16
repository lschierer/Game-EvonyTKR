package TestsFor::Game::EvonyTKR::Buff::Value;
use 5.40.0;

use Test::Most; 

use Game::EvonyTKR::Buff::Value;

my $v = Game::EvonyTKR::Buff::Value->new();
is $v->number, 0, 'default number is 0';
is $v->unit, 'percentage', 'default unit is percentage';

 my $value = Game::EvonyTKR::Buff::Value->new (
    number  => 15,
    unit    => 'percentage'
  );
isa_ok($value, 'Game::EvonyTKR::Buff::Value');

done_testing;

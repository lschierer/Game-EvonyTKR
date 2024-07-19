package TestsFor::Game::EvonyTKR;
use v5.40.0;
use Test::Most;

BEGIN {
  eval { require Test::Distribution; };
  if ($@) {
    plan skip_all => 'Test::Distribution not installed';
  } else {
    import Test::Distribution;
  }
}

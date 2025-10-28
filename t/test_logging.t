#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test2::V0;
use lib 'lib';

# File::Share requires the main module first
require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;

my $logger = Game::EvonyTKR::Log::Config->logger('Test');
$logger->info('Test script logging configured');

require Game::EvonyTKR::Role::Logger;

package Test::Package {
  use Mojo::Base -base,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;

  sub testMethod ($self) {
    $self->logger->info('Test Script Logger through Role::Logger');
    my $lc = Game::EvonyTKR::Log::Config->logger('Test');
    $lc->info('Test Script Logger through $lc');
    return 1;
  }
}

my $test = Test::Package->new();
ok($test->testMethod(), 'test method successful');
done_testing();

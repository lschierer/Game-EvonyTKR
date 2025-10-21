#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test::More;
use Test::Deep;
use lib 'lib';

# File::Share requires the main module first
require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;

my $logger = Game::EvonyTKR::Log::Config->logger('Test');
$logger->info('Test script logging configured');

require_ok('Game::EvonyTKR::Role::Logger');


package Test::Package {
  use Mojo::Base -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;

  sub testMethod ($self) {
    $self->logger->info('Test Script Logger through Role::Logger');
    my $lc = Game::EvonyTKR::Log::Config->logger('Test');
    $lc->info('Test Script Logger through $lc');
  }
}

my $test = Test::Package->new();
$test->testMethod();

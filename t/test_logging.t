#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use utf8::all;
use experimental qw(class);
use Test2::V0;
use Log::Log4perl::Level;
use Path::Tiny;

# set the environment variables before
# any BEGIN blocks in the modules being tested
BEGIN {
  $ENV{MOJO_MODE} = 'development';
  $ENV{LOG_DEBUG} = 1;

}

require Game::EvonyTKR::Role::Logging;


package Test::Package {
  use Mojo::Base -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;

  sub testMethod ($self) {
    $self->logger->info('Test Script Logger through Role::Logger');
    my $lc = Game::EvonyTKR::Role::Logging::get_logger('Test');
    $lc->info('Test Script Logger through $lc');
    return 1;
  }

}

my $test = Test::Package->new();
ok($test->testMethod(), 'test method successful');
ok($test->debug_log_level() =~ /(?:DEBUG|TRACE)/,
  'Correct log level for package based logger');

my $logger = Game::EvonyTKR::Role::Logging::get_logger('Test::Package');
ok(defined($logger), 'got logger from get_logger');
ok(Log::Log4perl::Level::to_level($logger->level()) =~ /(?:DEBUG|TRACE)/,
  'Correct log level for directly obtained logger');

my $logFile = Game::EvonyTKR::Role::Logging::logFileLocation();
$logFile = sprintf('%s/app-%s.log', $logFile, $$);
$logFile = Path::Tiny::path($logFile);
my $logData = $logFile->slurp_utf8;

say sprintf('log file is at "%s"',  $logFile);

like($logData, qr/Test::Package line \d+/, 'Correct caller in log');

done_testing();

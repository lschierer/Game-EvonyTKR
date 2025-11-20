#!/usr/bin/env perl
use v5.40.0;
use experimental qw(class);
use utf8::all;
use Test::More;
use Test::Deep;
use File::FindLib 'lib';
use Data::Printer;
require Path::Tiny;
require File::ShareDir;
require Game::EvonyTKR;
require Scalar::Util;

# Load required modules
require_ok( 'Game::EvonyTKR::Shared::Constants' );
require_ok( 'Game::EvonyTKR' );

use Log::Log4perl;

Log::Log4perl::init(\<<'EOT');
log4perl.rootLogger              = ERROR, Screen
log4perl.logger.Game.EvonyTKR.Model.Buff = WARN, Screen

log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr = 1
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d [%p] %m%n
EOT

my $constants = Game::EvonyTKR::Shared::Constants->new();

ok(Scalar::Util::blessed($constants) eq 'Game::EvonyTKR::Shared::Constants', 'Constants Object created');

done_testing();

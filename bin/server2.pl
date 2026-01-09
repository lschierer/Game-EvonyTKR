#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use lib 'lib';
use lib '../PAGI-WebServer/lib';

use Game::EvonyTKR;
use Getopt::Long;

# Parse command line options
my $mode = 'development';

GetOptions(
  'mode=s'   => \$mode,
) or die "Error in command line arguments\n";

say "Starting EvonyTKR server in $mode mode...";

unless ($mode =~ /(development|test|production)/) {
  croak("mode must be one of development|test|production, not '$mode'.");
}

my $config_dir   = 'share/conf';
my $local_config = Game::EvonyTKR->getConfig($mode, $config_dir);

Game::EvonyTKR->new(
  initial_config => $local_config,
  env            => $mode,
)->run;

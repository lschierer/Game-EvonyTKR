#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use lib 'lib';
use lib '../PAGI-WebServer/lib';

use Game::EvonyTKR;
use Getopt::Long;
use Path::Tiny qw(path);
use YAML::PP;
use Carp;

# Parse command line options
my $mode = 'development';

GetOptions(
  'mode=s'   => \$mode,
) or die "Error in command line arguments\n";

say "Starting EvonyTKR server in $mode mode...";

unless ($mode =~ /(development|test|production)/) {
  croak("mode must be one of development|test|production, not '$mode'.");
}

# Load config directly (getConfig is an instance method, not class method)
my $config_dir = path('share/conf');
my $config_file = $config_dir->child("$mode.yml");

unless ($config_file->is_file) {
  croak("Config file '$config_file' not found");
}

my $local_config = YAML::PP->new(
  schema       => [qw/ + Perl /],
  yaml_version => ['1.2', '1.1'],
)->load_file($config_file->stringify);

Game::EvonyTKR->new(
  initial_config => $local_config,
  env            => $mode,
)->run;

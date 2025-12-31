use v5.42.0;
use experimental qw(class);
use utf8::all;
use Mojo::File;
require Data::Printer;

require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::External::Common;
use Test2::V0;
use List::AllUtils qw( any none uniq );
use Carp;
use diagnostics;

# Setup logger
my $logger = Game::EvonyTKR::Log::Config->logger();

require Game::EvonyTKR::Model::Covenant;
require Game::EvonyTKR::Role::Constants::Covenants;

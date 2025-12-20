#!/usr/bin/env perl
use v5.40;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';

require Game::EvonyTKR::Shared::Parser;
require Game::EvonyTKR::Role::Logging;

my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);

my $parser = Game::EvonyTKR::Shared::Parser->new();
$parser->generate_grammar();

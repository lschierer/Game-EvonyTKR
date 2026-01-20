#!/usr/bin/env perl
use v5.40;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
use lib '../PAGI-WebServer/lib';

require Game::EvonyTKR::Shared::Parser;
use Log::Handler;

my $logger = Log::Handler->get_logger(__PACKAGE__);

my $parser = Game::EvonyTKR::Shared::Parser->new();
$parser->generate_grammar();

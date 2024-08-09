#!/usr/bin/env perl
# PODNAME: Game::EvonyTKR::Main
# ABSTRACT: Dancer2 REST API wrapper
use v5.40.0;
use utf8::all;
use strict;
use warnings;
use Carp;
use File::ShareDir ':ALL';
use File::Spec;
use File::HomeDir;
use File::Touch;
use Log::Log4perl        qw(get_logger);
use Log::Log4perl::Level ();

require Plack::Loader::Shotgun;
use Plack::Runner;
use namespace::clean;
use base qw(App::Cmd::Simple);
use FindBin;
use lib "$FindBin::Bin/../lib";
use Game::EvonyTKR::Web;


package main {
  Game::EvonyTKR::Web->run;

}

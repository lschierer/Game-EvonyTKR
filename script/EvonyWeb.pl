#!/usr/bin/env perl
use v5.40.0;
use experimental qw(class);
use utf8::all;

package main {
# VERSION
  use Carp;
  use Mojo::File qw(curfile);
  use lib curfile->dirname->sibling('lib')->to_string;
  use Game::EvonyTKR::Logger::Config;
  use Mojolicious::Commands;

  # Start command line interface for application
  Mojolicious::Commands->start_app('Game::EvonyTKR::Web');

  sub getLogDir {
    my $logConf = Game::EvonyTKR::Logger::Config->new();
    my $ld = $logConf->getLogDir(); 
    return $ld;
  }
}

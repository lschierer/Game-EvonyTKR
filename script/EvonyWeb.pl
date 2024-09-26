#!/usr/bin/env perl
use v5.40.0;

package main {
  use Mojo::File qw(curfile);
  use lib curfile->dirname->sibling('lib')->to_string;
  require Game::EvonyTKR::Logger::Config;
  use Mojolicious::Commands;

  # Start command line interface for application
  Mojolicious::Commands->start_app('Game::EvonyTKR::Web');

  sub getLogDir {
    my $ld = Game::EvonyTKR::Logger::Config::getLogDir(); 
    return $ld;
  }
}

#!/usr/bin/env perl
# PODNAME: App::EvonyTKRBackend
# ABSTRACT: Dancer2 REST API wrapper
use v5.40.0;
use utf8::all;
use strict;
use warnings;
use Carp;

require Game::EvonyTKR;

package App::EvonyTKRBackend {
# VERSION
  Game::EvonyTKR->import->execute;
}

__END__

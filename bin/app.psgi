#!/usr/bin/env perl
# PODNAME: Game::EvonyTKR::Main
# ABSTRACT: Dancer2 REST API wrapper

use strict;
use warnings;
use FindBin;
use Log::Log4perl qw(get_logger);
use Log::Log4perl::Level ();
use lib "$FindBin::Bin/../lib";

use Game::EvonyTKR::Logger;

my $logController = Game::EvonyTKR::Logger->new();
my $logger = $logController->logger();
# use this block if you don't need middleware, and only have a single target Dancer app to run here
use Game::EvonyTKR::REST;

$logger->info('Starting Game::EvonyTKR::REST');
Game::EvonyTKR::REST->to_app;

=begin comment
# use this block if you want to include middleware such as Plack::Middleware::Deflater

use Game::EvonyTKR::REST;
use Plack::Builder;

builder {
    enable 'Deflater';
    Game::EvonyTKR::REST->to_app;
}

=end comment

=cut

=begin comment
# use this block if you want to mount several applications on different path

use Game::EvonyTKR::REST;
use Game::EvonyTKR::REST_admin;

use Plack::Builder;

builder {
    mount '/'      => Game::EvonyTKR::REST->to_app;
    mount '/admin'      => Game::EvonyTKR::REST_admin->to_app;
}

=end comment

=cut


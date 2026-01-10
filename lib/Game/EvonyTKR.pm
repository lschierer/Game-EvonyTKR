use v5.42.0;
use utf8::all;
use lib 'lib';
use lib '../PAGI-WebServer/lib';

package Game::EvonyTKR {
  use Mooish::Base -standard;

  with 'WebFramework::Role::Logger';
  extends 'WebFramework::App';

  our $VERSION = 'v0.50.0';

  sub build ($self) {
    $self->logger->info("Building Game::EvonyTKR application v$VERSION");

    # Call parent build
    $self->SUPER::build();

    # Load data loaders first (must run before controllers)
    $self->logger->info("Loading DataLoaders module...");
    $self->load_module('^Game::EvonyTKR::Module::DataLoaders');

    # Load controllers
    $self->logger->info("Loading controllers...");
    $self->load_controller('Specialties');
    $self->load_controller('Books');
    $self->load_controller('AscendingAttributes');  # Helpers only, no routes
    $self->load_controller('Generals');
    $self->load_controller('Covenants');
    $self->load_controller('Root');  # Must be last for catch-all routes

    # Load middleware for static assets
    $self->logger->info("Loading middleware...");
    $self->load_module('Middleware' => {
      Static => {
        root => 'share/public',
        pass_through => 1,
        _order => 2
      },
    });

    $self->logger->info("Game::EvonyTKR application built successfully");
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR - Main Thunderhorse application for EvonyTKR Guide

=head1 DESCRIPTION

Web application providing guides and references for Evony: The King's Return.

This is a Thunderhorse-based rewrite of the original Mojolicious application,
designed for better async performance and lower resource usage.

=head1 VERSION

v0.50.0 - Thunderhorse migration MVP (Specialties, Books, Ascending Attributes, and Single Generals)

=cut

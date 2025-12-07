use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use namespace::autoclean;

package Game::EvonyTKR::Controller::PvP {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::StaticPages', -role;
  use Mojo::Home;
  use Carp;

  sub register ($c, $app, $config = {}) {
    $c->log_info("Registering root landing page route");
    $c->SUPER::register($app, $config);

    my $base = '/PvP';

    $c->static_pages($app, $base);
  }

}

1;
__END__

=pod

=head1 NAME

Game::EvonyTKR::Controller::PvP - Controller for the PvP tree

=head1 DESCRIPTION

Handles the /PvP routes by rendering share/pages/PvP/* as static pages.

=cut

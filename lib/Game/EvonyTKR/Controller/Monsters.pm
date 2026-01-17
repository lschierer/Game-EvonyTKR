use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Monsters {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::StaticPages', -role;
  use Mojo::Home;
  use Carp;

  sub register ($c, $app, $config = {}) {
    $c->logger->info("Registering root landing page route");
    $c->SUPER::register($app, $config);

    my $base = '/Monsters';

    $c->static_pages($app, $base);
  }

}

1;
__END__

=pod

=head1 NAME

Game::EvonyTKR::Controller::Monsters - Controller for the Monsters tree

=head1 DESCRIPTION

Handles the /Monsters routes by rendering share/pages/Monsters/* as static pages.

=cut

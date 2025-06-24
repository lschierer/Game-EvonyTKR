use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use YAML::PP;
use Mojolicious::Plugin::DefaultHelpers;
use namespace::clean;

package Game::EvonyTKR::Controller::Root {
  use Mojo::Base 'Mojolicious::Controller';
  use Mojo::Base 'Mojolicious::Plugin', -signatures, -role;
  use Log::Log4perl;
  use List::Util        qw( any );
  use Mojo::File::Share qw(dist_dir dist_file);
  use Carp;
  our $VERSION = 'v0.02.0';

  my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

  my $base = '/';
  my $routes;

  sub getBase($self) {
    return $base;
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    my @parts = split(/::/, ref($self));
    $base = pop(@parts);
    my $r = $app->routes;

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $base;

    # Set up routes
    $routes = $r->any("/");
    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("index");

    $logger->info("Routes for $base registered successfully");
  }

  sub index($self) {
    $self->app->log->debug(
      'Start of "Game::EvonyTKR::Controller::Root" index handler');
    $self->accepts('html');
    $self->res->headers->cache_control('max-age=1, no-cache');
    $self->render(title => 'Evony TKR Tips',);
  }

};
1;

__END__

#ABSTRACT: Dynamic root page for Schierer Site

=pod

=head1 DESCRIPTION

Dynamically generate the root site based on what home directories we are creating.

=cut

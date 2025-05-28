use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Controller::Specialities;
use namespace::clean;

package Game::EvonyTKR::Plugins::Specialities;
use Mojo::Base 'Mojolicious::Plugin', -signatures;
use Log::Log4perl;

sub register($self, $app, $config = {}) {
  my $r      = $config->{route} || $app->routes;
  my $logger = Log::Log4perl->get_logger(__PACKAGE__);

  # Define routes that point to the controller
  my $routes = $r->any('/Specialities')->to(
    namespace  => 'Game::EvonyTKR::Controller',
    controller => 'Specialities',
  );
  $routes->get('/')->to(action => 'index');
  $routes->get('/details/:name')->to(action => 'show');

  $app->helper(
    get_speciality_details => sub ($c, $name) {
      my $ctrl = Game::EvonyTKR::Controller::Specialities->new;
      return '' unless $ctrl->preShow($c, $name);
      return $c->render_to_string(template => 'specialities/details');
    }
  );

  $logger->debug("Registered Specialities routes");
}

1;

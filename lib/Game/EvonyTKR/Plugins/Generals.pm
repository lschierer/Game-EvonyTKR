package Game::EvonyTKR::Plugins::Generals;
use Mojo::Base 'Mojolicious::Plugin', -signatures;
use Log::Log4perl;

sub register($self, $app, $config = {}) {
  my $r      = $config->{route} || $app->routes;
  my $logger = Log::Log4perl->get_logger(__PACKAGE__);

  # Define routes that point to the controller
  my $routes = $r->any('/Generals')->to(
    namespace  => 'Game::EvonyTKR::Controller',
    controller => 'Generals',
  );
  $routes->get('/')->to(action => 'index');
  $routes->get('/details/:name')->to(action => 'show_general');

  $logger->debug("Registered Generals routes");
}

1;

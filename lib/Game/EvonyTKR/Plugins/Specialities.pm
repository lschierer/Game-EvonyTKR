package Game::EvonyTKR::Plugins::Specialities;
use Mojo::Base 'Mojolicious::Plugin', -signatures;
use Log::Log4perl;

sub register($self, $app, $config = {}) {
    my $r = $config->{route} || $app->routes;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    # Define routes that point to the controller
    my $routes = $r->any('/Specialities')->to(
          namespace  => 'Game::EvonyTKR::Controller',
          controller => 'Specialities',
        );
    $routes->get('/')->to( action => 'index');
    $routes->get('/details/:name')->to( action => 'show');

    $app->helper(
      'specialities.get_details' => sub ($c, $name) {
        # Create a new controller instance
        my $controller =
          $c->app->build_controller($c->req->clone, $c->res->clone);

        # Set the name parameter and no_layout flag
        $controller->param(name => $name);
        $controller->stash(no_layout => 1);

        # Call the show method
        return $controller->show();
      }
    );

    $logger->debug("Registered Specialities routes");
}

1;

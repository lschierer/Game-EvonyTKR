use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

package Game::EvonyTKR::Web::Routes::Generals::Conflicts {
# ABSTRACT: define /general routes
  use Mojo::Base 'Mojolicious::Plugin', -signatures;

  sub register ($self, $app, $r) {

    my $logger = $app->log;

    my $routes = $r->any('/conflicts')->to(
      namespace  => 'Game::EvonyTKR::Web::Controller',
      controller => 'Generals::Conflicts',
    );

    $routes->get('/' => [format => ['html', 'txt', 'json']])->to(
      format => 'html',
      action => 'index'
    );

    $routes->get('/list' => [format => ['html', 'txt', 'json']])->to(
      format => 'txt',
      action => 'list'
    );

    $routes->get('/named/:id' => [format => ['html', 'txt', 'json']])->to(
      format => undef,
      action => 'generalById',
    );

    $logger->trace('Web::Routes::General->register() complete');
  }

}
1;

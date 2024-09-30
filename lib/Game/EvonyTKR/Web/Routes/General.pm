use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

package Game::EvonyTKR::Web::Routes::General {
# ABSTRACT: define /general routes
  use Mojo::Base 'Mojolicious::Plugin', -signatures;

  sub register ($self, $app, $r) {

    my $logger = $app->log;

    my $generalRoutes = $r->any('/general')->to(
      namespace  => 'Game::EvonyTKR::Web::Controller',
      controller => 'General',
    );
    
    $generalRoutes->get('/' => [format => ['html','txt', 'json']])->to(
      format => 'html',
      action => 'index'
      );

    $generalRoutes->get('/list' => [format => ['html','txt', 'json']])->to(
      format => 'txt',
      action => 'list'
      );

    $generalRoutes->get('/named/:id' => [format => ['html','txt', 'json']])->to(
      format => undef,
      action => 'generalById',
    );

    $logger->trace('Web::Routes::General->register() complete');
  }

}
1;

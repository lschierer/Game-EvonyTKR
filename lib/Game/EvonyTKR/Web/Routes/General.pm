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
      namespace   => 'Game::EvonyTKR::Web::Controller',
      controller  => 'General',
      action      => 'list',
    );

    $generalRoutes->get('/')->to(
      action      => 'list'
      );
        
    $generalRoutes->get('/:id' => [format => ['txt', 'json']])->to(
      format      => undef,
      action      => 'generalById',
    );
    
    $logger->trace('Web::Routes::General->register() complete');
  }

}
1;

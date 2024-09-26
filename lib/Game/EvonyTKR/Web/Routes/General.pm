use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

package Game::EvonyTKR::Web::Routes::General {
# ABSTRACT: define /general routes
  use Mojo::Base 'Mojolicious::Plugin', -signatures;

  sub register ($self, $app, $r) {

    my $generalRoutes = $r->any('/general')->to(
      namespace   => 'Game::EvonyTKR::Web::Controller',
      controller  => 'General',
      action      => 'index',
    );

    $generalRoutes->get('/')->to(
      action      => 'index'
      );
        
    $generalRoutes->get('/list')->to(
      action      => 'list'
      );
    
    $generalRoutes->get('/:id' => [format => ['text', 'json']])->to(
      format      => undef,
      action      => 'generalById',
    );
    
  }

}
1;

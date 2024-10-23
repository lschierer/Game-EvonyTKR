use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

package Game::EvonyTKR::Web::Routes::Generals {
# ABSTRACT: define /general routes
  use Mojo::Base 'Mojolicious::Plugin', -signatures;

  sub register ($self, $app, $r) {

    my $logger = $app->log;

    my $generalRoutes = $r->any('/general')->to(
      namespace  => 'Game::EvonyTKR::Web::Controller',
      controller => 'Generals',
    );

    $generalRoutes->get('/' => [format => ['html','txt', 'json']])->to(
      format => 'html',
      action => 'index'
      );

    $generalRoutes->get('/list' => [format => ['html','txt', 'json']])->to(
      format => 'txt',
      action => 'list'
      );

    my $namedID = $generalRoutes->under('/named/'  => sub ($c) {
      my $result = $c->openapi->validate_request($c->req);
      if(!$result) {
        $c->respond_to(
          any => { data  => 'Invalid Request', status => 404 }
        );
      }
    });

    $namedID->get('/:id' => [format => ['html','txt', 'json']])->to(
      format => undef,
      action => 'GetGeneral',
    );

    $logger->trace('Web::Routes::General->register() complete');
  }

}
1;

use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

package Game::EvonyTKR::Web::Routes::Generals {
# ABSTRACT: define /general routes
  use Mojo::Base 'Mojolicious::Plugin', -signatures;

  sub register ($self, $app, $r) {

    my $logger = $app->log;

    my $generalRoutes = $r->any('/generals')->to(
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
      $logger->trace('in generalRoutes namedID under clause');
      my $result = true; #$c->openapi->validate_request($c->req);
      if(!$result) {
        $logger->warn('invalid request recieved',);
        $c->respond_to(
          any => { data  => 'Invalid Request', status => 404 }
        );
      }
      return 1;
    });

    $namedID->get('/:name' => [format => ['html','txt', 'json']])->to(
      format => undef,
      action => 'GetGeneral',
    );

    $logger->trace('Web::Routes::General->register() complete');
  }

}
1;

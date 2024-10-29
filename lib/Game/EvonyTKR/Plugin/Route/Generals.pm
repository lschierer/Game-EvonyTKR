use v5.40.0;
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File qw(curfile);
use Mojo::File::Share qw(dist_dir dist_file);
use File::FindLib 'lib';

package Game::EvonyTKR::Plugin::Route::Generals {
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Carp;

  sub register ($self, $app, $r) {
    my $generalRoutes = $r->any('/generals')->to(
      namespace  => 'Game::EvonyTKR::Controller',
      controller => 'Generals',
    );

    $generalRoutes->get('/' => [format => ['html', 'txt', 'json']])->to(
      format => 'html',
      action => 'list'
    );

  }

}
1;

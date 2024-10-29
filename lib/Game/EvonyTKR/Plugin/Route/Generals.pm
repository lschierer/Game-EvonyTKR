use v5.40.0;
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File qw(curfile);
use File::FindLib 'lib';

package Game::EvonyTKR::Plugin::Route::Generals {
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Carp;

  sub register ($self, $app, $r) {
    $app->routes->get('/generals')->to('Example#welcome');

  }

}
1;

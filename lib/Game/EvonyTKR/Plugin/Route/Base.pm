use v5.40.0;
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File qw(curfile);
use File::FindLib 'lib';

package Game::EvonyTKR::Plugin::Route::Base {
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Carp;

  sub register ($self, $app, $conf) {
    $app->routes->get('/')->to('Example#welcome');
    $app->plugin('Route::Generals');
  }

}
1;

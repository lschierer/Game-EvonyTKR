use v5.40.0;
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File        qw(curfile);
use Mojo::File::Share qw(dist_dir dist_file);
use File::FindLib 'lib';

package Game::EvonyTKR::Plugin::Route::Books {
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Carp;
  our $VERSION = 'v0.30.0';

  sub register ($self, $app, $r) {
    my $generalRoutes = $r->any('/books'=> [format => ['html', 'txt', 'json']])->to(
      namespace   => 'Game::EvonyTKR::Controller',
      controller  => 'Books',
      format      => undef,
    );

    $generalRoutes->get('/' )->to(
      action => 'getAllBooks'
    );

    $generalRoutes->get('/byId/:id')->to(
      action  => 'getABookByUuid',
    );

    $generalRoutes->get('/uuid/:type/:name' )->to(
      action  => 'getUUID',
    );

  }

}
1;

__END__
#ABSTRACT: The Router for the /books based routes

=pod

=head1 DESCRIPTION

I have pulled the route handling for this distribution out into plugins to keep
the main file as simple as possible.  This particular one handles /books based routes.

=cut

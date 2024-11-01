use v5.40.0;
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File        qw(curfile);
use Mojo::File::Share qw(dist_dir dist_file);
use File::FindLib 'lib';

package Game::EvonyTKR::Plugin::Route::Generals {
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Carp;
  our $VERSION = 'v0.30.0';

  sub register ($self, $app, $r) {
    my $generalRoutes = $r->any('/generals'=> [format => ['html', 'txt', 'json']])->to(
      namespace   => 'Game::EvonyTKR::Controller',
      controller  => 'Generals',
      format      => undef,
    );

    $generalRoutes->get('/' )->to(
      action => 'list'
    );

    $generalRoutes->get('/byId/:id')->to(
      action  => 'getGeneralById',
    );

    $generalRoutes->get('/uuid/:type/:name' )->to(
      action  => 'getUUID',
    );

  }

}
1;

__END__
#ABSTRACT: The Router for the /generals based routes

=pod

=head1 DESCRIPTION

I have pulled the route handling for this distribution out into plugins to keep
the main file as simple as possible.  This particular one handles /generals based routes.

=cut

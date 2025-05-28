use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use YAML::PP;
use Mojolicious::Plugin::DefaultHelpers;
use namespace::clean;

package Game::EvonyTKR::Controller::Root {
  use List::Util qw( any );
  use Mojo::Base 'Mojolicious::Controller', -role, -strict, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Carp;
  our $VERSION = 'v0.01.0';

  my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

  sub index($self) {
    $self->app->log->debug(
      'Start of "Game::EvonyTKR::Controller::Root" index handler');
    $self->accepts('html');
    $self->res->headers->cache_control('max-age=1, no-cache');
    $self->render(title => 'Evony TKR Tips',);
  }

};
1;

__END__

#ABSTRACT: Dynamic root page for Schierer Site

=pod

=head1 DESCRIPTION

Dynamically generate the root site based on what home directories we are creating.

=cut

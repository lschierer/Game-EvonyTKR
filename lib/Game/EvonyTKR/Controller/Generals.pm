use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Mojo::Home;
require Data::Printer;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Mojolicious::Controller', -role, -strict, -signatures;
  use HTML::FromANSI qw(ansi2html);
  our $VERSION = 'v0.30.0';

  my $generals = {};

  sub list ($self) {
    my $jsonResponse = {};
    preSeedGenerals($self);
    $self->respond_to(
      txt  => { text => Data::Printer::np($jsonResponse, indent => 2) },
      json => { json => $jsonResponse },
      html => sub {
        $self->render(
          text => ansi2html(
            Data::Printer::np($jsonResponse, indent => 4, colored => 0)
          )
        );
      },
      any => { data => '', status => 204 },
    );
    return;
  }

  sub preSeedGenerals($c) {
    my $home = Mojo::Home->new();
    $home->detect();
    my $generalDir  = $home->child('share')->child('generals');
    my $generalGlob = $generalDir->to_string() . "/*.yaml";
    $c->app()->log()->trace(sprintf('generalGlob pattern is %s', $generalGlob));
    my @generalFiles = glob($generalGlob);
    $c->app()->log()
      ->trace(sprintf('glob returned %d generals', scalar @generalFiles));
    foreach my $filename (@generalFiles) {
      if (-f $filename) {
        $c->app()->log()->trace(sprintf('ready to read %s', $filename));
      }
    }
  }

}
1;

__END__
#ABSTRACT: Controller in a Model-View-Controller take on handling Generals for the Evony TKR Game.

=head1 DESCRIPTION

Mojolicious uses a Model-View-Controller framework, this is a Controller in that framework.
This particular Controller is for handling Generals, which are central to success in the Game Evony TKR,
and the primary reason for this distribution.  This contains methods for listing the
Generals we have information about, finding and displaying the correct Model for a
particular General, and converting a General's name to UUIDv5 id string.

=cut

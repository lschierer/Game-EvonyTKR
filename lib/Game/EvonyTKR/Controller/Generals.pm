use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Mojolicious::Controller', -role, -strict, -signatures;
  use HTML::FromANSI qw(ansi2html);
  our $VERSION = 'v0.30.0';

  my $generals = {};

  sub list ($self) {
    my $jsonResponse = {};
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
}
1;

__END__
#ABSTRACT: Controller in a Model-View-Controller take on handling Generals for the Evony TKR Game.

use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

package Game::EvonyTKR::Web::Controller::General {
# ABSTRACT: handle /general routes
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use Util::Any ':all';
  use Game::EvonyTKR::General;
  use namespace::clean;
  use FindBin;
  use lib "$FindBin::Bin/../../../../../lib";
  use Mojo::Base 'Mojolicious::Controller', -signatures;

  sub index ($self) {
    $self->render(text => "Generals index Page");
  }

  sub list ($self) {
    $self->render(text => "Generals List Pages");
  }

  sub generalById($self) {
    my $id = $self->param('id');
    $self->render(text => "General '$id' requested.");
  }
}
1;

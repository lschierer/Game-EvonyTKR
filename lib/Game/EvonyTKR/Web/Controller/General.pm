use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

package Game::EvonyTKR::Web::Controller::General {
# ABSTRACT: handle /general routes
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::General;
  use namespace::clean;
  use FindBin;
  use lib "$FindBin::Bin/../../../../../lib";
  use Mojo::Base 'Mojolicious::Controller', -signatures;
  use Mojo::JSON qw(decode_json encode_json);

  sub index ($self) {
    $self->render(text => "Generals index Page");
  }

  sub list ($self) {
    $self->render(text => "Generals List Pages");
  }

  sub generalById($self) {
    my $id = $self->param('id');
    $self->log()->trace("looking for $id in generalById");
    
    my $generalShare =
      File::Spec->catfile(
        File::ShareDir::dist_dir('Game-EvonyTKR'), 'generals');
    my $FileWithPath = File::Spec->catfile($generalShare, "$id.yaml");

    if( -T -s -r $FileWithPath) {
      $self->log()->trace("found $id in generalById");
      my $general = Game::EvonyTKR::General->new(
        name  => $id,
      );
      $general->readFromFile();
      my $generalHashRef = $general->toHashRef();

      $self->respond_to(
        json  => {json => $generalHashRef} ,
        any   => { data => '', status => 204},
      );
    }
    else {
      $self->reply->not_found();
    }
    
  }
}
1;

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
  use Game::EvonyTKR::Web::Model::General;
  use namespace::autoclean;
# VERSION
  use FindBin;
  use lib "$FindBin::Bin/../../../../../lib";
  use Mojo::Base 'Mojolicious::Controller', -signatures;
  use Mojo::JSON qw(decode_json encode_json);

  my $generalModel = Game::EvonyTKR::Web::Model::General->new();

  sub index ($self) {
    $self->render(text => "Generals index Page");
  }

  sub list ($self) {
    $self->render(text => "Generals List Pages");
  }


  sub generalById($self) {
    my $id = $self->param('id');
    $self->log()->trace("looking for $id in generalById");
    if(not defined $generalModel) {
      $generalModel = Game::EvonyTKR::Web::Model::General->new();
    }
    my $general = $generalModel->get_by_id($id);

    if(defined $general){
      my $gc = blessed($general);
      my @gcl = split(/::/, $gc);
      if($gcl[2] =~ /general/i) {
        
        my $generalHashRef = $general->toHashRef();
        
        $self->respond_to(
          json  => {json => $generalHashRef} ,
          any   => { data => '', status => 204},
        );
        return;
      }
      else {
        $self->log()->error("general is defined but not a General");
      }
    }
    $self->reply->not_found();
  }
  
}
1;

use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

package Game::EvonyTKR::Web::Controller::General {
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::Data;
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Web::Model::General;
  use namespace::autoclean;

# VERSION
  use File::FindLib 'lib';
  use Mojo::Base 'Mojolicious::Controller', -strict, -signatures;
  use Mojo::JSON qw(decode_json encode_json);

  my $generalModel = Game::EvonyTKR::Web::Model::General->new();

  my $EvonyData = Game::EvonyTKR::Data->new();

  sub list  {
    my ($self) = @_;
    $self->render(text => "Generals List Pages");
  }

  sub _specialityParamHelper($c, $name) {
    my @specialityLevels = qw( None None None None None );

    my $specialityLevelEnum = $EvonyData->specialityLevels();
    my $t = $specialityLevelEnum->compiled_check();
    
    for my $sl (1 .. 4) {
      my $sp = $c->param("specialityLevel$sl");
      if(defined $sp) {
        if( $t->($sp) ) {
          $c->log()->debug(sprintf(
            'setting %s at specialityLevel%d for %s', $sp, $sl, $name));
          @specialityLevels[$sl-1] = $sp;
        }
        else {
          $c->log()->warn(sprintf(
            'invalid specialityLevel %s at %d for %s', $sp, $sl, $name));
          $c->log()->warn(sprintf('valid values are %s',
            Data::Printer::np $specialityLevelEnum->values()));
        }
      }
    }
    $c->log()->debug(sprintf('detected specialityLevels %s',
      Data::Printer::np @specialityLevels));
    return @specialityLevels;
  }

  sub generalById {
    my ($self) = @_;
    $self->log()->trace("get in Controller::General");
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
        $self->log()->trace('general found');
        my $verbose = $self->param('verbose');
        if(defined $verbose and $verbose ne 'false'){
          $verbose = 1;
        }
        else {
          $verbose = 0;
        }
      
        my @specialityLevels = _specialityParamHelper($self, $id,);
        foreach my $i (0 .. $#specialityLevels) {
          my $sp = $specialityLevels[$i];
          my $sl = $i + 1;
          $general->changeActiveSpecialityLevel($sl, $sp);
        }

        my $generalHashRef = $general->toHashRef($verbose);
        
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
    $self->log()->error('no id from Model');
    $self->reply->not_found();
  }
  
}
1;
__END__

# ABSTRACT: Restify controller depicting the REST actions for the /accounts collection.
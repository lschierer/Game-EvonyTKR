use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

class Game::EvonyTKR::Web::Model::General :isa(Game::EvonyTKR::Web::Logger) {
# PODNAME: Game::EvonyTKR::Web::Model::General
# ABSTRACT: store Game::EvonyTKR::General objects in memory for use by Game::EvonyTKR::Web::Controllers
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::General;
  use namespace::autoclean;
# VERSION
  use FindBin;
  use lib "$FindBin::Bin/../../../../../lib";

  field $generals :reader;

  method add_general($ng) {
    my $gc = blessed($ng);
    my @gcl = split(/::/, $gc);
    if($gcl[2] !~ /general/i ) {
      $self->logger()->logwarn("provided object is of type '$gc' not 'Game::EvonyTKR::General '");
      return 0;
    }

    if (not exists($generals->{$ng->name()})) {
      $generals->{$ng->name()} = $ng;
      $generals->{$ng->generalType()}->{$ng->name()} = $ng;
      return 1;
    }
    return 0;
  }

  method remove_general($rg) {
    my $gc = blessed($rg);
    my @gcl = split(/::/, $gc);
    if($gcl[2] !~ /general/i ) {
      $self->logger()->logwarn("provided object is of type '$gc' not 'Game::EvonyTKR::General '");
      return 0;
    }
    delete $generals->{$rg->name()};
    delete $generals->${$rg->generalType()}->{$rg->name()};
  }

  method get_by_id($name) {
    if(exists $generals->{$name}) {
      my $g = $generals->{$name};
      my $gc = blessed $g;
      my @gcl = split(/::/, $gc);
      if($gcl[2] !~ /general/i ) { 
        $self->logger()->logwarn("$name refers to a type of general, not a single general");
        $self->logger()->trace("$name has class $gc");
        $self->logger()->trace(sprintf("g looks like %s", Data::Printer::np $g));
      }
      else {
        $self->logger()->trace("returning cached general for $name");
        return $g;
      }
    }
    else {
      my $generalShare =
      File::Spec->catfile(
        File::ShareDir::dist_dir('Game-EvonyTKR'), 'generals');
      my $FileWithPath = File::Spec->catfile($generalShare, "$name.yaml");

      if( -T -s -r $FileWithPath) {
        $self->logger()->trace("found $name.yaml in get_by_id");
        my $ng = Game::EvonyTKR::General->new(
          name  => $name,
        );
        $ng->readFromFile();
        if($self->add_general($ng)) {
          return $ng;
        }
      }
    }
  }

  method get_all_by_type($type) {
    if(exists $generals->{$type}) {
      my $g = $generals->{$type};
      my $gc = blessed $g;
      my @gcl = split(/::/, $gc);
      if($gcl[2] =~ /general/i ) { 
        $self->logger->logwarn("$type refers to a single of general, not a type of general");
        return {};
      }
      else {
        return $g;
      }
    }
  }

}
1;

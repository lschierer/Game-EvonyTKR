use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

class Game::EvonyTKR::Buff::EvaluationMultipliers :isa(Game::EvonyTKR::Logger) {
  use Carp;
  use Data::Printer;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils qw(is enum);
  use Util::Any -all;
  use Game::EvonyTKR::Buff::Data;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Buff::EvaluationMultipliers

# ABSTRACT: Module for processing information about Evony TKR Generals.

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

  my $classData = Game::EvonyTKR::Buff::Data->new();
  field @BuffAttributes :reader;
  field @BuffConditions :reader;
  field @BuffClasses :reader;

  ADJUST {
    if(scalar @BuffAttributes == 0) {
      $classData->set_BuffAttributes();
      @BuffAttributes = $classData->BuffAttributes();
    }

    if(scalar @BuffConditions == 0) {
      $classData->set_BuffConditions();
      @BuffConditions = $classData->BuffConditions();
    }

    if(scalar @BuffClasses == 0) {
      $classData->set_BuffClasses();
      @BuffClasses = $classData->BuffClasses();
    }
  }

}
1;

__END__

=head1 DESCRIPTION

This base class sets up some generic data that all the sub classes need.
It is useless in and of itself.  You need one of the sub classes. 

=cut

=method new()

auto generated constructor for this class
=cut

=method BuffAttributes()

returns an array of supported attributes that a Game::EvonyTKR::Buff can have
=cut

=method BuffConditions()

returns an array of supported conditions that a Game::EvonyTKR::Buff can have
=cut

=method BuffClasses()

returns an array of supported troop classes that a Game::EvonyTKR::Buff can have
=cut

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

  field @debuffConditions :reader = (
    'Enemy',
    'Enemy_In_City',
    'Reduces_Enemy',
    'Reduces_Enemy_in_Attack',
    'Reduces_Enemy_with_a_Dragon',
    'Reduces Monster',
  );

  method getMultiplierForBuff {
    $self->logcroak("getMultiplierForBuff not implemented, parent called");
  }

  method getMultiplierForDebuff {
    $self->logcroak("getMultiplierForDebuff not implemented, parent called");
  }

  method EvAnsCategory($buff) {
    my %lookup = ();
    if(any {
        my $tc = $_;
        any { $_ =~ /$tc/i} $self->debuffConditions()
      } $buff->condition()) {
        %lookup = (
          'Attack'                      => 'Toughness',
          'Attack Speed'                => 'Unused',
          'Death to Survival'           => 'Attack',
          'Death to Soul'               => 'Attack',
          'Death to Wounded'            => 'Attack',
          'Defense'                     => 'Attack',
          'Deserter Capacity'           => 'Unused',
          'Double Items Drop Rate'      => 'Unused',
          'HP'                          => 'Attack',
          'Healing Speed'               => 'Unused',
          'Hospital Capacity'           => 'Unused',
          'Leadership'                  => 'Attack',
          'Load'                        => 'Unused',
          'March Size Capacity'         => 'Unused',
          'March Time'                  => 'Unused',
          'Marching Speed'              => 'Unused',
          'Marching Speed to Monsters'  => 'Unused',
          'Monster Attack'              => 'Toughness',
          'Politics'                    => 'Attack',
          'Rally Capacity'              => 'Unused',
          'Range'                       => 'Unused',
          'Resources Production'        => 'Unused',
          'Stamina cost'                => 'Unused',
          'SubCity Construction Speed'  => 'Unused',
          'SubCity Gold Production'     => 'Unused',
          'SubCity Training Speed'      => 'Unused',
          'SubCity Troop Capacity'      => 'Unused',
          'Training Capacity'           => 'Unused',
          'Training Speed'              => 'Unused',
          'Wounded to Death'            => 'Attack',
        );
      }
      else {
        %lookup = (
          'Attack'                      => 'Attack',
          'Attack Speed'                => 'Unused',
          'Death to Survival'           => 'Preservation',
          'Death to Soul'               => 'Preservation',
          'Death to Wounded'            => 'Preservation',
          'Defense'                     => 'Toughness',
          'Deserter Capacity'           => 'Unused',
          'Double Items Drop Rate'      => 'Unused',
          'HP'                          => 'Toughness',
          'Healing Speed'               => 'Unused',
          'Hospital Capacity'           => 'Unused',
          'Leadership'                  => 'Toughness',
          'Load'                        => 'Unused',
          'March Size Capacity'         => 'Attack',
          'March Time'                  => 'Unused',
          'Marching Speed'              => 'Unused',
          'Marching Speed to Monsters'  => 'Unused',
          'Monster Attack'              => 'Attack',
          'Politics'                    => 'Toughness',
          'Rally Capacity'              => 'Attack',
          'Range'                       => 'Attack',
          'Resources Production'        => 'Unused',
          'Stamina cost'                => 'Unused',
          'SubCity Construction Speed'  => 'Unused',
          'SubCity Gold Production'     => 'Unused',
          'SubCity Training Speed'      => 'Unused',
          'SubCity Troop Capacity'      => 'Unused',
          'Training Capacity'           => 'Unused',
          'Training Speed'              => 'Unused',
          'Wounded to Death'            => 'Preservation',
        );
      }
      return $lookup{$buff->attribute()};
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

=method debuffConditions()

returns an array of those conditions that are Debuffs rather than Buffs.
=cut

=method getMultiplierForBuff

stub that child classes are required to implement
=cut

=method getMultiplierForDebuff

stub that child classes are required to implement
=cut

=method EvAnsCategory($buff)

$buff must be a Game::EvonyTKR::Buff

returns one of Attack, Toughness, Preservation, or Unused
to correspond with how the EvAns scores use this Buff.

For use in categorizing the results into the 3 broad EvAns categories. 
=cut

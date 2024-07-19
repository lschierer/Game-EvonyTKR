use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Ranged :isa(Game::EvonyTKR::General) {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use namespace::autoclean;
  use Game::EvonyTKR::General;
# PODNAME: Game::EvonyTKR::General::Ranged

# ABSTRACT: Module for processing information about Evony TKR Ranged Specialists.

=head1 DESCRIPTION

=for Out of all Game::EvonyTKR::General instances, some are specialize in enhancing Ranged Troops.  The in-game notation for which Generals specialize 
in which ways is not always entirely reliable, and these modules will, at times, and for particular generals, deviate from it.  Rather, it will rely on the data files provided.  

=cut 

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }
 
  use constant DEFAULT_BUFF_MULTIPLIERS => Game::EvonyTKR::Buff::EvaluationMultipliers->new(
    GenericAttack           => 2.84933,
    RangedAttack            => 2.51600,
    GroundSpeed             => 0,
    MountedSpeed            => 0,
    RangedRangeIncrease     => 0.12000,
    SiegeRangeIncrease      => 0.50000,
    FlatSiegeRangeIncrease  => 0.02500,

    GenericDefense          => 1.25151,
    RangedDefense           => 0.75150,

    GenericHP               => 1.46101,
    RangedHP                => 0.96100,

    GenericAttackDebuff     => 1.63836,
    GroundAttackDebuff      => 0.44400,
    MountedAttackDebuff     => 0.33744,
    RangedAttackDebuff      => 0.28416,
    SiegeAttackDebuff       => 0.39072,
    
    GenericDefenseDebuff    => 1.31069,
    GroundDefenseDebuff     => 0.60000,
    MountedDefenseDebuff    => 0.72000,
    RangedDefenseDebuff     => 0.96000,
    SiegeDefenseDebuff      => 0.84000,

    GenericHPDebuff         => 1.31069,
    GroundHPDebuff          => 0.44400,
    MountedHPDebuff         => 0.33744,
    RangedHPDebuff          => 0.28416,
    SiegeHPDebuff           => 0.39072,
    Death2WoundedDebuff     => 0.12555,
    Death2SoulsDebuff       => 0.12555,
  );

  method is_ground_general() {
    return 0;
  }

  method is_ground_mounted() {
    return 0;
  }

  method is_ranged_general() {
    return 0;
  }

  method is_siege_general() {
    return 1;
  }

  method is_wall_general() {
    return 0;
  }

  method is_mayor() {
    return 0;
  }

  method is_officer() {
    return 0;
  }

}

1;
use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Siege :isa(Game::EvonyTKR::General) {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use namespace::autoclean;
  use Game::EvonyTKR::General;
# PODNAME: Game::EvonyTKR::General::Siege

# ABSTRACT: Module for processing information about Evony TKR Siege Specialists.

=head1 DESCRIPTION

=for Out of all Game::EvonyTKR::General instances, some are specialize in enhancing Siege Machines.  The in-game notation for which Generals specialize 
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
    SiegeAttack             => 2.87400,
    GroundSpeed             => 0,
    MountedSpeed            => 0,
    RangedRangeIncrease     => 0,
    SiegeRangeIncrease      => 0.50000,
    FlatSiegeRangeIncrease  => 0.06000,
    GenericDefense          => 1.10751,
    SiegeDefense            => 0.60750,
    GenericHP               => 1.17501,
    SiegeHP                 => 0.67500,
    GenericAttackDebuff     => 2.06640,
    GroundAttackDebuff      => 0.61600,
    MountedAttackDebuff     => 0.7000,
    RangedAttackDebuff      => 0.44800,
    SiegeAttackDebuff       => 0.53200,
    GenericDefenseDebuff    => 1.63800,
    GroundDefenseDebuff     => 0.35000,
    MountedDefenseDebuff    => 0.42000,
    RangedDefenseDebuff     => 0.49000,
    SiegeDefenseDebuff      => 0.56000,
    GenericHPDebuff         => 1.63800,
    GroundHPDebuff          => 0.35000,
    MountedHPDebuff         => 0.42000,
    RangedHPDebuff          => 0.49000,
    SiegeHPDebuff           => 0.56000,
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
use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Siege :isa(Game::EvonyTKR::General) {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use namespace::autoclean;
  use Game::EvonyTKR::General;
# PODNAME: Game::EvonyTKR::General::Ground

# ABSTRACT: Module for processing information about Evony TKR Ground Specialists.

=head1 DESCRIPTION

=for Out of all Game::EvonyTKR::General instances, some are specialize in enhancing Ground Troops.  The in-game notation for which Generals specialize 
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

  # yes, it is intentional that it is *Ranged* and not *Ground* that is set higher here for Defense and HP. 
  use constant DEFAULT_BUFF_MULTIPLIERS => Game::EvonyTKR::Buff::EvaluationMultipliers->new(
    GroundAttack            => 1.81500,
    GroundSpeed             => 0.60000,
    MountedSpeed            => 0,
    RangedRangeIncrease     => 0,
    SiegeRangeIncrease      => 0.50000,
    FlatSiegeRangeIncrease  => 0.06000,
    GenericDefense          => 1.55391,
    RangedDefense           => 0.60750,
    GenericHP               => 1.67101,
    RangedHP                => 1.05390,
    GenericAttackDebuff     => 3.54240,
    GroundAttackDebuff      => 0.76800,
    MountedAttackDebuff     => 1.20000,
    RangedAttackDebuff      => 1.05600,
    SiegeAttackDebuff       => 0.91200,
    GenericDefenseDebuff    => 2.80800,
    GroundDefenseDebuff     => 0.60000,
    MountedDefenseDebuff    => 0.72000,
    RangedDefenseDebuff     => 0.96000,
    SiegeDefenseDebuff      => 0.84000,
    GenericHPDebuff         => 2.80800,
    GroundHPDebuff          => 0.60000,
    MountedHPDebuff         => 0.72000,
    RangedHPDebuff          => 0.96000,
    SiegeHPDebuff           => 0.84000,
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
use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Ranged :isa(Game::EvonyTKR::General) {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use Class::ISA; 
  use Util::Any -all;
  use namespace::autoclean;
  use Game::EvonyTKR::General;
  use overload 
    '<=>' => \&_comparison,
    'cmp' => \&_comparison,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality;
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

  method is_mounted_general() {
    return 0;
  }

  method is_ranged_general() {
    return 1;
  }

  method is_siege_general() {
    return 0;
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

=method _comparison

This compares on the General's name only currently. 

I can envison doing something based on a computed power score.
=cut
  method _comparison($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList = Class::ISA::self_and_super_path($otherClass);
    if(none {$_ eq 'Game::EvonyTKR::General'} @classList) {
      croak '$other is not a Game::EvonyTKR::General'
    }

    return $self->name() cmp $other->name();
  }

=method _equality

This compares on the General's name and on the bools I set up. 
I am checking the bools because I have at least one general I have purposefully put in twice to eval in two different roles. 

=cut
  method _equality ($other, $swap = 0) { 
    my $otherClass = blessed $other;
    my @classList = Class::ISA::self_and_super_path($otherClass);
    if(none {$_ eq 'Game::EvonyTKR::General'} @classList) {
      croak '$other is not a Game::EvonyTKR::General'
    }
    if($self->name() eq $other->name()) {
      if($self->is_ranged_general() == $other->is_ranged_general()){
        return 1;
      }
    }
    return 0;
  }

=method _inequality

This compares on the General's name and on the bools I set up. 
I am checking the bools because I have at least one general I have purposefully put in twice to eval in two different roles. 
=cut
  method _inequality ($other, $swap = 0) { 
    my $otherClass = blessed $other;
    my @classList = Class::ISA::self_and_super_path($otherClass);
    if(none {$_ eq 'Game::EvonyTKR::General'} @classList) {
      croak '$other is not a Game::EvonyTKR::General'
    }
    if($self->name() eq $other->name()) {
      if($self->is_ranged_general() == $other->is_ranged_general()){
        return 0;
      }
    }
    return 1;
  }
}

1;
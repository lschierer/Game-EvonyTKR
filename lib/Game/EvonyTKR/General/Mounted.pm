use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Mounted :isa(Game::EvonyTKR::General) {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
  use Class::ISA;
  use Util::Any -all;
  use namespace::autoclean;
  use Game::EvonyTKR::General;
  use overload
    '<=>'       => \&_comparison,
    'cmp'       => \&_comparison,
    'eq'        => \&_equality,
    '=='        => \&_equality,
    'ne'        => \&_inequality,
    '!='        => \&_inequality,
    "fallback"  => 1;
# PODNAME: Game::EvonyTKR::General::Mounted

# ABSTRACT: Module for processing information about Evony TKR Mounted Specialists.

=head1 DESCRIPTION

=for Out of all Game::EvonyTKR::General instances, some are specialize in enhancing Mounted Troops.  The in-game notation for which Generals specialize
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
    GenericAttack           => 2.13833,
    MountedAttack           => 1.80500,

    MarchSizeIncrease       => 5.00000,

    GroundSpeed             => 0,
    MountedSpeed            => 0, #currently appears unused by the game
    RangedRangeIncrease     => 0,
    SiegeRangeIncrease      => 0,
    FlatSiegeRangeIncrease  => 0,

    GenericDefense          => 1.36491,
    MountedDefense          => 0.86490,

    GenericHP               => 1.46101,
    MountedHP               => 0.96100,

    GenericAttackDebuff     => 3.54240,
    GroundAttackDebuff      => 0.76800,
    MountedAttackDebuff     => 0.41450,
    RangedAttackDebuff      => 0.66320,
    SiegeAttackDebuff       => 0.91200,

    GenericDefenseDebuff    => 1.72432,
    GroundDefenseDebuff     => 0.58030,
    MountedDefenseDebuff    => 0.72000,
    RangedDefenseDebuff     => 0.96000,
    SiegeDefenseDebuff      => 0.49740,

    GenericHPDebuff         => 1.72432,
    GroundHPDebuff          => 0.58030,
    MountedHPDebuff         => 0.41450,
    RangedHPDebuff          => 0.66320,
    SiegeHPDebuff           => 0.49740,

    Death2WoundedDebuff     => 0.12555,
    Death2SoulsDebuff       => 0.12555,
  );

  method is_ground_general() {
    return 0;
  }

  method is_mounted_general() {
    return 1;
  }

  method is_ranged_general() {
    return 0;
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

=method <=>

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

=method eq

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
      if($self->is_mounted_general() == $other->is_mounted_general()){
        return 1;
      }
    }
    return 0;
  }

=method ne

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
      if($self->is_mounted_general() == $other->is_mounted_general()){
        return 0;
      }
    }
    return 1;
  }
}

1;
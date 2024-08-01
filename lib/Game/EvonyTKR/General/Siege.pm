use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Siege :isa(Game::EvonyTKR::General) {
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
    GenericAttack           => 3.20733,
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

  method is_mounted_general() {
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
      if($self->is_siege_general() == $other->is_siege_general()){
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
      if($self->is_siege_general() == $other->is_siege_general()){
        return 0;
      }
    }
    return 1;
  }
}

1;
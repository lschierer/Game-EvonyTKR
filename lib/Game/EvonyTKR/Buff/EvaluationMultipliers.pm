use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Buff::EvaluationMultipliers {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
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


=attr GenericAttack

the Attack Buff with no conditions or class attribute.
=cut
  field $GenericAttack :reader :param //= 2.14833;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericAttack) or push @errors => "GenericAttack must be a Positive Number, not $GenericAttack";
    my $range = t('NumRange[0, 10]');
    $range->check($GenericAttack) or push @errors => "GenericAttack must be within the range 0-10 inclusive, not $GenericAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundAttack

the Attack Buff with no conditions but with Ground Class attribute.
=cut

  field $GroundAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundAttack) or push @errors => "GroundAttack must be a Positive Number, not $GroundAttack";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundAttack) or push @errors => "GroundAttack must be within the range 0-10 inclusive, not $GroundAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedAttack

the Attack Buff with no conditions but with Mounted Class attribute.
=cut

  field $MountedAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedAttack) or push @errors => "MountedAttack must be a Positive Number, not $MountedAttack";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedAttack) or push @errors => "MountedAttack must be within the range 0-10 inclusive, not $MountedAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedAttack

the Attack Buff with no conditions but with Ranged Class attribute.
=cut

  field $RangedAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedAttack) or push @errors => "RangedAttack must be a Positive Number, not $RangedAttack";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedAttack) or push @errors => "RangedAttack must be within the range 0-10 inclusive, not $RangedAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeAttack

the Attack Buff with no conditions but with Siege Class attribute.
=cut

  field $SiegeAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeAttack) or push @errors => "SiegeAttack must be a Positive Number, not $SiegeAttack";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeAttack) or push @errors => "SiegeAttack must be within the range 0-10 inclusive, not $SiegeAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GenericDefense

the Defense Buff with no conditions or class attribute.
=cut
  field $GenericDefense :reader :param //= 1.55391;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericDefense) or push @errors => "GenericDefense must be a Positive Number, not $GenericDefense";
    my $range = t('NumRange[0, 10]');
    $range->check($GenericDefense) or push @errors => "GenericDefense must be within the range 0-10 inclusive, not $GenericDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundDefense

the Defense Buff with no conditions but with Ground Class attribute.
=cut

  field $GroundDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundDefense) or push @errors => "GroundDefense must be a Positive Number, not $GroundDefense";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundDefense) or push @errors => "GroundDefense must be within the range 0-10 inclusive, not $GroundDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedDefense

the Defense Buff with no conditions but with Mounted Class attribute.
=cut

  field $MountedDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedDefense) or push @errors => "MountedDefense must be a Positive Number, not $MountedDefense";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedDefense) or push @errors => "MountedDefense must be within the range 0-10 inclusive, not $MountedDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedDefense

the Defense Buff with no conditions but with Ranged Class attribute.
=cut

  field $RangedDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedDefense) or push @errors => "RangedDefense must be a Positive Number, not $RangedDefense";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedDefense) or push @errors => "RangedDefense must be within the range 0-10 inclusive, not $RangedDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeDefense

the Defense Buff with no conditions but with Siege Class attribute.
=cut

  field $SiegeDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeDefense) or push @errors => "SiegeDefense must be a Positive Number, not $SiegeDefense";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeDefense) or push @errors => "SiegeDefense must be within the range 0-10 inclusive, not $SiegeDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GenericHP

the HP Buff with no conditions or class attribute.
=cut
  field $GenericHP :reader :param //= 1.67101;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericHP) or push @errors => "GenericHP must be a Positive Number, not $GenericHP";
    my $range = t('NumRange[0, 10]');
    $range->check($GenericHP) or push @errors => "GenericHP must be within the range 0-10 inclusive, not $GenericHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundHP

the HP Buff with no conditions but with Ground Class attribute.
=cut

  field $GroundHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundHP) or push @errors => "GroundHP must be a Positive Number, not $GroundHP";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundHP) or push @errors => "GroundHP must be within the range 0-10 inclusive, not $GroundHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedHP

the HP Buff with no conditions but with Mounted Class attribute.
=cut

  field $MountedHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedHP) or push @errors => "MountedHP must be a Positive Number, not $MountedHP";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedHP) or push @errors => "MountedHP must be within the range 0-10 inclusive, not $MountedHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedHP

the HP Buff with no conditions but with Ranged Class attribute.
=cut

  field $RangedHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedHP) or push @errors => "RangedHP must be a Positive Number, not $RangedHP";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedHP) or push @errors => "RangedHP must be within the range 0-10 inclusive, not $RangedHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeHP

the HP Buff with no conditions but with Siege Class attribute.
=cut

  field $SiegeHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeHP) or push @errors => "SiegeHP must be a Positive Number, not $SiegeHP";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeHP) or push @errors => "SiegeHP must be within the range 0-10 inclusive, not $SiegeHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundSpeed

the Ground (In-Battle Movement) Speed Buff with no conditions.
This defaults to zero because we do not know the troop type yet.
=cut

  field $GroundSpeed :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundSpeed) or push @errors => "GroundSpeed must be a Positive Number, not $GroundSpeed";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundSpeed) or push @errors => "GroundSpeed must be within the range 0-10 inclusive, not $GroundSpeed";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedSpeed

the Mounted (In-Battle Movement) Speed Buff with no conditions.
This defaults to zero because we do not know the troop type yet.
=cut

  field $MountedSpeed :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedSpeed) or push @errors => "MountedSpeed must be a Positive Number, not $MountedSpeed";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedSpeed) or push @errors => "MountedSpeed must be within the range 0-10 inclusive, not $MountedSpeed";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MarchSizeIncrease

the March Size Increase Buff with no conditions.
=cut

  field $MarchSizeIncrease :reader :param //= 2.66667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MarchSizeIncrease) or push @errors => "MarchSizeIncrease must be a Positive Number, not $MarchSizeIncrease";
    my $range = t('NumRange[0, 10]');
    $range->check($MarchSizeIncrease) or push @errors => "MarchSizeIncrease must be within the range 0-10 inclusive, not $MarchSizeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RallySizeIncrease

the Rally Size Increase Buff with no conditions.
=cut

  field $RallySizeIncrease :reader :param //= 0.50000;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RallySizeIncrease) or push @errors => "RallySizeIncrease must be a Positive Number, not $RallySizeIncrease";
    my $range = t('NumRange[0, 10]');
    $range->check($RallySizeIncrease) or push @errors => "RallySizeIncrease must be within the range 0-10 inclusive, not $RallySizeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedRangeIncrease

The Ranged Range Increase Buff with no conditions (in other words, how far away can a ranged troop hit something).
This defaults to zero because we do not know the troop type yet.
=cut

  field $RangedRangeIncrease :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedRangeIncrease) or push @errors => "RangedRangeIncrease must be a Positive Number, not $RangedRangeIncrease";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedRangeIncrease) or push @errors => "RangedRangeIncrease must be within the range 0-10 inclusive, not $RangedRangeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr FlatSiegeRangeIncrease

There are two versions of this buff, this one is for flat increases.
The Flat Siege Range Increase Buff with no conditions (in other words, how far away can a Siege Machine hit something).
This defaults to zero because we do not know the troop type yet.

=cut

  field $FlatSiegeRangeIncrease :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($FlatSiegeRangeIncrease) or push @errors => "FlatSiegeRangeIncrease must be a Positive Number, not $FlatSiegeRangeIncrease";
    my $range = t('NumRange[0, 10]');
    $range->check($FlatSiegeRangeIncrease) or push @errors => "FlatSiegeRangeIncrease must be within the range 0-10 inclusive, not $FlatSiegeRangeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeRangeIncrease

There are two versions of this buff, this one is for percentage increases.
The Siege Range Increase Buff with no conditions (in other words, how far away can a Siege Machine hit something).
This defaults to zero because we do not know the troop type yet.

=cut

  field $SiegeRangeIncrease :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeRangeIncrease) or push @errors => "SiegeRangeIncrease must be a Positive Number, not $SiegeRangeIncrease";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeRangeIncrease) or push @errors => "SiegeRangeIncrease must be within the range 0-10 inclusive, not $SiegeRangeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }


=attr Death2Wounded

the Troop Death into Wounded Buff with no conditions.
The original at EvonyAnswers splits this into two, one for when there is
an "Attacking" condition, and one with twice the value when there isn't.
I am using the value from the "Attacking" version, because you will not actually get twice the value of the other in any _single_ senario, he values it more becuase it is used in more situations.  I am handling that with subclasses instead.
=cut

  field $Death2Wounded :reader :param //= 0.12555;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2Wounded) or push @errors => "Death2Wounded must be a Positive Number, not $Death2Wounded";
    my $range = t('NumRange[0, 10]');
    $range->check($Death2Wounded) or push @errors => "Death2Wounded must be within the range 0-10 inclusive, not $Death2Wounded";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr Death2Souls

the Troop Death into Souls Buff with no conditions.
=cut

  field $Death2Souls :reader :param //= 0.12555;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2Souls) or push @errors => "Death2Souls must be a Positive Number, not $Death2Souls";
    my $range = t('NumRange[0, 10]');
    $range->check($Death2Souls) or push @errors => "Death2Souls must be within the range 0-10 inclusive, not $Death2Souls";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }


=head3 Debuffs

=attr GenericAttackDebuff

the AttackDebuff with no conditions or class attribute.
=cut
  field $GenericAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericAttackDebuff) or push @errors => "GenericAttackDebuff must be a Positive Number, not $GenericAttackDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($GenericAttackDebuff) or push @errors => "GenericAttackDebuff must be within the range 0-10 inclusive, not $GenericAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundAttackDebuff

the AttackDebuff with no conditions but with Ground Class attribute.
=cut

  field $GroundAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundAttackDebuff) or push @errors => "GroundAttackDebuff must be a Positive Number, not $GroundAttackDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundAttackDebuff) or push @errors => "GroundAttackDebuff must be within the range 0-10 inclusive, not $GroundAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedAttackDebuff

the AttackDebuff with no conditions but with Mounted Class attribute.
=cut

  field $MountedAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedAttackDebuff) or push @errors => "MountedAttackDebuff must be a Positive Number, not $MountedAttackDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedAttackDebuff) or push @errors => "MountedAttackDebuff must be within the range 0-10 inclusive, not $MountedAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedAttackDebuff

the AttackDebuffDebuff with no conditions but with Ranged Class attribute.
=cut

  field $RangedAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedAttackDebuff) or push @errors => "RangedAttackDebuff must be a Positive Number, not $RangedAttackDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedAttackDebuff) or push @errors => "RangedAttackDebuff must be within the range 0-10 inclusive, not $RangedAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeAttackDebuff

the AttackDebuffDebuff with no conditions but with Siege Class attribute.
=cut

  field $SiegeAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeAttackDebuff) or push @errors => "SiegeAttackDebuff must be a Positive Number, not $SiegeAttackDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeAttackDebuff) or push @errors => "SiegeAttackDebuff must be within the range 0-10 inclusive, not $SiegeAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GenericDefenseDebuff

the DefenseDebuff with no conditions or class attribute.
=cut
  field $GenericDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericDefenseDebuff) or push @errors => "GenericDefenseDebuff must be a Positive Number, not $GenericDefenseDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($GenericDefenseDebuff) or push @errors => "GenericDefenseDebuff must be within the range 0-10 inclusive, not $GenericDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundDefenseDebuff

the DefenseDebuff with no conditions but with Ground Class attribute.
=cut

  field $GroundDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundDefenseDebuff) or push @errors => "GroundDefenseDebuff must be a Positive Number, not $GroundDefenseDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundDefenseDebuff) or push @errors => "GroundDefenseDebuff must be within the range 0-10 inclusive, not $GroundDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedDefenseDebuff

the DefenseDebuff with no conditions but with Mounted Class attribute.
=cut

  field $MountedDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedDefenseDebuff) or push @errors => "MountedDefenseDebuff must be a Positive Number, not $MountedDefenseDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedDefenseDebuff) or push @errors => "MountedDefenseDebuff must be within the range 0-10 inclusive, not $MountedDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedDefenseDebuff

the DefenseDebuff with no conditions but with Ranged Class attribute.
=cut

  field $RangedDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedDefenseDebuff) or push @errors => "RangedDefenseDebuff must be a Positive Number, not $RangedDefenseDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedDefenseDebuff) or push @errors => "RangedDefenseDebuff must be within the range 0-10 inclusive, not $RangedDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeDefenseDebuff

the DefenseDebuff with no conditions but with Siege Class attribute.
=cut

  field $SiegeDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeDefenseDebuff) or push @errors => "SiegeDefenseDebuff must be a Positive Number, not $SiegeDefenseDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeDefenseDebuff) or push @errors => "SiegeDefenseDebuff must be within the range 0-10 inclusive, not $SiegeDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GenericHPDebuff

the HPDebuff with no conditions or class attribute.
=cut
  field $GenericHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericHPDebuff) or push @errors => "GenericHPDebuff must be a Positive Number, not $GenericHPDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($GenericHPDebuff) or push @errors => "GenericHPDebuff must be within the range 0-10 inclusive, not $GenericHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr GroundHPDebuff

the HPDebuff with no conditions but with Ground Class attribute.
=cut

  field $GroundHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundHPDebuff) or push @errors => "GroundHPDebuff must be a Positive Number, not $GroundHPDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($GroundHPDebuff) or push @errors => "GroundHPDebuff must be within the range 0-10 inclusive, not $GroundHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr MountedHPDebuff

the HPDebuff with no conditions but with Mounted Class attribute.
=cut

  field $MountedHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedHPDebuff) or push @errors => "MountedHPDebuff must be a Positive Number, not $MountedHPDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($MountedHPDebuff) or push @errors => "MountedHPDebuff must be within the range 0-10 inclusive, not $MountedHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr RangedHPDebuff

the HPDebuff with no conditions but with Ranged Class attribute.
=cut

  field $RangedHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedHPDebuff) or push @errors => "RangedHPDebuff must be a Positive Number, not $RangedHPDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($RangedHPDebuff) or push @errors => "RangedHPDebuff must be within the range 0-10 inclusive, not $RangedHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr SiegeHPDebuff

the HPDebuff with no conditions but with Siege Class attribute.
=cut

  field $SiegeHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeHPDebuff) or push @errors => "SiegeHPDebuff must be a Positive Number, not $SiegeHPDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($SiegeHPDebuff) or push @errors => "SiegeHPDebuff must be within the range 0-10 inclusive, not $SiegeHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr Death2WoundedDebuff

the Troop Death into Wounded Debuff with no conditions.
=cut

  field $Death2WoundedDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2WoundedDebuff) or push @errors => "Death2WoundedDebuff must be a Positive Number, not $Death2WoundedDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($Death2WoundedDebuff) or push @errors => "Death2WoundedDebuff must be within the range 0-10 inclusive, not $Death2WoundedDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=attr Death2SoulsDebuff

the Troop Death into Souls Debuff with no conditions.
=cut

  field $Death2SoulsDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2SoulsDebuff) or push @errors => "Death2SoulsDebuff must be a Positive Number, not $Death2SoulsDebuff";
    my $range = t('NumRange[0, 10]');
    $range->check($Death2SoulsDebuff) or push @errors => "Death2SoulsDebuff must be within the range 0-10 inclusive, not $Death2SoulsDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }
}
1;

__END__

=head1 DESCRIPTION

For now a Game::EvonyTKR::General has-a (set of) Game::EvonyTKR::Buff::EvaluationMultipliers rather than :does Game::EvonyTKR::Buff:Evaluation which is what I wanted.  This design choice was forced because roles are not yet implemented in perl Corinna.

This is intended as an abstract class, but again, those are not implemented in Corinna yet.  I'm not sure if this will save me time or cost me time as a result.

Default values come from https://www.evonyanswers.com/post/evony-answers-attribute-methodology-explanation as of 2024-07-18, except the
defaults for the debuffs, which I do intend to use from that site, but cannot apply here, because they are totally custom per troop type to account for the
in-game biases between troops of different classes.

This value has the generic versions, or what would happen if you had generals that had no conditions on their debuffs.  That is, the general does I<not> say any of

=for :list

* when attacking

* when reinforcing

* when the main defense general

or any of the variety of other things that get read in as conditions by Game::EvonyTKR::Buff - except that it I<does> have entries for generic debuffs, with, as I said, bogus values.

While EvonyAnswers chose to organize this into Objects for Offensive, Toughness, Troop Preservation, and the corresponding Debuffs, I have gone with a flatter list to allow for greater flexibility as some of these do not apply at all to certain situations, and to take advantage of the currently limited state of perl Corinna.

I recognize that the flat list of attributes is an anti-pattern, and that a hash is typically recommended, but with this pattern, I can take advantage of Corinna's built in type checking and method generation capabilities to the maximal extent currently implemented.

=cut

=method new()

auto generated constructor for this class
=cut

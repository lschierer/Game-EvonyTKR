use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Buff::EvaluationMultipliers::Mayor {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Buff::EvaluationMultipliers::Mayor

# ABSTRACT: Game::EvonyTKR::Buff Evaluation Multipliers for Mayor use cases.

=head1 SYNOPSIS

=over

=item comment Brief examples of using the module.

=back

=head1 DESCRIPTION

See Game::EvonyTKR::Buff::EvaluationMultipliers for a general description.

This provides the base overrides for the Mayor use cases. 

I have left values in place for most of the buffs on the theory that this
helps keep the Mayor's troops alive, and thus keeps the Mayor's debuffs in play.   However, since you only need one troop of any type for a debuff to be effective, you do not care *which* troop class that one troop is from, and thus one troop buff is just as good as another. A generic buff is still slightly better than a specific buff in that it helps keep three (Sub Cities do not have siege) troops alive instead of just one.  Siege buffs are unfortunately worthless.

=cut 

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }
  
=head1 Attributes 

=over

=item GenericAttack
 
the Attack Buff with no conditions or class attribute. 
=cut 
  field $GenericAttack :reader  = 0.33333;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericAttack) or push @errors => "GenericAttack must be a Positive Number, not $GenericAttack";
    my $range = t('IntRange[0, 10]');
    $range->check($GenericAttack) or push @errors => "GenericAttack must be within the range 0-10 inclusive, not $GenericAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeAttack
 
the Attack Buff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeAttack :reader = 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeAttack) or push @errors => "SiegeAttack must be a Positive Number, not $SiegeAttack";
    my $range = t('IntRange[0, 10]');
    $range->check($SiegeAttack) or push @errors => "SiegeAttack must be within the range 0-10 inclusive, not $SiegeAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }


=item GenericDefense
 
the Defense Buff with no conditions or class attribute. 
=cut 
  field $GenericDefense :reader  = 0.66668;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericDefense) or push @errors => "GenericDefense must be a Positive Number, not $GenericDefense";
    my $range = t('IntRange[0, 10]');
    $range->check($GenericDefense) or push @errors => "GenericDefense must be within the range 0-10 inclusive, not $GenericDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeDefense
 
the Defense Buff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeDefense :reader = 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeDefense) or push @errors => "SiegeDefense must be a Positive Number, not $SiegeDefense";
    my $range = t('IntRange[0, 10]');
    $range->check($SiegeDefense) or push @errors => "SiegeDefense must be within the range 0-10 inclusive, not $SiegeDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericHP
 
the HP Buff with no conditions or class attribute. 
=cut 
  field $GenericHP :reader  = 0.66668;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericHP) or push @errors => "GenericHP must be a Positive Number, not $GenericHP";
    my $range = t('IntRange[0, 10]');
    $range->check($GenericHP) or push @errors => "GenericHP must be within the range 0-10 inclusive, not $GenericHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeHP
 
the HP Buff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeHP :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeHP) or push @errors => "SiegeHP must be a Positive Number, not $SiegeHP";
    my $range = t('IntRange[0, 10]');
    $range->check($SiegeHP) or push @errors => "SiegeHP must be within the range 0-10 inclusive, not $SiegeHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MarchSizeIncrease 

the March Size Increase Buff with no conditions. 
=cut 

  field $MarchSizeIncrease :reader = 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MarchSizeIncrease) or push @errors => "MarchSizeIncrease must be a Positive Number, not $MarchSizeIncrease";
    my $range = t('IntRange[0, 10]');
    $range->check($MarchSizeIncrease) or push @errors => "MarchSizeIncrease must be within the range 0-10 inclusive, not $MarchSizeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RallySizeIncrease 

the Rally Size Increase Buff with no conditions. 
=cut 

  field $RallySizeIncrease :reader = 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RallySizeIncrease) or push @errors => "RallySizeIncrease must be a Positive Number, not $RallySizeIncrease";
    my $range = t('IntRange[0, 10]');
    $range->check($RallySizeIncrease) or push @errors => "RallySizeIncrease must be within the range 0-10 inclusive, not $RallySizeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

##### Debuffs 

=item GenericAttackDebuff
 
the AttackDebuff with no conditions or class attribute. 
=cut 
  field $GenericAttackDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericAttackDebuff) or push @errors => "GenericAttackDebuff must be a Positive Number, not $GenericAttackDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($GenericAttackDebuff) or push @errors => "GenericAttackDebuff must be within the range 0-10 inclusive, not $GenericAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundAttackDebuff
 
the AttackDebuff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundAttackDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundAttackDebuff) or push @errors => "GroundAttackDebuff must be a Positive Number, not $GroundAttackDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($GroundAttackDebuff) or push @errors => "GroundAttackDebuff must be within the range 0-10 inclusive, not $GroundAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedAttackDebuff
 
the AttackDebuff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedAttackDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedAttackDebuff) or push @errors => "MountedAttackDebuff must be a Positive Number, not $MountedAttackDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($MountedAttackDebuff) or push @errors => "MountedAttackDebuff must be within the range 0-10 inclusive, not $MountedAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedAttackDebuff
 
the AttackDebuffDebuff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedAttackDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedAttackDebuff) or push @errors => "RangedAttackDebuff must be a Positive Number, not $RangedAttackDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($RangedAttackDebuff) or push @errors => "RangedAttackDebuff must be within the range 0-10 inclusive, not $RangedAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeAttackDebuff
 
the AttackDebuffDebuff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeAttackDebuff :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeAttackDebuff) or push @errors => "SiegeAttackDebuff must be a Positive Number, not $SiegeAttackDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($SiegeAttackDebuff) or push @errors => "SiegeAttackDebuff must be within the range 0-10 inclusive, not $SiegeAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericDefenseDebuff
 
the DefenseDebuff with no conditions or class attribute. 
=cut 
  field $GenericDefenseDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericDefenseDebuff) or push @errors => "GenericDefenseDebuff must be a Positive Number, not $GenericDefenseDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($GenericDefenseDebuff) or push @errors => "GenericDefenseDebuff must be within the range 0-10 inclusive, not $GenericDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundDefenseDebuff
 
the DefenseDebuff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundDefenseDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundDefenseDebuff) or push @errors => "GroundDefenseDebuff must be a Positive Number, not $GroundDefenseDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($GroundDefenseDebuff) or push @errors => "GroundDefenseDebuff must be within the range 0-10 inclusive, not $GroundDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedDefenseDebuff
 
the DefenseDebuff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedDefenseDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedDefenseDebuff) or push @errors => "MountedDefenseDebuff must be a Positive Number, not $MountedDefenseDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($MountedDefenseDebuff) or push @errors => "MountedDefenseDebuff must be within the range 0-10 inclusive, not $MountedDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedDefenseDebuff
 
the DefenseDebuff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedDefenseDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedDefenseDebuff) or push @errors => "RangedDefenseDebuff must be a Positive Number, not $RangedDefenseDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($RangedDefenseDebuff) or push @errors => "RangedDefenseDebuff must be within the range 0-10 inclusive, not $RangedDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeDefenseDebuff
 
the DefenseDebuff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeDefenseDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeDefenseDebuff) or push @errors => "SiegeDefenseDebuff must be a Positive Number, not $SiegeDefenseDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($SiegeDefenseDebuff) or push @errors => "SiegeDefenseDebuff must be within the range 0-10 inclusive, not $SiegeDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericHPDebuff
 
the HPDebuff with no conditions or class attribute. 
=cut 
  field $GenericHPDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericHPDebuff) or push @errors => "GenericHPDebuff must be a Positive Number, not $GenericHPDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($GenericHPDebuff) or push @errors => "GenericHPDebuff must be within the range 0-10 inclusive, not $GenericHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundHPDebuff
 
the HPDebuff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundHPDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundHPDebuff) or push @errors => "GroundHPDebuff must be a Positive Number, not $GroundHPDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($GroundHPDebuff) or push @errors => "GroundHPDebuff must be within the range 0-10 inclusive, not $GroundHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedHPDebuff
 
the HPDebuff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedHPDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedHPDebuff) or push @errors => "MountedHPDebuff must be a Positive Number, not $MountedHPDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($MountedHPDebuff) or push @errors => "MountedHPDebuff must be within the range 0-10 inclusive, not $MountedHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedHPDebuff
 
the HPDebuff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedHPDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedHPDebuff) or push @errors => "RangedHPDebuff must be a Positive Number, not $RangedHPDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($RangedHPDebuff) or push @errors => "RangedHPDebuff must be within the range 0-10 inclusive, not $RangedHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeHPDebuff
 
the HPDebuff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeHPDebuff :reader :param //= 2;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeHPDebuff) or push @errors => "SiegeHPDebuff must be a Positive Number, not $SiegeHPDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($SiegeHPDebuff) or push @errors => "SiegeHPDebuff must be within the range 0-10 inclusive, not $SiegeHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item Death2WoundedDebuff 

the Troop Death into Wounded Debuff with no conditions. 
=cut 

  field $Death2WoundedDebuff :reader :param //= 1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2WoundedDebuff) or push @errors => "Death2WoundedDebuff must be a Positive Number, not $Death2WoundedDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($Death2WoundedDebuff) or push @errors => "Death2WoundedDebuff must be within the range 0-10 inclusive, not $Death2WoundedDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item Death2SoulsDebuff 

the Troop Death into Souls Debuff with no conditions. 
=cut 

  field $Death2SoulsDebuff :reader :param //= 1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2SoulsDebuff) or push @errors => "Death2SoulsDebuff must be a Positive Number, not $Death2SoulsDebuff";
    my $range = t('IntRange[0, 10]');
    $range->check($Death2SoulsDebuff) or push @errors => "Death2SoulsDebuff must be within the range 0-10 inclusive, not $Death2SoulsDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=back

=cut
}
1;

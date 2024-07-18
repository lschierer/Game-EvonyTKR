use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Buff::EvaluationMultipliers {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Buff::EvaluationMultipliers

# ABSTRACT: Module for processing information about Evony TKR Generals.

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for This ought to be a role, but that is not yet implemented in perl Corinna.

For now a Game::EvonyTKR::General has-a (set of) Game::EvonyTKR::Buff::EvaluationMultipliers rather than :does Game::EvonyTKR::Buff:Evaluation which is what I wanted.  

This is intended as an abstract class, but again, those are not implemented in Corinna yet.  I'm not sure if this will save me time or cost me time as a result. 

Default values come from https://www.evonyanswers.com/post/evony-answers-attribute-methodology-explanation as of 2024-07-18, except the
defaults for the debuffs, which I do intend to use from that site, but cannot apply here, because they are totally custom per troop type to account for the 
in-game biases between troops of different classes.

This value has the generic versions, or what would happen if you had generals that had no conditions on their debuffs.  That is, the general does I<not> say any of 

=over

=item * when attacking

=item * when reinforcing

=item * when the main defense general

=back

or any of the variety of other things that get read in as conditions by Game::EvonyTKR::Buff - except that it I<does> have entries for generic debuffs, with, as I said, bogus values. 

While EvonyAnswers chose to organize this into Objects for Offensive, Toughness, Troop Preservation, and the corresponding Debuffs, I have gone with a flatter list to allow for greater flexibility as some of these do not apply at all to certain situations, and to take advantage of the currently limited state of perl Corinna. 

I recognize that the flat list of attributes is an anti-pattern, and that a hash is typically recommended, but with this pattern, I can take advantage of Corinna's built in type checking and method generation capabilities to the maximal extent currently implemented. 

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

=item GenericAttack 
the Attack Buff with no conditions or class attribute. 
=cut 
  field $GenericAttack :reader :param //= 2.14833;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericAttack) or push @errors => "GenericAttack must be a Positive Number, not $GenericAttack";
    $range = t('IntRange[0, 10]');
    $range->check($GenericAttack) or push @errors => "GenericAttack must be within the range 0-10 inclusive, not $GenericAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundAttack 
the Attack Buff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundAttack) or push @errors => "GroundAttack must be a Positive Number, not $GroundAttack";
    $range = t('IntRange[0, 10]');
    $range->check($GroundAttack) or push @errors => "GroundAttack must be within the range 0-10 inclusive, not $GroundAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedAttack 
the Attack Buff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedAttack) or push @errors => "MountedAttack must be a Positive Number, not $MountedAttack";
    $range = t('IntRange[0, 10]');
    $range->check($MountedAttack) or push @errors => "MountedAttack must be within the range 0-10 inclusive, not $MountedAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedAttack 
the Attack Buff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedAttack) or push @errors => "RangedAttack must be a Positive Number, not $RangedAttack";
    $range = t('IntRange[0, 10]');
    $range->check($RangedAttack) or push @errors => "RangedAttack must be within the range 0-10 inclusive, not $RangedAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeAttack 
the Attack Buff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeAttack :reader :param //= 0.11111;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeAttack) or push @errors => "SiegeAttack must be a Positive Number, not $SiegeAttack";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeAttack) or push @errors => "SiegeAttack must be within the range 0-10 inclusive, not $SiegeAttack";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericDefense 
the Defense Buff with no conditions or class attribute. 
=cut 
  field $GenericDefense :reader :param //= 1.55391;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericDefense) or push @errors => "GenericDefense must be a Positive Number, not $GenericDefense";
    $range = t('IntRange[0, 10]');
    $range->check($GenericDefense) or push @errors => "GenericDefense must be within the range 0-10 inclusive, not $GenericDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundDefense 
the Defense Buff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundDefense) or push @errors => "GroundDefense must be a Positive Number, not $GroundDefense";
    $range = t('IntRange[0, 10]');
    $range->check($GroundDefense) or push @errors => "GroundDefense must be within the range 0-10 inclusive, not $GroundDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedDefense 
the Defense Buff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedDefense) or push @errors => "MountedDefense must be a Positive Number, not $MountedDefense";
    $range = t('IntRange[0, 10]');
    $range->check($MountedDefense) or push @errors => "MountedDefense must be within the range 0-10 inclusive, not $MountedDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedDefense 
the Defense Buff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedDefense) or push @errors => "RangedDefense must be a Positive Number, not $RangedDefense";
    $range = t('IntRange[0, 10]');
    $range->check($RangedDefense) or push @errors => "RangedDefense must be within the range 0-10 inclusive, not $RangedDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeDefense 
the Defense Buff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeDefense :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeDefense) or push @errors => "SiegeDefense must be a Positive Number, not $SiegeDefense";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeDefense) or push @errors => "SiegeDefense must be within the range 0-10 inclusive, not $SiegeDefense";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericHP 
the HP Buff with no conditions or class attribute. 
=cut 
  field $GenericHP :reader :param //= 1.67101;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericHP) or push @errors => "GenericHP must be a Positive Number, not $GenericHP";
    $range = t('IntRange[0, 10]');
    $range->check($GenericHP) or push @errors => "GenericHP must be within the range 0-10 inclusive, not $GenericHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundHP 
the HP Buff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundHP) or push @errors => "GroundHP must be a Positive Number, not $GroundHP";
    $range = t('IntRange[0, 10]');
    $range->check($GroundHP) or push @errors => "GroundHP must be within the range 0-10 inclusive, not $GroundHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedHP 
the HP Buff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedHP) or push @errors => "MountedHP must be a Positive Number, not $MountedHP";
    $range = t('IntRange[0, 10]');
    $range->check($MountedHP) or push @errors => "MountedHP must be within the range 0-10 inclusive, not $MountedHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedHP 
the HP Buff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedHP) or push @errors => "RangedHP must be a Positive Number, not $RangedHP";
    $range = t('IntRange[0, 10]');
    $range->check($RangedHP) or push @errors => "RangedHP must be within the range 0-10 inclusive, not $RangedHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeHP 
the HP Buff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeHP :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeHP) or push @errors => "SiegeHP must be a Positive Number, not $SiegeHP";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeHP) or push @errors => "SiegeHP must be within the range 0-10 inclusive, not $SiegeHP";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundSpeed 
the Ground (In-Battle Movement) Speed Buff with no conditions. 
=cut 

  field $GroundSpeed :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundSpeed) or push @errors => "GroundSpeed must be a Positive Number, not $GroundSpeed";
    $range = t('IntRange[0, 10]');
    $range->check($GroundSpeed) or push @errors => "GroundSpeed must be within the range 0-10 inclusive, not $GroundSpeed";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedSpeed 
the Mounted (In-Battle Movement) Speed Buff with no conditions. 
=cut 

  field $MountedSpeed :reader :param //= 0.16667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedSpeed) or push @errors => "MountedSpeed must be a Positive Number, not $MountedSpeed";
    $range = t('IntRange[0, 10]');
    $range->check($MountedSpeed) or push @errors => "MountedSpeed must be within the range 0-10 inclusive, not $MountedSpeed";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MarchSizeIncrease 
the March Size Increase Buff with no conditions. 
=cut 

  field $MarchSizeIncrease :reader :param //= 2.66667;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MarchSizeIncrease) or push @errors => "MarchSizeIncrease must be a Positive Number, not $MarchSizeIncrease";
    $range = t('IntRange[0, 10]');
    $range->check($MarchSizeIncrease) or push @errors => "MarchSizeIncrease must be within the range 0-10 inclusive, not $MarchSizeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RallySizeIncrease 
the Rally Size Increase Buff with no conditions. 
=cut 

  field $RallySizeIncrease :reader :param //= 0.50000;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RallySizeIncrease) or push @errors => "RallySizeIncrease must be a Positive Number, not $RallySizeIncrease";
    $range = t('IntRange[0, 10]');
    $range->check($RallySizeIncrease) or push @errors => "RallySizeIncrease must be within the range 0-10 inclusive, not $RallySizeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedRangeIncrease 
The Ranged Range Increase Buff with no conditions (in other words, how far away can a ranged troop hit something).
This defaults to zero with the assumption that everyone is appropriately using the skill books and armor, and thus it cancels out.  I dislike that, as it would lead an uninformed player to disvalue the books and armor and get slaughtered. 
=cut 

  field $RangedRangeIncrease :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedRangeIncrease) or push @errors => "RangedRangeIncrease must be a Positive Number, not $RangedRangeIncrease";
    $range = t('IntRange[0, 10]');
    $range->check($RangedRangeIncrease) or push @errors => "RangedRangeIncrease must be within the range 0-10 inclusive, not $RangedRangeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeRangeIncrease 
The Siege Range Increase Buff with no conditions (in other words, how far away can a Siege Machine hit something).
This defaults to zero with the assumption that everyone is appropriately using the skill books and armor, and thus it cancels out.  I dislike that, as it would lead an uninformed player to disvalue the books and armor and get slaughtered. 
=cut 

  field $SiegeRangeIncrease :reader :param //= 0;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeRangeIncrease) or push @errors => "SiegeRangeIncrease must be a Positive Number, not $SiegeRangeIncrease";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeRangeIncrease) or push @errors => "SiegeRangeIncrease must be within the range 0-10 inclusive, not $SiegeRangeIncrease";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item Death2Wounded 
the Troop Death into Wounded Buff with no conditions. 
=cut 

  field $Death2Wounded :reader :param //= 0.25555;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2Wounded) or push @errors => "Death2Wounded must be a Positive Number, not $Death2Wounded";
    $range = t('IntRange[0, 10]');
    $range->check($Death2Wounded) or push @errors => "Death2Wounded must be within the range 0-10 inclusive, not $Death2Wounded";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item Death2Souls 
the Troop Death into Souls Buff with no conditions. 
=cut 

  field $Death2Souls :reader :param //= 0.12555;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2Souls) or push @errors => "Death2Souls must be a Positive Number, not $Death2Souls";
    $range = t('IntRange[0, 10]');
    $range->check($Death2Souls) or push @errors => "Death2Souls must be within the range 0-10 inclusive, not $Death2Souls";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }


##### Debuffs 

=item GenericAttackDebuff 
the AttackDebuff with no conditions or class attribute. 
=cut 
  field $GenericAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericAttackDebuff) or push @errors => "GenericAttackDebuff must be a Positive Number, not $GenericAttackDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($GenericAttackDebuff) or push @errors => "GenericAttackDebuff must be within the range 0-10 inclusive, not $GenericAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundAttackDebuff 
the AttackDebuff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundAttackDebuff) or push @errors => "GroundAttackDebuff must be a Positive Number, not $GroundAttackDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($GroundAttackDebuff) or push @errors => "GroundAttackDebuff must be within the range 0-10 inclusive, not $GroundAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedAttackDebuff 
the AttackDebuff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedAttackDebuff) or push @errors => "MountedAttackDebuff must be a Positive Number, not $MountedAttackDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($MountedAttackDebuff) or push @errors => "MountedAttackDebuff must be within the range 0-10 inclusive, not $MountedAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedAttackDebuff 
the AttackDebuffDebuff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedAttackDebuff) or push @errors => "RangedAttackDebuff must be a Positive Number, not $RangedAttackDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($RangedAttackDebuff) or push @errors => "RangedAttackDebuff must be within the range 0-10 inclusive, not $RangedAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeAttackDebuff 
the AttackDebuffDebuff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeAttackDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeAttackDebuff) or push @errors => "SiegeAttackDebuff must be a Positive Number, not $SiegeAttackDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeAttackDebuff) or push @errors => "SiegeAttackDebuff must be within the range 0-10 inclusive, not $SiegeAttackDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericDefenseDebuff 
the DefenseDebuff with no conditions or class attribute. 
=cut 
  field $GenericDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericDefenseDebuff) or push @errors => "GenericDefenseDebuff must be a Positive Number, not $GenericDefenseDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($GenericDefenseDebuff) or push @errors => "GenericDefenseDebuff must be within the range 0-10 inclusive, not $GenericDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundDefenseDebuff 
the DefenseDebuff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundDefenseDebuff) or push @errors => "GroundDefenseDebuff must be a Positive Number, not $GroundDefenseDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($GroundDefenseDebuff) or push @errors => "GroundDefenseDebuff must be within the range 0-10 inclusive, not $GroundDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedDefenseDebuff 
the DefenseDebuff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedDefenseDebuff) or push @errors => "MountedDefenseDebuff must be a Positive Number, not $MountedDefenseDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($MountedDefenseDebuff) or push @errors => "MountedDefenseDebuff must be within the range 0-10 inclusive, not $MountedDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedDefenseDebuff 
the DefenseDebuff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedDefenseDebuff) or push @errors => "RangedDefenseDebuff must be a Positive Number, not $RangedDefenseDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($RangedDefenseDebuff) or push @errors => "RangedDefenseDebuff must be within the range 0-10 inclusive, not $RangedDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeDefenseDebuff 
the DefenseDebuff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeDefenseDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeDefenseDebuff) or push @errors => "SiegeDefenseDebuff must be a Positive Number, not $SiegeDefenseDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeDefenseDebuff) or push @errors => "SiegeDefenseDebuff must be within the range 0-10 inclusive, not $SiegeDefenseDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GenericHPDebuff 
the HPDebuff with no conditions or class attribute. 
=cut 
  field $GenericHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GenericHPDebuff) or push @errors => "GenericHPDebuff must be a Positive Number, not $GenericHPDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($GenericHPDebuff) or push @errors => "GenericHPDebuff must be within the range 0-10 inclusive, not $GenericHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item GroundHPDebuff 
the HPDebuff with no conditions but with Ground Class attribute. 
=cut 
  
  field $GroundHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($GroundHPDebuff) or push @errors => "GroundHPDebuff must be a Positive Number, not $GroundHPDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($GroundHPDebuff) or push @errors => "GroundHPDebuff must be within the range 0-10 inclusive, not $GroundHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item MountedHPDebuff 
the HPDebuff with no conditions but with Mounted Class attribute. 
=cut 

  field $MountedHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($MountedHPDebuff) or push @errors => "MountedHPDebuff must be a Positive Number, not $MountedHPDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($MountedHPDebuff) or push @errors => "MountedHPDebuff must be within the range 0-10 inclusive, not $MountedHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item RangedHPDebuff 
the HPDebuff with no conditions but with Ranged Class attribute. 
=cut 

  field $RangedHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($RangedHPDebuff) or push @errors => "RangedHPDebuff must be a Positive Number, not $RangedHPDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($RangedHPDebuff) or push @errors => "RangedHPDebuff must be within the range 0-10 inclusive, not $RangedHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item SiegeHPDebuff 
the HPDebuff with no conditions but with Siege Class attribute. 
=cut 

  field $SiegeHPDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($SiegeHPDebuff) or push @errors => "SiegeHPDebuff must be a Positive Number, not $SiegeHPDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($SiegeHPDebuff) or push @errors => "SiegeHPDebuff must be within the range 0-10 inclusive, not $SiegeHPDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item Death2WoundedDebuff 
the Troop Death into Wounded Debuff with no conditions. 
=cut 

  field $Death2WoundedDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2WoundedDebuff) or push @errors => "Death2WoundedDebuff must be a Positive Number, not $Death2WoundedDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($Death2WoundedDebuff) or push @errors => "Death2WoundedDebuff must be within the range 0-10 inclusive, not $Death2WoundedDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

=item Death2SoulsDebuff 
the Troop Death into Souls Debuff with no conditions. 
=cut 

  field $Death2SoulsDebuff :reader :param //= 0.1;

  ADJUST {
    my @errors;
    my $pNum = t('PositiveOrZeroNum');
    $pNum->check($Death2SoulsDebuff) or push @errors => "Death2SoulsDebuff must be a Positive Number, not $Death2SoulsDebuff";
    $range = t('IntRange[0, 10]');
    $range->check($Death2SoulsDebuff) or push @errors => "Death2SoulsDebuff must be within the range 0-10 inclusive, not $Death2SoulsDebuff";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }


}
1;

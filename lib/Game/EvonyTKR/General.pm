use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Buff::EvaluationMultipliers;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::General

# ABSTRACT: Module for processing information about Evony TKR Generals.

=head1 DESCRIPTION

=for Generals in Evony TKR are one of the more complicated and simultaneously
most frequently changing things that a player must make decisions about.  

This base class implements the attributes and methods common to all Generals, but should not be used directly.  Rather sub classes should be created for each of the crtical use cases in the game.  If the decision of which general to use does not require customized logic, it almost certainly is not critical enough to need particular consideration here either.  

=cut 

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

=attr name

the general's name, which will also be the key by which we find the general.
=cut
  field $name :reader :param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    } 
  }

=attr leadership

this is the base value for leadership, one of the four basic attributes of a general. 
=cut
  field $leadership :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($leadership) or push @errors => "leadership must be a number, not $leadership";
    $type->check($leadership) or push @errors => "leadership must be positive, not $leadership";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

=attr leadership_increment

by how much does the effective value of leadership (as opposed to its base value) increase with each level gained? 
=cut
  field $leadership_increment :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($leadership_increment) or push @errors => "leadership_increment must be a number, not $leadership_increment";
    $type->check($leadership_increment) or push @errors => "leadership_increment must be positive, not $leadership_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

=attr attack

this is the base value for attack, one of the four basic attributes of a general. 
=cut
  field $attack :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($attack) or push @errors => "attack must be a number, not $attack";
    $type->check($attack) or push @errors => "attack must be positive, not $attack";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

=attr attack_increment

by how much does the effective value of attack (as opposed to its base value) increase with each level gained? 
=cut
  field $attack_increment :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($attack_increment) or push @errors => "attack_increment must be a number, not $attack_increment";
    $type->check($attack_increment) or push @errors => "attack_increment must be positive, not $attack_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

=attr defense

this is the base value for defense, one of the four basic attributes of a general. 
=cut
  field $defense :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($defense) or push @errors => "defense must be a number, not $defense";
    $type->check($defense) or push @errors => "defense must be positive, not $defense";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

=attr defense_increment

by how much does the effective value of defense (as opposed to its base value) increase with each level gained? 
=cut
  field $defense_increment :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($defense_increment) or push @errors => "defense_increment must be a number, not $defense_increment";
    $type->check($defense_increment) or push @errors => "defense_increment must be positive, not $defense_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

=attr politics

this is the base value for politics, one of the four basic attributes of a general. 
=cut
  field $politics :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($politics) or push @errors => "politics must be a number, not $politics";
    $type->check($politics) or push @errors => "politics must be positive, not $politics";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

=attr politics_increment

by how much does the effective value of politics (as opposed to its base value) increase with each level gained? 
=cut
  field $politics_increment :reader :param;

  ADJUST {
    my @errors;
    my $type = t('PositiveOrZeroNum');
    is_Num($politics_increment) or push @errors => "politics_increment must be a number, not $politics_increment";
    $type->check($politics_increment) or push @errors => "politics_increment must be positive, not $politics_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

=attr level

generals start at level 1 and can grow to level 45.  Thier effective statistics increase as they do so by the increments listed in the _increment versions of each attribute. 
=cut
  field $level :reader :param //= 45;

  ADJUST {
    my @errors;
    my $pInt = t('PositiveOrZeroInt');
    is_Int($level) or push @errors => "level must be an integer, not $level";
    $pInt->check($level) or push @errors => "level must be positive, not $level";
    my $type = t('IntRange[1, 45]');
    $type->check($level) or push @errors => "level must be between 1 and 45 inclusive";

    if (@errors) {
      die join ', ' => @errors;
    }
  }

=method effective_leadership()

returns the value that a user of this general at this investment level will experience for leadership.
=cut
  method effective_leadership() {
    return $level * $leadership_increment + $leadership;
  }

=method effective_attack()

returns the value that a user of this general at this investment level will experience for attack.
=cut
  method effective_attack() {
    return $level * $attack_increment + $attack;
  }

=method effective_defense()

returns the value that a user of this general at this investment level will experience for defense.
=cut
  method effective_defense() {
    return $level * $defense_increment + $defense;
  }

=method effective_politics()

returns the value that a user of this general at this investment level will experience for politics.
=cut
  method effective_politics() {
    return $level * politics_increment + $politics_increment;
  }

=attr builtInBook

each general comes with one Game::EvonyTKR::SkillBook built in.  This will be an instance of the ::Special variety of SkillBook. 
=cut
  field $builtInBook :reader :param;

  ADJUST {
    my @errors;
    my $type = blessed $builtInBook;
    if($type ne 'Game::EvonyTKR::SkillBook::Special'){
      push @errors => "builtInBook must be a Game::EvonyTKR::SkillBook::Special, not $type";
      if (@errors) {
        die join ', ' => @errors;
      }
    }
  }

  use constant DEFAULT_BUFF_MULTIPLIERS => Game::EvonyTKR::Buff::EvaluationMultipliers->new();
  
=attr BuffMultipliers

when evaluating generals, not all buffs are equally important.  Nor are these scaling factors constant across the game, but rather differ both by type of general and by situation.  Tihs contains the scaling factors for this general.
=cut
  field $BuffMultipliers :reader :param //= __CLASS__->DEFAULT_BUFF_MULTIPLIERS;

  ADJUST {
    my @errors;
    my $type = blessed $BuffMultipliers;
    if($type ne 'Game::EvonyTKR::Buff::EvaluationMultipliers'){
      push @errors => "BuffMultipliers must be a Game::EvonyTKR::Buff::EvaluationMultipliers, not $type";
      if (@errors) {
        die join ', ' => @errors;
      }
    }
  }

}
1;

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

=head1 SYNOPSIS

=for comment Brief examples of using the module.

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

  field $name :reader :param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    } 
  }


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

  method effective_leadership() {
    return $level * $leadership_increment + $leadership;
  }

  method effective_attack() {
    return $level * $attack_increment + $attack;
  }

  method effective_defense() {
    return $level * $defense_increment + $defense;
  }

  method effective_politics() {
    return $level * politics_increment + $politics_increment;
  }

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

  field $BuffMultipliers :reader :param //= Game::EvonyTKR::Buff::EvaluationMultipliers->new();

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
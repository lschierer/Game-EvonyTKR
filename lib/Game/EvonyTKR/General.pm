use 5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General {
use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
use Types::Common::Numeric qw(PositiveOrZeroInt PositiveOrZeroNum IntRange);
use Type::Utils "is"; 
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
    is_Num($leadership) or push @errors => "leadership must be a number, not $leadership";
    PositiveOrZeroNum->check($leadership) or push @errors => "leadership must be positive, not $leadership";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

  field $leadership_increment :reader :param;

  ADJUST {
    my @errors;
    is_Num($leadership_increment) or push @errors => "leadership_increment must be a number, not $leadership_increment";
    PositiveOrZeroNum->check($leadership_increment) or push @errors => "leadership_increment must be positive, not $leadership_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $attack :reader :param;

  ADJUST {
    my @errors;
    is_Num($attack) or push @errors => "attack must be a number, not $attack";
    PositiveOrZeroNum->check($attack) or push @errors => "attack must be positive, not $attack";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

  field $attack_increment :reader :param;

  ADJUST {
    my @errors;
    is_Num($attack_increment) or push @errors => "attack_increment must be a number, not $attack_increment";
    PositiveOrZeroNum->check($attack_increment) or push @errors => "attack_increment must be positive, not $attack_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $defense :reader :param;

  ADJUST {
    my @errors;
    is_Num($defense) or push @errors => "defense must be a number, not $defense";
    PositiveOrZeroNum->check($defense) or push @errors => "defense must be positive, not $defense";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

  field $defense_increment :reader :param;

  ADJUST {
    my @errors;
    is_Num($defense_increment) or push @errors => "defense_increment must be a number, not $defense_increment";
    PositiveOrZeroNum->check($defense_increment) or push @errors => "defense_increment must be positive, not $defense_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $politics :reader :param;

  ADJUST {
    my @errors;
    is_Num($politics) or push @errors => "politics must be a number, not $politics";
    PositiveOrZeroNum->check($politics) or push @errors => "politics must be positive, not $politics";
    if (@errors) {
      die join ', ' => @errors;
    }
  } 

  field $politics_increment :reader :param;

  ADJUST {
    my @errors;
    is_Num($politics_increment) or push @errors => "politics_increment must be a number, not $politics_increment";
    PositiveOrZeroNum->check($politics_increment) or push @errors => "politics_increment must be positive, not $politics_increment";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $level :reader :param //= 45;

  ADJUST {
    my @errors;
    is_Int($level) or push @errors => "level must be an integer, not $level";
    PositiveOrZeroInt->check($level) or push @errors => "level must be positive, not $level";
    IntRange[1, 45]->check($level) or push @errors => "level must be between 1 and 45 inclusive";

    if (@errors) {
      die join ', ' => @errors;
    }
  }

}
1;
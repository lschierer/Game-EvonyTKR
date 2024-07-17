use 5.40.0;
use experimental qw(class);

class Game::EvonyTKR::SkillBook {
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common qw( -lexical -all );
  use Type::Utils "is"; 
  use Game::EvonyTKR::Buff;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::SkillBook

# ABSTRACT: Module for processing information about Evony TKR SkillBooks.

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

This is the base class, providing common methods for all SkillBooks.  You should probably be using either the SkillBook::Standard or the SkilllBook::Special subclass instead. 

=cut 

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    t->add_types(
    -Standard,
    -TypeTiny,
    -Common,
    -Common-String,
    -Common-Numeric
    );

    t->add_types('Game::EvonyTKR::Buff');
  }

  field $name :reader :param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field @buffs :reader;

  method add_buff($nb) {
    
  }
}

1;
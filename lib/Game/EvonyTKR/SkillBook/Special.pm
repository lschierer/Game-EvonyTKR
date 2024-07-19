use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::SkillBook::Special :isa(Game::EvonyTKR::SkillBook) {
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common qw( t);
  use Type::Utils "is"; 
  use Game::EvonyTKR::Buff;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::SkillBook::Special

# ABSTRACT: Module for processing information about Evony TKR SkillBooks Built-in to each General.

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

The SkillBook::Special class is for the built-in SkillBooks are intrinsic to a particular General.  

While there is no particular logic here, by requiring this class, I ensure that a General is not accidentally created with a Standard SkillBook. 

=cut 

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

=attr builtin

Most of the time when dealing with SkillBook::Special, we are dealing with the general's built in skill book.  Occasionally we are dealing with a removable skill set that the game displays as a "skin" or alternative outfit/appearance for the general.  These alternative apperances come with buffs that behave functionally equivalent to an additional SkillBook. 
=cut
  field builtin :reader :param //= 1;

=method is_builtin()

an assertion that tells us if this is the built in skill or an extra this general has from an add-on.
=cut
  method is_builtin() {
    return 1;
  }
  
}

1;
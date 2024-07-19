use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::SkillBook {
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common qw( t);
  use Type::Utils "is"; 
  use Game::EvonyTKR::Buff;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::SkillBook

# ABSTRACT: Module for processing information about Evony TKR SkillBooks.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

This is the base class, providing common methods for all SkillBooks.  You should probably be using either the SkillBook::Standard or the SkilllBook::Special subclass instead. 

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

each SkillBook has a name
=cut
  field $name :reader :param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

=attr buffs

each SkillBook has one or more Game::EvonyTKR:Buffs
=cut
  field @buffs :reader;

  method add_buff($nb) {
    if(blessed $nb ne 'Game::EvonyTKR::Buff'){
      return 0;
    } elsif (scalar @buffs >= 1){
      my $found_match = 0;
      foreach(@buffs) {
        if(not $nb->compare($_)) {
          $found_match = 1;
        }
      }
      if($found_match) {
        return 0;
      }
    }
    push @buffs, $nb;
  }
}

1;
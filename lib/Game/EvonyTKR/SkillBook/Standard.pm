use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::SkillBook::Standard :isa(Game::EvonyTKR::SkillBook) {
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common qw( t);
  use Type::Utils "is"; 
  use Game::EvonyTKR::Buff;
  use Carp;
  use namespace::autoclean;
  use overload 
    '<=>' => \&comparison,
    'cmp' => \&comparison,
    'eq'  => \&equality,
    '=='  => \&equality,
    'ne'  => \&inequality,
    '!='  => \&inequality;

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

  field $level :reader :param //= 1;

  ADJUST {
    my @errors;
    my $pInt = t('PositiveOrZeroInt');
    $pInt->check($level) or push @errors => "Level must be a Positive Integer, not $level";
    my $range = t('NumRange[1, 4]');
    $range->check($level) or push @errors => "Level must be within the range 1-4 inclusive, not $level";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

  method comparison($a, $b, $reversed = 0) {
    if(blessed $a ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$a is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if(blessed $b ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$b is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if($a->name() eq $b->name()) {
      return $a->level() <=> $b->level();
    } else {
      return $a->name() cmp $b->name();
    }
  }

  method equality ($a, $b, $reversed = 0) {
    if(blessed $a ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$a is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if(blessed $b ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$b is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if($a->name() eq $b->name()) {
      return $a->level() == $b->level();
    } else {
      return 0;
    }
  }

  method inequality ($a, $b, $reversed = 0) {
    if(blessed $a ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$a is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if(blessed $b ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$b is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if($a->name() ne $b->name()) {
      return $a->level() != $b->level();
    } else {
      return 0;
    }
  }

  
}

1;
__END__

# PODNAME: Game::EvonyTKR::SkillBook::Standard

# ABSTRACT: Module for processing information about the Standard Evony TKR SkillBooks which can be added to the SkillBook slots each General comes with.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

The SkillBook::Standard class is for the SkillBooks are I<not> intrinsic to a particular General, but rather the ones which are manually added by the user.  This includes both the ones that can be won for free and the premium ones that must be purchased in packs.  

=cut 

=attr $level

One of the primary differences between ::Standard and ::Special books is that all ::Standard books have a level attribute.  Higher level books automatically overright lower level books of the same name when added to a General. That is, a General can have at most one SkillBook that has a given name attribute, despite the fact that ::Standard SkillBooks are only I<uniquely> identified when you provide both name and level attributes.  

A SkillBook::Standard's level must be between 1 and 4, inclusive. 
=cut



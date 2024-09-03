use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::SkillBook::Standard : isa(Game::EvonyTKR::SkillBook) {
# PODNAME: Game::EvonyTKR::SkillBook::Standard

  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common   qw( t);
  use Type::Utils "is";
  use Game::EvonyTKR::Buff;
  use Carp;
  use namespace::autoclean;
  use overload
    '<=>' => \&_comparison,
    'cmp' => \&_comparison,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $level : reader : param //= 1;

  ADJUST {
    my @errors;
    my $pInt = t('PositiveOrZeroInt');
    $pInt->check($level)
      or push @errors => "Level must be a Positive Integer, not $level";
    my $range = t('NumRange[1, 4]');
    $range->check($level)
      or push @errors =>
      "Level must be within the range 1-4 inclusive, not $level";
    if (@errors) {
      croak join ', ' => @errors;
    }
  }

  method _comparison($other, $swap = 0) {
    if (blessed $other ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$other is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if ($self->name() eq $other->name()) {
      return $level <=> $other->level();
    }
    else {
      return $self->name() cmp $other->name();
    }
  }

  method _equality ($other, $swap = 0) {
    if (blessed $other ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$other is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if ($self->name() eq $other->name()) {
      return $level == $other->level();
    }
    else {
      return 0;
    }
  }

  method _inequality ($other, $swap = 0) {
    if (blessed $other ne 'Game::EvonyTKR::SkillBook::Standard') {
      croak '$other is not a Game::EvonyTKR::SkillBook::Standard';
    }
    if ($self->name() ne $other->name()) {
      return $level != $other->level();
    }
    else {
      return 0;
    }
  }

}

1;
__END__


# ABSTRACT: Module for processing information about the Standard Evony TKR SkillBooks which can be added to the SkillBook slots each General comes with.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

The SkillBook::Standard class is for the SkillBooks are I<not> intrinsic to a particular General, but rather the ones which are manually added by the user.  This includes both the ones that can be won for free and the premium ones that must be purchased in packs.

=cut

=attr $level

One of the primary differences between ::Standard and ::Special books is that all ::Standard books have a level attribute.  Higher level books automatically overright lower level books of the same name when added to a General. That is, a General can have at most one SkillBook that has a given name attribute, despite the fact that ::Standard SkillBooks are only I<uniquely> identified when you provide both name and level attributes.

A SkillBook::Standard's level must be between 1 and 4, inclusive.
=cut



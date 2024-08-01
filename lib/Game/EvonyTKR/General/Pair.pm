use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Pair {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
  use Util::Any -all;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Buff::EvaluationMultipliers;
  use namespace::autoclean;
  # PODNAME: Game::EvonyTKR::General::Pair
  # ABSTRACT: Manage Game::EvonyTKR::Generals as Pairs
  use overload
    'eq'        => \&_equality,
    '=='        => \&_equality,
    'ne'        => \&_inequality,
    '!='        => \&_inequality,
    "fallback"  => 1;


  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

  field $primary :reader :param;

  field $secondary :reader :param;

  ADJUST {
    my @errors;
    my $primaryClass = blessed $primary;
    my @GeneralClasses = (
      'Game::EvonyTKR::General',
      'Game::EvonyTKR::General::Ground',
      'Game::EvonyTKR::General::Mounted',
      'Game::EvonyTKR::General::Ranged',
      'Game::EvonyTKR::General::Siege',
    );
    if(none { $_ eq $primaryClass} @GeneralClasses) {
      push @errors => "primary must be a Game::EvonyTKR::General or subclass no $primaryClass";
    }
    my $secondaryClass = blessed $secondary;
    if(none { $_ eq $secondaryClass} @GeneralClasses) {
      push @errors => "primary must be a Game::EvonyTKR::General or subclass no $primaryClass";
    }
    if (@errors) {
      croak join ', ' => @errors;
    }
  }


  method _equality ($other, $swap = 0) {
    if(blessed $other eq 'Game::EvonyTKR::General::Pair') {
      if($self->primary() eq $other->primary()) {
        return $self->secondary() eq $other->secondary();
      }
    }else {
      croak "other must be a 'Game::EvonyTKR::General::Pair' not a " . blessed $other;
    }
    return 0;
  }

  method _inequality($other, $swap = 0) {
    if(blessed $other eq 'Game::EvonyTKR::General::Pair') {
      if($self->primary() eq $other->primary()) {
        return $self->secondary() ne $other->secondary();
      }
    }else {
      croak "other must be a 'Game::EvonyTKR::General::Pair' not a " . blessed $other;
    }
    return 1;
  }

};
1

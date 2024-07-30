use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Pair {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Buff::EvaluationMultipliers;
  use namespace::autoclean;
  # PODNAME: Game::EvonyTKR::General::Pair
  # ABSTRACT: Manage Game::EvonyTKR::Generals as Pairs
  use overload 
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality;


  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

  field $primary :reader :param;

  ADJUST {
    my @errors;
    if(blessed $primary ne )
  }


  method _equality ($other, $swap = 0) {
    if(exists $other->name){
      
    }
  }
  

};
1

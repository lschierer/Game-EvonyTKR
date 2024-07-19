use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Buff::EvaluationMultipliers::Attacking {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use namespace::autoclean;

# PODNAME: Game::EvonyTKR::Buff::EvaluationMultipliers::Attacking

# ABSTRACT: Game::EvonyTKR::Buff Evaluation Multipliers for Attacking use cases.

=head1 DESCRIPTION

See Game::EvonyTKR::Buff::EvaluationMultipliers for a general description.

This provides the base overrides for the Attacking use cases. 

=cut 

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not. 
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }
  
}
1;


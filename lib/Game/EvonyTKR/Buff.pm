use 5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Buff {
use Game::EvonyTKR::Buff::Data;
use Types::Standard qw(is_Int Int Str is_Str);
use Types::Common::Numeric qw(PositiveOrZeroInt);
use Type::Utils "is"; 
use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Buff

# ABSTRACT: Buff and Debuff primatives 

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for A Buff is an attribute that modifies various attributes in Evony TKR

Buffs can be positive or negative.  When negative, they are commonly referred to
as "Debuffs," however the game internals make very little distinction between a positive and negative buff. 

Buffs are most commonly I<calculated> as if all buffs came from generals.  This is not true, buffs come from a variety of sources, and this module forms the primative for use in any of them. 

=cut

my $classData = Game::EvonyTKR::Buff::Data->new();
my @BuffAttributes;
my @BuffConditions;
my @BuffClasses;

ADJUST {
  if(!(@BuffAttributes)) {
    $classData->set_BuffAttributes();
    @BuffAttributes = $classData->BuffAttributes();
  }

  if(!(@BuffConditions)) {
    $classData->set_BuffConditions();
    @BuffConditions = $classData->BuffConditions();
  }
  
  if(!(@BuffClasses)) {
    $classData->set_BuffClasses();
    @BuffClasses = $classData->BuffClasses();
  }
}

field $attribute :reader :param;

field $class :reader :param  //= 'All Troops';

method has_class {
  if(defined $class) {
    if($class !~ /All/i) {
      return true;
    }
  }
  return 0;
}

field @condition :reader ;

method set_condition ($nc) {
  if(is_Str($nc)) {
    if(!(@BuffConditions)){
      $classData->set_BuffConditions();
      @BuffConditions = $classData->BuffConditions();
    }
    if(grep {/^$nc$/} @BuffConditions) {
      if(!(grep {/^$nc$/} @condition)) {
        push @condition, $nc;
      }
    }
  }
}

method has_condition {
  if(scalar @condition >= 1) {
    return true;
  }
  return 0;
}

field $value :reader :param;

}

 
# methods


 
1;


use v5.40.0;
use experimental qw(class);
use utf8::all;


class Game::EvonyTKR::Buff :isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Buff

# ABSTRACT: Buff and Debuff primatives
  use Game::EvonyTKR::Buff::Data;
  use Types::Standard qw(is_Int Int Str is_Str);
  use Types::Common::Numeric qw(PositiveOrZeroInt);
  use Type::Utils "is";
  use Carp;
  use Class::ISA;
  use Data::Printer;
  use Util::Any -all;
  use Array::Utils qw(:all);
  use namespace::autoclean;
  use overload
    '<=>' => \&compare,
    'cmp' => \&compare,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

  my $classData = Game::EvonyTKR::Buff::Data->new();
  my @BuffAttributes;
  my @BuffConditions;
  my @BuffClasses;

  ADJUST {
    if(scalar @BuffAttributes == 0) {
      $classData->set_BuffAttributes();
      @BuffAttributes = $classData->BuffAttributes();
    }

    if(scalar @BuffConditions == 0) {
      $classData->set_BuffConditions();
      @BuffConditions = $classData->BuffConditions();
    }

    if(scalar @BuffClasses == 0) {
      $classData->set_BuffClasses();
      @BuffClasses = $classData->BuffClasses();
    }
  }

  field $attribute :reader :param;

  field $buffClass :reader :param  //= 'All Troops';

  field @condition :reader ;

  field $value :reader :param;

  field $inherited :reader :param //= 0;

  ADJUST {
    if(scalar @BuffClasses == 0) {
      $classData->set_BuffClasses();
      @BuffClasses = $classData->BuffClasses();
    }
    if(scalar @BuffClasses == 0) {
      $self->logger()->logcroak("no classes loaded");
    } elsif (! grep {/^$buffClass$/} @BuffClasses){
      $self->logger()->logcroak("$buffClass is an invalid class");
    }
  }

  method has_buffClass {
    if(defined $buffClass) {
      if($buffClass !~ /All/i) {
        return true;
      }
    }
    return 0;
  }

  method toggleInherited {
    $self->logger()->trace("Toggling inherited value for " . np $self);
    $inherited = not $inherited;
  }

  method set_condition ($nc) {
    if(is_Str($nc)) {
      if(scalar @BuffConditions == 0){
        $classData->set_BuffConditions();
        @BuffConditions = $classData->BuffConditions();
      }
      if(scalar @BuffConditions == 0){
        $self->logger()->logcroak('No conditions');
      } elsif (grep {/^$nc$/} @BuffConditions) {
        if(!(grep {/^$nc$/} @condition)) {
          push @condition, $nc;
        }
      } else {
        $self->logger()->logcroak( "$nc is an invalid condition");
      }
    }
  }

  method has_condition {
    if(scalar @condition >= 1) {
      return true;
    }
    return 0;
  }

  method compare($other, $swap = 0) {
    my $otherClass = blessed $other;
    if(not defined $otherClass){
      $self->logger()->logcroak( "otherClass is not defined");
    }
    $self->logger()->trace("comparing against a object of type $otherClass");
    if(not defined $other ){
      return 0;
    } elsif ($otherClass ne 'Game::EvonyTKR::Buff') {
      return 0;
    } else {
      if ($other->attribute() ne $attribute) {
        $self->logger()->trace("$attribute ne " . $other->attribute());
        return 0;
      }
      if ($other->value()->number() != $value->number()) {
        $self->logger()->trace($value->number() . " != " . $other->value()->number());
        return 0;
      }
      if ($other->value()->unit() ne $value->unit()) {
        $self->logger()->trace($value->unit() . " ne " . $other->value()->unit());
        return 0;
      }  
      if (
        ($self->has_buffClass() && (not $other->has_buffClass())) ||
        ((not $self->has_buffClass()) && $other->has_buffClass())) {
          $self->logger()->trace($self->has_buffClass() . " and not " . $other->has_buffClass());
          return 0;
        } elsif (
        ($self->has_condition() && (not $other->has_condition())) ||
        ((not $self->has_condition()) && $other->has_condition())
      ) {
        $self->logger()->trace($self->has_condition() . " and not " . $other->has_condition());
        return 0;
      } else {
        my $otherClass = $other->buffClass();
        if($buffClass ne $otherClass) {
          $self->logger()->trace("$otherClass ne $buffClass");
          return 0;
        }
        my @other_condition = $other->condition();
        if( array_diff(@condition, @other_condition) ) {
          $self->logger()->trace("condition arrays differ");
          $self->logger()->trace("my conditions are " . np @condition);
          $self->logger()->trace("their conditions are " . np @other_condition);
          return 0;
        }
      }
    }
    $self->logger()->trace("ran out of things to test, they must be the same");
    return 1;
  }

  method toHashRef() {
    my $returnRef = {};
    $returnRef->{'attribute'} = $self->attribute();
    $returnRef->{'value'} = {
      number  => $self->value()->number(),
      unit    => $self->value()->unit(),
    };
    
    if($self->buffClass() !~ /All Troops/i ) {
      $returnRef->{'class'} = $self->buffClass();
    }

    my @_conditions = $self->condition();
    if(scalar @_conditions) {
      $returnRef->{'condition'} = \@_conditions;
    }
    $returnRef->{'inherited'} = $self->inherited();
    return $returnRef;
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method _equality($other, $swap = 0){
    my $otherClass = blessed $other;
    my @classList = Class::ISA::self_and_super_path($otherClass);
    if(none {$_ eq 'Game::EvonyTKR::Buff'} @classList) {
      $self->logger()->logcroak('$other is not a Game::EvonyTKR::Buff'); 
    }
    my $result = $self->compare($other);
    $self->logger()->trace("compare functionr returned $result");
    if($result) {
      $self->logger()->trace("_equality returning true for result '$result'");
      return 1
    }
    $self->logger()->trace("_equality returning false for result '$result'");
    return 0;
  }

  method _inequality($other, $swap = 0){
    my $otherClass = blessed $other;
    my @classList = Class::ISA::self_and_super_path($otherClass);
    if(none {$_ eq 'Game::EvonyTKR::Buff'} @classList) {
      $self->logger()->logcroak('$other is not a Game::EvonyTKR::Buff'); 
    }
    if($self->compare($other) != 0) {
      return 1
    }
    return 0;
  }

}

1;

__END__

=head1 DESCRIPTION

A Buff is an attribute that modifies various attributes in Evony TKR

Buffs can be positive or negative.  When negative, they are commonly referred to
as "Debuffs," however the game internals make very little distinction between a positive and negative buff.

Buffs are most commonly I<calculated> as if all buffs came from generals.  This is not true, buffs come from a variety of sources, and this module forms the primative for use in any of them.

=cut

=method compare($other, $swap)

this function returns true if the $other is logically the same
as this Game::EvonyTKR::Buff.  It is written with a particular style to aid in debugging it should I ever suspect I've done it wrong.  Rather than attempting to return immediately, I have written each test to store a unique negative value then I return false if my stored value has been set negative anywhere in the overall function.  This way, by setting $debug to a truthy value, the function will inform me via the "say" statement at the bottom which test determined the difference.   The function defaults to truthy.

$swap is currently unused, but will eventually handle the case someone somehow calls the <=> operator backwards, which is apparently possible.
=cut

=method inherited

this function is used to indicate that the Buff in question should not be exported when computing level at a time values as opposed to cumulative values, as it has been inherited from some other place. 
=cut

=method toggleInherited

This toggles the value of the inherirted field.
=cut

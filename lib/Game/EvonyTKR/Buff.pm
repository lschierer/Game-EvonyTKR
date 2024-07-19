use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Buff {
  use Game::EvonyTKR::Buff::Data;
  use Types::Standard qw(is_Int Int Str is_Str);
  use Types::Common::Numeric qw(PositiveOrZeroInt);
  use Type::Utils "is"; 
  use Carp;
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

  ADJUST {
    if(scalar @BuffClasses == 0) {
      $classData->set_BuffClasses();
      @BuffClasses = $classData->BuffClasses();
    }
    if(scalar @BuffClasses == 0) {
      croak "no classes loaded";
    } elsif (! grep {/^$buffClass$/} @BuffClasses){
      croak "$buffClass is an invalid class."
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

  field @condition :reader ;

  method set_condition ($nc) {
    if(is_Str($nc)) {
      if(scalar @BuffConditions == 0){
        $classData->set_BuffConditions();
        @BuffConditions = $classData->BuffConditions();
      }
      if(scalar @BuffConditions == 0){
        croak 'No conditions';
      } elsif (grep {/^$nc$/} @BuffConditions) {
        if(!(grep {/^$nc$/} @condition)) {
          push @condition, $nc;
        }
      } else {
        croak '$nc is an invalid condition';
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

=head2 method compare($other)

this function returns true if the $other is logically the same
as this Game::EvonyTKR::Buff.  It is written with a particular style to aid in debugging it should I ever suspect I've done it wrong.  Rather than attempting to return immediately, I have written each test to store a unique negative value then I return false if my stored value has been set negative anywhere in the overall function.  This way, by setting $debug to a truthy value, the function will inform me via the "say" statement at the bottom which test determined the difference.   The function defaults to truthy.
=cut 

  method compare($other) {
    my $debug = 0; #set to 1 to debug this function
    my $code = 1;
    if(blessed $other ne 'Game::EvonyTKR::Buff') {
      $code = -1;
    } elsif($other->attribute() ne $attribute) {
      $code = -2;
    } elsif($other->value()->number() != $value->number) {
      $code = -3;
    } elsif($other->value()->unit() ne $value->unit) {
      $code = -4;
    } elsif(
      ($self->has_buffClass() && (not $other->has_buffClass())) || 
      ((not $self->has_buffClass()) && $other->has_buffClass())) {
        $code = -5;
      }elsif(
      ($self->has_condition() && (not $other->has_condition())) ||
      ((not $self->has_condition()) && $other->has_condition())
    ) {
      $code = -6;
    }elsif($buffClass ne $other->buffClass()) {
      $code = -7;
    }
    if( $code >= 0) {
      # I cannot use an else-if pattern for this because 
      # the precoditions for testing are not in place,
      # are expensive to gather, and need not be gathered at all
      # if I can detect false without doing so. 
      my @other_condition = $other->condition();
      my @diff = condition_difference(\@condition, \@other_condition);
      if(scalar @diff != 0) {
        $code = -8;
      }
    }
    if($debug) {
      say $code;
    }
    return ($code > 0);
  }

  sub condition_difference($one, $two) {
    # lifted from https://perldoc.perl.org/perlfaq4#How-do-I-compute-the-difference-of-two-arrays%3f-How-do-I-compute-the-intersection-of-two-arrays
    my (@union, @intersection, @difference);
    my %count;
    foreach my $element (@$one, @$two) { $count{$element}++ }
    foreach my $element (keys %count) {
        push @union, $element;
        push @{ $count{$element} > 1 ? \@intersection : \@difference }, $element;
    }
    return @difference;
  }

}
 
1;


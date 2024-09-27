use v5.40.0;
use experimental qw(class);
use utf8::all;
use FindBin;
use lib "$FindBin::Bin/../../../lib";

class Game::EvonyTKR::Buff : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Buff
# VERSION
  use Game::EvonyTKR::Data;
  use Types::Standard        qw(is_Int Int Str is_Str);
  use Types::Common::Numeric qw(PositiveOrZeroInt);
  use Type::Utils "is";
  use Carp;
  use Data::Printer;
  use Util::Any -all;
  use Array::Utils qw(:all);
  use namespace::autoclean;
  use FindBin;
  use lib "$FindBin::Bin/../../../lib";
  use overload
    '<=>' => \&compare,
    'cmp' => \&compare,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

  my $classData = Game::EvonyTKR::Data->new();
  my @BuffAttributes;
  my @AllConditions;
  my @BuffClasses;

  ADJUST {
    if (scalar @BuffAttributes == 0) {
      $classData->set_BuffAttributes();
      @BuffAttributes = $classData->BuffAttributes();
    }

    if (scalar @AllConditions == 0) {
      @AllConditions = $classData->AllConditions();
    }

    if (scalar @BuffClasses == 0) {
      @BuffClasses = $classData->BuffClasses();
    }
  }

  field $attribute : reader : param;

  field $buffClass : reader : param //= 'All Troops';

  field @condition : reader;

  field $value : reader : param;

  field $inherited : reader : param //= 0;

  ADJUST {
    if (not defined $attribute) {
      $self->logger()->logcroak('Attribute must be defined');
    }
    else {
      if (scalar @BuffAttributes == 0) {
        $classData->set_BuffAttributes();
        @BuffAttributes = $classData->BuffAttributes();
      }
      if (scalar @BuffAttributes == 0) {
        $self->logger()->logcroak('no attributes loaded');
      }
      elsif (none { $_ =~ /^$attribute$/ } @BuffAttributes) {
        $self->logger()->logcroak("$attribute is an invalid attribute");
      }
    }

    if (scalar @BuffClasses == 0) {
      $classData->set_BuffClasses();
      @BuffClasses = $classData->BuffClasses();
    }
    if (scalar @BuffClasses == 0) {
      $self->logger()->logcroak("no classes loaded");
    }
    elsif (none { $_ =~ /^$buffClass$/ } @BuffClasses) {
      $self->logger()
        ->logcroak(
        "$buffClass is an invalid class, valid options are " . np @BuffClasses);
    }
  }

  method getEvAnsScore($name, $EvalData, $GeneralBias) {
    $self->logger()
      ->trace("starting getEvAnsScore for $name with $GeneralBias");
    if (any { $_ =~ /$GeneralBias/i } @BuffClasses) {
      if (
        any {
          my $tc = $_;
          any { $_ =~ /$tc/i } $EvalData->RelevantDebuffConditions()
        } @condition
      ) {
        $self->logger()
          ->debug("Detected a debuff for $name in " . np @condition);
        return $value->number() * $EvalData->getMultiplierForDebuff(
          $self->attribute(), $GeneralBias,
          $self->buffClass(), $self->value()->unit(),
        );
      }
      else {
        my $debugCondition = np @condition;
        $self->logger()
          ->debug("condition array '$debugCondition' indicates this is a buff");
        if (scalar @condition) {
          if (
            any {
              my $tc = $_;
              any { $_ =~ /^$tc$/i } $EvalData->RelevantBuffConditions()
            } @condition
          ) {
            return $value->number() * $EvalData->getMultiplierForBuff(
              $self->attribute(), $GeneralBias,
              $self->buffClass(), $self->value()->unit(),
            );
          }
          else {
            $self->logger()
              ->debug(
"buff for $name has a condition that disqualifies it from applying"
              );
            $self->logger()->trace("buff conditions: " . np @condition);
            $self->logger()
              ->trace("qualifying conditions: "
                . np $EvalData->RelevantBuffConditions());
          }
        }
        else {
          $self->logger()->trace('buff has no conditions');
          return $value->number() * $EvalData->getMultiplierForBuff(
            $self->attribute(), $GeneralBias,
            $self->buffClass(), $self->value()->unit(),
          );
        }
      }
    }
    else {
      $self->logger()->error("GeneralBias $GeneralBias for $name is invalid.");
    }
    return 0;
  }

  method has_buffClass {
    if (defined $buffClass) {
      if ($buffClass !~ /All/i) {
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
    if (is_Str($nc)) {
      if (scalar @AllConditions == 0) {
        $classData->collateConditions();
        @AllConditions = $classData->AllConditions();
      }
      if (scalar @AllConditions == 0) {
        $self->logger()->logcroak('No conditions');
      }
      elsif (grep {/^$nc$/} @AllConditions) {
        if (!(grep {/^$nc$/} @condition)) {
          push @condition, $nc;
        }
      }
      else {
        $self->logger()
          ->logcroak("$nc is an invalid condition, valid conditions are "
            . np @AllConditions);
      }
    }
  }

  method has_condition {
    if (scalar @condition >= 1) {
      return true;
    }
    return 0;
  }

  method compare($other, $swap = 0) {
    my $otherClass = blessed $other;
    if (not defined $otherClass) {
      $self->logger()->logcroak("otherClass is not defined");
    }
    $self->logger()->trace("comparing against a object of type $otherClass");
    if (not defined $other) {
      return 0;
    }
    elsif ($otherClass ne 'Game::EvonyTKR::Buff') {
      return 0;
    }
    else {
      if ($other->attribute() ne $attribute) {
        $self->logger()->trace("$attribute ne " . $other->attribute());
        return $attribute cmp $other->attribute();
      }
      if ($other->value()->number() != $value->number()) {
        $self->logger()
          ->trace($value->number() . " != " . $other->value()->number());
        return $value->number() <=> $other->value()->number();
      }
      if ($other->value()->unit() ne $value->unit()) {
        $self->logger()
          ->trace($value->unit() . " ne " . $other->value()->unit());
        return $value->unit() cmp $other->value()->unit();
      }
      if ($self->has_buffClass() && ($other->has_buffClass())) {
        $self->logger()->trace('both have classes');
        if( $self->buffClass() ne $other->buffClass()){
          return $self->buffClass() cmp $other->buffClass();
        }
      }
      elsif($self->has_buffClass()) {
        $self->logger()->trace('only I have a class');
        return 1;
      }
      elsif($other->has_buffClass()) {
        $self->logger()->trace('only the other has a class');
        return -1;
      }
      elsif ($self->has_condition() && $other->has_condition()) {
        $self->logger()->trace('both have conditions');
        
        my @cone = sort $self->condition();
        my $sizeone = scalar @cone;
        my @ctwo = sort $other->condition();
        my $sizetwo = scalar @ctwo;

        if($sizeone == $sizetwo) {
          $self->logger()->trace('both have the same number of conditions');
          for my $index (0..$#cone) {
            my $c1 = $cone[$index];
            my $c2 = $ctwo[$index];
            if($c1 ne $c2) {
              return $c1 cmp $c2
            }
          }
        }
      }
      elsif ($self->has_condition()) {
        $self->logger()->trace('only I have conditions');
        return -1;
      }
      elsif ($other->has_condition()) {
        $self->logger()->trace('only the other has conditions');
        return 1;
      }
    }
    $self->logger()->trace("ran out of things to test, they must be the same");
    return 0;
  }

  method toHashRef() {
    my $returnRef = {};
    $returnRef->{'attribute'} = $self->attribute();
    $returnRef->{'value'}     = {
      number => $self->value()->number(),
      unit   => $self->value()->unit(),
    };

    if ($self->buffClass() !~ /All Troops/i) {
      $returnRef->{'class'} = $self->buffClass();
    }

    my @_conditions = $self->condition();
    if (scalar @_conditions) {
      $returnRef->{'condition'} = \@_conditions;
    }
    $returnRef->{'inherited'} = $self->inherited();
    return $returnRef;
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method _equality($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'Buff') {
      $self->logger()->logcroak('$other is not a Game::EvonyTKR::Buff');
    }
    my $result = $self->compare($other);
    $self->logger()->trace("compare function returned $result");
    if ($result) {
      $self->logger()->trace("_equality returning true for result '$result'");
      return 0;
    }
    $self->logger()->trace("_equality returning false for result '$result'");
    return 1;
  }

  method _inequality($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'Buff') {
      $self->logger()->logcroak('$other is not a Game::EvonyTKR::Buff');
    }
    my $result = $self->compare($other);
    $self->logger()->trace("compare function returned $result");
    if ($result) {
      $self->logger()->trace("_inequality returning true for result '$result'");
      return 1;
    }
    $self->logger()->trace("_inequality returning false for result '$result'");
    return 0;
  }

}

1;

__END__

# ABSTRACT: Buff and Debuff primatives

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

=method getEvAnsScore($name, $EvalData, $GeneralBias)

$name is simply a string used to help identify entries in logs.

$EvalData must contain an instance of a I<child class> of Game::EvonyTKR::Buff::EvaluationData

$GeneralBias must contain a valid instane of a class from Game::EvonyTKR::Data

returns the EvAnsScore for this buff for a general with the provided bias.
=cut

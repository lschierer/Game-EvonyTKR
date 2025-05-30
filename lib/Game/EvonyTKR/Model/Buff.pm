use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Model::Buff : isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::Buff
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Types::Common qw( t );
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&TO_JSON,
    '.'        => \&concat,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $attribute : reader : param;

  field $value : reader : param;

  field $debuffConditions : reader : param //= [];

  field $buffConditions : reader : param //= [];

  field $targetedTypes : reader : param //= [];

  method conditions() {
    my @result;

    # Check if debuffConditions exists and is an array reference
    if (defined $self->debuffConditions()
      && ref($self->debuffConditions()) eq 'ARRAY') {
      push @result, @{ $self->debuffConditions() };
    }

    # Check if buffConditions exists and is an array reference
    if (defined $self->buffConditions()
      && ref($self->buffConditions()) eq 'ARRAY') {
      push @result, @{ $self->buffConditions() };
    }

    return @result;
  }

  field $passive : reader : param //= 0;

  method set_condition ($condition) {
    my $logger = $self->logger();
    my $re     = $self->buffConditionValues->as_regexp();

    # Check if the condition is a valid buff condition
    if ($condition =~ /$re/) {
      # Initialize the array if it doesn't exist
      $buffConditions //= [];

      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } @$buffConditions) {
        push @$buffConditions, $condition;
      }

      $logger->debug("Added buff condition: $condition");
      return 1;
    }

    $re = $self->debuffConditionValues->as_regexp();

    # Check if the condition is a valid debuff condition
    if ($condition =~ /$re/) {
      # Initialize the array if it doesn't exist
      $debuffConditions //= [];

      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } @$debuffConditions) {
        push @$debuffConditions, $condition;
      }

      $logger->debug("Added debuff condition: $condition");
      return 1;
    }

    # If we get here, the condition wasn't valid
    $logger->error(
      "Invalid condition: '$condition'. Must be one of: "
        . join(", ",
        $self->buffConditionValues()->values(),
        $self->debuffConditionValues()->values())
    );
    return 0;

  }

  method validate() {
    my @errors;
    my $re = $self->buffConditionValues()->as_regexp;
    $self->logger->trace("re for buffConditionValues is $re");
    my @tbc = @{$buffConditions};
    my @invalid;

    # Check if we got an array reference instead of a flat array
    if (@tbc == 1 && ref($tbc[0]) eq 'ARRAY') {
      $self->logger->error("needed to flatten buffConditions"
          . Data::Printer::np($buffConditions));
      @tbc = @{ $tbc[0] };    # Flatten it
    }

    @invalid = grep !/$re/, @tbc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf(
'Detected illegal value "%s" in buffConditions. All values must be one of: %s',
          $iv, join(', ', $self->buffConditionValues->values()));
      }
    }

    $re = $self->debuffConditionValues()->as_regexp;
    $self->logger->trace("re for debuffConditionValues is $re");

    my @tdc = @{$debuffConditions};

    # Check if we got an array reference instead of a flat array
    if (@tdc == 1 && ref($tdc[0]) eq 'ARRAY') {
      @tdc = @{ $tdc[0] };    # Flatten it
      $self->logger->error("needed to flatten debuffConditions");
    }

    @invalid = grep !/$re/, @tdc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf(
'Detected illegal value "%s" in debuffConditions. All values must be one of: %s',
          $iv, join(', ', $self->debuffConditionValues()->values()));
      }
    }

    my $type = t('BoolLike');
    $type->check($passive)
      or push @errors => "passive must be 0 or 1, not $passive";

    if (@errors) {
      $self->logger()->logcroak(join(', ' => @errors));
    }

    $value->validate();

  }

  ADJUST {
    $self->validate();
  }

  method has_targetedType() {
    if (ref $targetedTypes eq 'ARRAY') {
      return scalar @{$targetedTypes};
    }
    else {
      return length($targetedTypes);
    }
  }

  method toHashRef() {
    my $c;
    my $conditionCount = scalar $self->conditions();
    $self->logger()->info("in toHashRef, I have $conditionCount conditions");
    if ($conditionCount) {
      $c = $self->conditions();
    }
    else {
      $c = [];
    }
    return {
      attribute => $attribute,
      value     => {
        number => $value->number(),
        unit   => $value->unit(),
      },
      passive       => $passive,
      targetedTypes => scalar @{$targetedTypes} > 0 ? $targetedTypes : [],
      conditions    => $c,
    };
  }

  method TO_JSON() {
    return $self->toHashRef();
  }

  method concat($other, $swap) {
    if ($swap) {
      return $other . $self->TO_JSON();
    }
    else {
      return $self->TO_JSON() . $other;
    }
  }

}
1;

__END__

#ABSTRACT: The basic unit of incrementing or decrementing strength of Generals and Troops

=pod

=head1 DESCRIPTION

A buff is the basic unit of incrementing some aspect of the strength of either a general or the troops that general leads.

A debuff is a negative buff, and so the same class implements both.

Buffs/Debuffs apply to attributes, and may do so conditionally (for example when
attacking, or when defending), or unconditionally.  The buff may apply actively
(when a general leads a march, or when a civilization treasure is activated), or
passively (the buff/debuff applies simply because you own the General or Treasure).

Both active and passive buffs may be conditional on the General's (Treasure's, Dragon's,
so on) level, the level to which the General has been ascended, and/or the level
to which the General's specialities have been activated.  Similar level related
conditions apply to Buffs/Debuffs associated with Senate Seats, Armor, and Spiritual
Beast Seals.

=cut

=method attribute()

returns the attribute that which this Buff/Debuff affects.

=cut

=method value()

returns the Game::EvonyTKR::Model::Buff::Value that is the amount by which this
buff/debuff affects its attribute.

=cut

=method debuffConditions()

if not empty, this is a debuff, and this defines when the debuff is active.

=cut

=method buffConditions()

if not empty, restricts when this buff/debuff acts on its attribute

=cut

=method targetedTypes()

if not empty, restricts what targets are affected by this buff/debuff.

note that one of the targets is "monsters."  If that is the target, then
and this is a *buff*, then this is effectively a *debuff* for you. if the target is
Monsters and it is a *debuff*, it is effectively a *buff* for you.

=cut

=method conditions

if not empty, there are restictions on when this buff/debuff affects its attribute.
You should use the more specific methods to determine what those conditions are, but you
could use this if you are just printing them.

=cut

=method validate()

this will cause a croak if anything about this buff violates its defined structure.

=cut


=cut

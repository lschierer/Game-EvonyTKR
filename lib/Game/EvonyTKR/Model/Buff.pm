use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;

class Game::EvonyTKR::Model::Buff : isa(Game::EvonyTKR::Shared::Constants) {
# PODNAME: Game::EvonyTKR::Model::Buff
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Types::Common qw( t );
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $attribute : reader : param;

  field $value : reader : param;

  field $debuffConditions : reader : param //= [];

  field $buffConditions : reader : param //= [];

  field $targetedType : reader : param //= '';

  field $passive : reader : param //= 0;

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

  method set_target ($tt) {
    if (any { $_ eq $tt } values %{ $self->TroopTypeValues }) {
      $targetedType = $tt;
    }
  }

  method set_condition ($condition) {
    my $logger = $self->logger();

    # Check if the condition is a valid buff condition
    if (any { $condition eq $_ } keys %{ $self->BuffConditionValues }) {
      # Initialize the array if it doesn't exist
      $buffConditions //= [];

      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } @$buffConditions) {
        push @$buffConditions, $condition;
      }

      $logger->debug("Added buff condition: $condition");
      return 1;
    }

    # Check if the condition is a valid debuff condition
    if (any { $condition eq $_ } @{ $self->DebuffConditionValues }) {
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
        keys %{ $self->BuffConditionValues },
        @{ $self->DebuffConditionValues })
    );
    return 0;

  }

  method validate() {
    my @errors;
    my @tbc = @{$buffConditions};
    my @invalid;

    # Check if we got an array reference instead of a flat array
    if (@tbc == 1 && ref($tbc[0]) eq 'ARRAY') {
      $self->logger->error("needed to flatten buffConditions"
          . Data::Printer::np($buffConditions));
      @tbc = @{ $tbc[0] };    # Flatten it
    }

    # Find elements in @tbc that are not in the valid conditions
    my %valid_conditions = map { $_ => 1 } keys %{ $self->BuffConditionValues };
    @invalid = grep { !exists $valid_conditions{$_} } @tbc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf(
'Detected illegal value "%s" in buffConditions. All values must be one of: %s',
          $iv, join(', ', keys %{ $self->BuffConditionValues }));
      }
    }

    my @tdc = @{$debuffConditions};

    # Check if we got an array reference instead of a flat array
    if (@tdc == 1 && ref($tdc[0]) eq 'ARRAY') {
      @tdc = @{ $tdc[0] };    # Flatten it
      $self->logger->error("needed to flatten debuffConditions");
    }

    # Find elements in @tdc that are not in the valid debuff conditions
    my %valid_debuff_conditions =
      map { $_ => 1 } @{ $self->DebuffConditionValues };
    @invalid = grep { !exists $valid_debuff_conditions{$_} } @tdc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf(
'Detected illegal value "%s" in debuffConditions. All values must be one of: %s',
          $iv, join(', ', @{ $self->DebuffConditionValues() }));
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
    return length($targetedType);
  }

  method match_buff (
    $test_attribute,
    $test_targetedType     = '',
    $test_buffConditions   = [],
    $test_debuffConditions = [],
  ) {
    my $logger = $self->logger;

    # Override empty array with defaults
    if (scalar @$test_buffConditions == 0) {
      $test_buffConditions = [
        "Attacking",
        "brings a dragon",
        "brings dragon or beast to attack",
        "dragon to the attack",
        "leading the army to attack",
        "Marching",
        "When Rallying",
      ];
      $logger->trace("Empty buff conditions provided, using defaults instead");
    }

    $logger->trace(sprintf(
'Checking match for buff: attr=%s, targetType=%s, buff conditions=%s, debuff conditions=%s',
      $attribute,
      $targetedType,
      join(', ', ref $buffConditions eq 'ARRAY'   ? @$buffConditions   : ()),
      join(', ', ref $debuffConditions eq 'ARRAY' ? @$debuffConditions : ()),
    ));

    # Check attribute match
    return 0 unless $attribute eq $test_attribute;

    # Check targetedType match if provided
    if (length($test_targetedType) && length($targetedType)) {
      # targetType often comes from generals, convert it for use here.
      if ($test_targetedType =~ /^(\w+)_specialist$/) {
        my $short = $1;

        my %general_to_targeted = (
          mounted => 'Mounted Troops',
          ground  => 'Ground Troops',
          ranged  => 'Ranged Troops',
          siege   => 'Siege Machines',
        );

        if (exists $general_to_targeted{$short}) {
          $test_targetedType = $general_to_targeted{$short};
          $logger->trace(
            "Normalized test_targetedType to '$test_targetedType'");
        }
        else {
          $logger->warn(
            "Unrecognized general specialist key: $test_targetedType");
          return 0;
        }
      }

      # Match against the buff's targetedTypes
      if ($targetedType ne $test_targetedType) {
        $logger->trace(sprintf(
          '  ✗ Rejected: test_targetedType "%s" not matched by "%s"',
          $test_targetedType, $targetedType
        ));
        return 0;
      }
    }

    # Check debuff conditions
    my $has_debuff_conditions = scalar @$debuffConditions > 0;

    # If test_debuffConditions is empty, reject any buff with debuff conditions
    if (scalar @$test_debuffConditions == 0) {
      if ($has_debuff_conditions) {
        $logger->trace(
          "  ✗ Rejected: buff has debuff conditions but none were requested");
        return 0;
      }
    }
# If test_debuffConditions is provided, check that all debuff conditions are in the allowed list
    elsif ($has_debuff_conditions) {
      foreach my $condition (@$debuffConditions) {
        if (none { $_ eq $condition } @$test_debuffConditions) {
          $logger->trace(
            "  ✗ Rejected: debuff condition '$condition' not in allowed list");
          return 0;
        }
      }
    }
    else {
      $logger->trace(
"  ✗ Rejected: debuff conditions are not present in buff and are required."
      );
      return 0;
    }

    # Check buff conditions
    my $has_buff_conditions = scalar @$buffConditions > 0;

# If test_buffConditions is provided, check that all buff conditions are in the allowed list
    if (scalar @$test_buffConditions > 0) {
      # A buff with no conditions should match when conditions are specified
      if (!$has_buff_conditions) {
        $logger->trace(
          "  ✓ Buff has no conditions, accepting unconditional buff");
        # Continue to the end of the function
      }
      else {
        $logger->trace("Checking buff conditions: "
            . join(', ', @$buffConditions)
            . " against allowed: "
            . join(', ', @$test_buffConditions));

        # Check if ANY of the buff's conditions are NOT in the allowed list
        my %allowed_conditions = map { $_ => 1 } @$test_buffConditions;
        foreach my $condition (@$buffConditions) {
          if (!exists $allowed_conditions{$condition}) {
            $logger->trace(
              "  ✗ Rejected: buff condition '$condition' not in allowed list");
            return 0;
          }
        }

      }
    }
    else {
      $logger->error("NO TEST BUFF CONDITIONS!!");
    }

    $logger->trace("  ✓ Buff matched");
    return 1;
  }

  method to_hash {
    my $c;
    my $conditionCount = scalar $self->conditions();
    $self->logger()->debug("in to_hash, I have $conditionCount conditions");
    if ($conditionCount) {
      my @rc = $self->conditions();
      $c = \@rc;
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
      passive      => $passive,
      targetedType => length($targetedType) > 0 ? $targetedType : '',
      conditions   => $c,
    };
  }

  method TO_JSON {
    return $self->to_hash();
  }

  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  method concat($other, $swap) {
    if ($swap) {
      return $other . $self->as_string();
    }
    else {
      return $self->as_string() . $other;
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
to which the General's specialties have been activated.  Similar level related
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
to which the General's specialties have been activated.  Similar level related
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

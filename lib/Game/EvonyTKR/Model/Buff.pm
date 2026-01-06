use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

require Game::EvonyTKR::Model::Buff::Value;
use namespace::autoclean;

package Game::EvonyTKR::Model::Buff {
  use Moo;
  extends 'Game::EvonyTKR::Model::Base';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  use List::AllUtils qw( any none );
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';

  has attribute => (is => 'rw');
  has targetedType => (is => 'rw');

  has value => (
    is => 'rw',
    lazy => 1,
    default => sub { Game::EvonyTKR::Model::Buff::Value->new() }
  );

  has debuffConditions => (
    is => 'rw',
    lazy => 1,
    default => sub { [] }
  );

  has buffConditions => (
    is => 'rw',
    lazy => 1,
    default => sub { [] }
  );

  has passive => (
    is => 'rw',
    default => sub { 0 }
  );

  has DISABLED => (
    is => 'rw',
    default => sub { 0 }
  );

  sub clone ($self) {
    my $copy = __PACKAGE__->new(
      attribute        => $self->attribute,
      debuffConditions =>
        [$self->debuffConditions ? $self->debuffConditions->@* : ()],
      buffConditions =>
        [$self->buffConditions ? $self->buffConditions->@* : ()],
      passive => $self->passive,
      value   => $self->value->clone,
    );

    if (!defined $self->targetedType) {
      $copy->set_target(undef);
    }
    elsif (ref($self->targetedType) eq 'ARRAY') {
      $copy->set_target([$self->targetedType->@*]);
    }
    else {
      $copy->set_target($self->targetedType);    # scalar
    }

    return $copy;
  }

  sub conditions ($self) {
    my @result = ();

    # Check if debuffConditions exists and is an array reference
    if (defined $self->debuffConditions
      && ref($self->debuffConditions) eq 'ARRAY') {
      push @result, $self->debuffConditions->@*;
    }

    # Check if buffConditions exists and is an array reference
    if (defined $self->buffConditions
      && ref($self->buffConditions) eq 'ARRAY') {
      push @result, $self->buffConditions->@*;
    }

    return \@result;
  }

  sub set_target ($self, $tt) {
    return unless defined($tt);
    if (ref($tt) eq 'ARRAY') {
      if (defined($self->targetedType)) {
        if (ref($self->targetedType) eq 'ARRAY' or length($self->targetedType))
        {
          $self->log_warn(sprintf(
            'warning, overwriting existing value "%s" with "%s"',
            $self->targetedType,
            ref($tt) eq 'ARRAY'
            ? join ', ',
              $tt->@*
            : $tt,
          ));
        }
        else {
          $self->log_error(
            sprintf('unexpected targedType: "%s"', ref($self->targetedType)));
        }
      }
      foreach my $ttv ($tt->@*) {
        unless (any { $_ eq $tt } values $self->TroopTypeValues->%*) {
          $self->log_error(
            sprintf('illegal value in new targetedType value: %s', $ttv));
          return;
        }
      }
      $self->targetedType = $tt;
    }
    elsif (any { $_ eq $tt } values $self->TroopTypeValues->%*) {
      if (defined($self->targetedType)) {
        if (ref($self->targetedType) eq 'ARRAY' or length($self->targetedType))
        {
          $self->log_warn(sprintf(
            'warning, overwriting existing value "%s" with "%s"',
            $self->targetedType,
            ref($tt) eq 'ARRAY'
            ? join ', ',
              $tt->@*
            : $tt,
          ));
        }
        else {
          $self->log_error(
            sprintf('unexpected targedType: "%s"', ref($self->targetedType)));
        }
      }
      $self->targetedType($tt);
    }
  }

  sub set_condition ($self, $condition) {

    if ($self->attribute eq 'March Size') {
      # march size *cannot* take a condition
      $self->log_info(
        'skipping non-operative conditon on March Size attribute');
      return 1;
    }

    if (ref($condition) && ref($condition) eq 'ARRAY') {
      foreach my $c ($condition->@*) {
        $self->set_condition($c);
      }
      return;
    }

    # Check if the condition is a valid buff condition
    if (any { $condition eq $_ } keys $self->BuffConditionValues->%*) {

      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } $self->buffConditions->@*) {
        push @{ $self->buffConditions }, $condition;
      }

      $self->log_debug("Added buff condition: $condition");
      return 1;
    }

    # Check if the condition is a valid debuff condition
    if (any { $condition eq $_ } $self->DebuffConditionValues->@*) {
      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } $self->debuffConditions->@*) {
        push @{ $self->debuffConditions }, $condition;
      }

      $self->log_debug("Added debuff condition: $condition");
      return 1;
    }

    # If we get here, the condition wasn't valid
    $self->log_error(
      "Invalid condition: '$condition'. Must be one of: "
        . join(", ",
        keys $self->BuffConditionValues->%*,
        $self->DebuffConditionValues->@*)
    );
    return 0;
  }

  sub validate ($self) {
    my @errors;
    my @tbc = $self->buffConditions->@*;
    my @invalid;

    # Check if we got an array reference instead of a flat array
    if (@tbc == 1 && ref($tbc[0]) eq 'ARRAY') {
      $self->log_error("needed to flatten buffConditions"
          . Data::Printer::np($self->buffConditions));
      @tbc = @{ $tbc[0] };    # Flatten it
    }

    # Find elements in @tbc that are not in the valid conditions
    my %valid_conditions = map { $_ => 1 } keys %{ $self->BuffConditionValues };
    @invalid = grep { !exists $valid_conditions{$_} } @tbc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf('Detected illegal value "%s" in buffConditions. '
            . 'All values must be one of: %s',
          $iv, join(', ', keys %{ $self->BuffConditionValues }));
      }
    }

    my @tdc = @{ $self->debuffConditions };

    # Check if we got an array reference instead of a flat array
    if (@tdc == 1 && ref($tdc[0]) eq 'ARRAY') {
      @tdc = @{ $tdc[0] };    # Flatten it
      $self->log_error("needed to flatten debuffConditions");
    }

    # Find elements in @tdc that are not in the valid debuff conditions
    my %valid_debuff_conditions =
      map { $_ => 1 } @{ $self->DebuffConditionValues };
    @invalid = grep { !exists $valid_debuff_conditions{$_} } @tdc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf('Detected illegal value "%s" in debuffConditions. '
            . 'All values must be one of: %s',
          $iv, join(', ', @{ $self->DebuffConditionValues }));
      }
    }

    unless ($self->passive == 0 || $self->passive == 1) {
      push @errors, "passive must be 0 or 1, not $self->passive";
    }

    if (@errors) {
      $self->log_logcroak(join ', ', @errors);
      return;
    }

    $self->value->validate();

  }

  sub has_targetedType ($self) {
    if (ref($self->targetedType) eq 'ARRAY') {
      return scalar @{ $self->targetedType };
    }
    return length($self->targetedType);
  }

  sub match_buff (
    $self, $test_attribute,
    $test_targetedType     = '',
    $test_buffConditions   = [],
    $test_debuffConditions = [],
  ) {
    my $no_op_Conditions = [
      "brings a dragon",
      'brings a sacred dragon',
      'brings a spiritual beast',
      'During SvS',
      'leading the army',
      'you own the General',
    ];
    # Override empty array with defaults
    if (scalar @$test_buffConditions == 0) {
      #$test_buffConditions = [
      #  "Attacking",
      #  "brings a dragon",
      #  "brings dragon or beast to attack",
      #  "dragon to the attack",
      #  "leading the army to attack",
      #  "Marching",
      #  "When Rallying",
      #];
      $self->log_warn("Empty buff conditions provided!!");
    }

    $self->log_debug(sprintf(
      'Checking match for buff: attr=%s, targetType=%s, '
        . 'buff conditions=%s, debuff conditions=%s',
      $self->attribute,
      $self->targetedType,
      join(', ',
        ref $self->buffConditions eq 'ARRAY' ? @{ $self->buffConditions } : ()),
      join(', ',
        ref $self->debuffConditions eq 'ARRAY'
        ? @{ $self->debuffConditions }
        : ()),
    ));

    # Check attribute match
    return 0 unless $self->attribute eq $test_attribute;

    # Check targetedType match if provided
    if (length($test_targetedType) && length($self->targetedType)) {
      # targetType often comes from generals, convert it for use here.
      if (exists $self->GeneralTypes2TroopTypes->{$test_targetedType}) {
        $test_targetedType =
          $self->GeneralTypes2TroopTypes->{$test_targetedType};
        $self->log_debug(
          "Normalized test_targetedType to '$test_targetedType'");
      }
      else {
        $self->log_warn(
          "Unrecognized general specialist key: $test_targetedType");
        return 0;
      }

      # Match against the buff's targetedTypes
      if ($self->targetedType ne $test_targetedType) {
        $self->log_debug(sprintf(
          '  ✗ Rejected: test_targetedType "%s" not matched by "%s"',
          $test_targetedType, $self->targetedType
        ));
        return 0;
      }
    }
    elsif (ref($self->targetedType) eq 'ARRAY') {
      my $match = 0;
      foreach my $tt ($self->targetedType->@*) {
        if ($tt eq $test_attribute) {
          $match = 1;
          last;
        }
      }
      if ($match == 0) {
        $self->log_debug(sprintf(
          '  ✗ Rejected: test_targetedType "%s" not matched by "%s"',
          $test_targetedType, join ', ', $self->targetedType->@*
        ));
        return 0;
      }
    }

    # Check debuff conditions
    my $has_debuff_conditions = scalar @{ $self->debuffConditions } > 0;

    # If test_debuffConditions is empty, reject any buff with debuff conditions
    if (scalar @$test_debuffConditions == 0) {
      if ($has_debuff_conditions) {
        $self->log_debug(
          "  ✗ Rejected: buff has debuff conditions but none were requested");
        return 0;
      }
    }
    # If test_debuffConditions is provided,
    # check that all debuff conditions are in the allowed list
    elsif ($has_debuff_conditions) {
      foreach my $condition ($self->debuffConditions->@*) {
        if (none { $_ eq $condition } @$test_debuffConditions) {
          $self->log_debug(
            "  ✗ Rejected: debuff condition '$condition' not in allowed list");
          return 0;
        }
      }
    }
    else {
      $self->log_debug("  ✗ Rejected: debuff conditions are not "
          . "present in buff and are required.");
      return 0;
    }

    # Check buff conditions
    my $has_buff_conditions = scalar @{ $self->buffConditions } > 0;

    # If test_buffConditions is provided,
    # check that all buff conditions are in the allowed list
    my %allowed_conditions;
    if (scalar @$test_buffConditions > 0) {
      %allowed_conditions =
        map { $_ => 1 } ($test_buffConditions->@*, $no_op_Conditions->@*);
    }
    else {
      %allowed_conditions = map { $_ => 1 } $no_op_Conditions->@*;
    }

    # A buff with no conditions should match when conditions are specified
    if (!$has_buff_conditions) {
      $self->log_debug(
        "  ✓ Buff has no conditions, accepting unconditional buff");
      # Continue to the end of the function
    }
    else {
      foreach my $condition ($self->buffConditions->@*) {
        if (!exists $allowed_conditions{$condition}) {
          $self->log_debug(
            "  ✗ Rejected: buff condition '$condition' not in allowed list");
          return 0;    #####<---- Line 220
        }
      }
    }

    $self->log_debug("  ✓ Buff matched");
    return 1;
  }

  sub from_hash ($class, $hashref) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    my $v      = Game::EvonyTKR::Model::Buff::Value->new(
      number => abs($hashref->{value}->{number}),
      unit   => ($hashref->{value}->{unit} // 'percentage'),
    );
    my $r = Game::EvonyTKR::Model::Buff->new(
      attribute => $hashref->{attribute},
      passive   => $hashref->{passive} // 0,
      value     => $v,
    );
    if (exists $hashref->{targetedType}) {
      $logger->debug(sprintf(
        'found targetedType in hashref: "%s"',
        defined($hashref->{targetedType})
        ? ref($hashref->{targetedType})
            ? join ', ',
            map { sprintf('"%s"', $_) } $hashref->{targetedType}->@*
            : $hashref->{targetedType}
        : 'undef'
      ));
      $r->set_target($hashref->{targetedType});
    }
    if (exists $hashref->{targetedTroops}) {
      $logger->debug(sprintf(
        'found targetedTroops in hashref: "%s"',
        defined($hashref->{targetedTroops})
        ? ref($hashref->{targetedTroops})
            ? join ', ',
            map { sprintf('"%s"', $_) } $hashref->{targetedTroops}->@*
            : $hashref->{targetedTroops}
        : 'undef'
      ));
      $r->set_target($hashref->{targetedTroops});
    }
    if (exists $hashref->{troop}) {
      $logger->debug(
        sprintf('found troop in hashref: "%s"', $hashref->{troop}));
      $r->set_target($hashref->{troop});
    }
    if (exists $hashref->{condition}) {
      foreach my $c (@{ $hashref->{conditions} }) {
        $r->set_condition($c);
      }
    }
    elsif (exists $hashref->{conditions}) {
      foreach my $c (@{ $hashref->{conditions} }) {
        $r->set_condition($c);
      }
    }
    return $r;
  }

  sub to_hash ($self) {
    my $c;
    my $conditionCount = scalar @{ $self->conditions() };
    $self->log_debug(
      sprintf('in to_hash, I have %s conditions.', 0+ $conditionCount));

    # Debug buff value
    if (defined($self->value)) {
      $self->log_debug(sprintf(
        "Buff value: number=%s, unit=%s",
        $self->value->number // 'undef',
        $self->value->unit   // 'undef'
      ));
    }
    else {
      $self->log_warn("Buff value is undefined!");
    }

    my $rc;
    if ($conditionCount) {
      $rc = $self->conditions();
    }
    else {
      $c = [];
    }
    my $r = {
      __CLASS__    => __PACKAGE__,
      attribute    => $self->attribute,
      value        => $self->value,    # Let JSON role handle the blessed object
      passive      => $self->passive,
      targetedType => $self->targetedType,
      conditions   => $rc,
    };
    return $r;
  }

  sub as_string ($self, @args) {
    my $json =
      JSON::PP->new->utf8->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub to_wire_hash ($self) {
    return $self->to_hash();    # to_hash already has everything we need
  }

  sub from_wire_hash ($class, $w) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    my $buff   = $class->new(
      attribute    => $w->{attribute},
      passive      => $w->{passive} // 0,
      targetedType => $w->{targetedType},
    );

    # Set value
    if ($w->{value}) {
      $buff->value(Game::EvonyTKR::Model::Buff::Value->new(
        number => $w->{value}->{number},
        unit   => $w->{value}->{unit},
      ));
    }

    # Set conditions
    if ($w->{conditions} && @{ $w->{conditions} }) {
      foreach my $condition (@{ $w->{conditions} }) {
        $buff->set_condition($condition);
      }
    }
    $logger->debug(sprintf(
      'from_wire_hash returning a %s buff with '
        . 'attribute "%s"; '
        . 'targetedType "%s" '
        . 'conditions %s '
        . ' and value %s.',
      $buff->passive ? 'passive' : '',
      $buff->attribute,
      ($buff->targetedType // ''),
      (join ', ', map { sprintf('"%s"', $_) } $buff->conditions->@* // ''),
      sprintf("%s%s",
        $buff->value->number,
        $buff->value->unit eq 'percentage' ? '%' : ' flat'),
    ));
    return $buff;
  }

  sub concat($self, $other, $swap) {
    if ($swap) {
      return $other . $self->as_string();
    }
    else {
      return $self->as_string() . $other;
    }
  }

  sub _isTrue ($self, $other = undef, $swap = undef) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa(__PACKAGE__);
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

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Util::Buff;
require Game::EvonyTKR::Shared::Constants::BuffConstants;
require Game::EvonyTKR::Role::Logger;
use namespace::autoclean;

package Game::EvonyTKR::Model::Buff {
  use Mojo::Base -base,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Mojo::Base 'Game::EvonyTKR::Util::Buff',   -role;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants', -role;

  use List::AllUtils qw( any none );
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';

  has ['attribute', 'targetedType'] => undef;
  has 'value' => sub { Game::EvonyTKR::Model::Buff::Value->new() };
  has ['debuffConditions', 'buffConditions'] => sub { [] };
  has ['passive', 'DISABLED']                => 0;

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
          $self->logger->warn(sprintf(
            'warning, overwriting existing value "%s" with "%s"',
            $self->targetedType,
            ref($tt) eq 'ARRAY'
            ? join ', ',
              $tt->@*
            : $tt,
          ));
        }
        else {
          $self->logger->error(
            sprintf('unexpected targedType: "%s"', ref($self->targetedType)));
        }
      }
      foreach my $ttv ($tt->@*) {
        unless (any { $_ eq $tt } values $self->TroopTypeValues->%*) {
          $self->logger->error(
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
          $self->logger->warn(sprintf(
            'warning, overwriting existing value "%s" with "%s"',
            $self->targetedType,
            ref($tt) eq 'ARRAY'
            ? join ', ',
              $tt->@*
            : $tt,
          ));
        }
        else {
          $self->logger->error(
            sprintf('unexpected targedType: "%s"', ref($self->targetedType)));
        }
      }
      $self->targetedType = $tt;
    }
  }

  sub set_condition ($self, $condition) {

    print STDERR "DEBUG: set_condition called with: $condition\n";
    print STDERR "DEBUG: logger method exists: " . (defined $self->can('logger') ? 'YES' : 'NO') . "\n";

    if ($self->attribute eq 'March Size') {
      # march size *cannot* take a condition
      $self->logger->info(
        'skipping non-operative conditon on March Size attribute');
      return 1;
    }

    # Check if the condition is a valid buff condition
    if (any { $condition eq $_ } keys $self->BuffConditionValues->%*) {

      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } $self->buffConditions->@*) {
        push @{ $self->buffConditions }, $condition;
      }

      $self->logger->debug("Added buff condition: $condition");
      return 1;
    }

    # Check if the condition is a valid debuff condition
    if (any { $condition eq $_ } $self->DebuffConditionValues->@*) {
      # Add the condition if it's not already there
      unless (grep { $_ eq $condition } $self->debuffConditions->@*) {
        push @{ $self->debuffConditions }, $condition;
      }

      $self->logger->debug("Added debuff condition: $condition");
      return 1;
    }

    # If we get here, the condition wasn't valid
    $self->logger->error(
      "Invalid condition: '$condition'. Must be one of: "
        . join(", ",
        keys $self->BuffConditionValues->%*,
        $self->DebuffConditionValues->@*)
    );
    return 0;
  }

  sub to_hash ($self) {
    my $c;
    my $conditionCount = scalar @{ $self->conditions() };
    $self->logger->debug("in to_hash, I have $conditionCount conditions");
    my $rc;
    if ($conditionCount) {
      $rc = $self->conditions();
    }
    else {
      $c = [];
    }
    my $r = {
      attribute => $self->attribute,
      value     => {
        number => $self->value->number(),
        unit   => $self->value->unit(),
      },
      passive      => $self->passive,
      targetedType => $self->targetedType,
      conditions   => $rc,
    };
    return $r;
  }

  sub TO_JSON ($self) {
    return $self->to_hash();
  }

  sub as_string ($self) {
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub concat($self, $other, $swap) {
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

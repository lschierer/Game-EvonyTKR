use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Model::Buff :isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::Buff
  use List::AllUtils qw( any none );
  use UUID qw(uuid5);
  use Mojo::JSON qw (encode_json);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'  => \&to_String;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $attribute :reader :param;

  field $value :reader :param;

  field $debuffConditions :reader :param //= [];

  field $buffConditions :reader :param //= [];

  field $targetedTypes :reader :param // = [];

  method conditions() {
    return (@{$self->debuffConditions()}, @{$self->buffConditions()});
  }

  field $passive :reader :param //= 0;

  method validate() {
    my @errors;
    for my $c (@{$self->buffConditions()} ) {
      if ( none { $_ =~ /$c/i } $self->$self->buffConditionValues()) {
        push @errors, sprintf('Detected illegal value "%s" in buffConditions.  All values must be one of "%s"',
          $c, Data::Printer::np($self->buffConditionValues()));
      }
    }
    for my $c (@{$self->debuffConditions()} ) {
      if ( none { $_ =~ /$c/i } $self->$self->debuffConditionValues()) {
        push @errors, sprintf('Detected illegal value "%s" in debuffConditions.  All values must be one of "%s"',
          $c, Data::Printer::np($self->debuffConditionValues()));
      }
    }
    if ( none { $_ =~ /$attribute/i } $self->BuffAttributes() ) {
      push @errors, sprintf('Detected illegal value %s as attribute. You must use one of %s.',
        $attribute, Data::Printer::np($self->BuffAttributes()));
    }

    for my $t ( @{ $self->targetedTypes() }) {
      if ( none { $_ =~ /$t/i } $self->targetedTypeValues() ) {
        push @errors, sprintf(
        'Detected illegal value %s in targetedTypes.  All values must be one of %s.',
          $t, Data::Printer::np($self->targetedTypeValues()));
      }
    }

    my $type = t('BoolLike');
    $type->check($passive)
      or push @errors => "passive must be 0 or 1, not $passive";

    if(@errors) {
      $self->logger()->logcroak(join(', ' => @errors));
    }

    $value->validate();

  }

  ADJUST {
    $self->validate();
  }

  TO_JSON() {
    return {
      attribute     => $self->attribute(),
      value         => {
        number      => $self->value()->number(),
        unit        => $self->value()->unit(),
      },
      passive       => $self->passive(),
      targetedTypes => $self->targetedTypes(),
      conditions    => $self->conditions(),
    };
  }

  method to_String() {
    $self->TO_JSON();
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

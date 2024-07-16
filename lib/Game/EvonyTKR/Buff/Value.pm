package Game::EvonyTKR::Buff::Value;
use 5.38.0;
use Moose;
use Moose::Util::TypeConstraints;
use Data::Dumper qw(Dumper);
use strict;
use warnings;
use namespace::autoclean;

# ABSTRACT: Values for Evony TKR Buffs

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for A Buff needs a (numeric) Value to be meaningful.  This may be either a flat or percentage based value.

Flat buffs are simply added to the attribute they enhance.  To date, I am unaware of any flat debuffs in the game.

Percentage based buffs are multiplied against the base value for that attribute.  Thus if you have multiple percentage
based buffs, you are not going to experience exponential growth, since each will be multiplied against the same base value
before they are then added together. 

=cut

# extends, roles, attributes, etc.

subtype 'ValueType'
  => as Str
  => where {
    $_ eq 'flat' or $_ eq 'percentage'
  }
  => message {
    "($_) is not a valid ValueType.  Valid values are 'flat' and 'percentage'."
  };

has 'number' => (
  is  => 'ro',
  isa => 'Int'
);

has 'unit' => (
  is  => 'ro',
  isa => 'ValueType'
);

1;

use v5.40.0;
use experimental qw(class);
use utf8::all;

use FindBin;
require JSON::PP;
use lib "$FindBin::Bin/../../../lib";
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;

class Game::EvonyTKR::Model::Specialty : isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::Specialty

  use Carp;
  use Data::Printer;

  use List::MoreUtils;
  use Util::Any -all;
  use UUID qw(uuid5);
  use namespace::autoclean;
# VERSION
  use Game::EvonyTKR::Model::Logger;
  use overload
    '""'       => \&as_string,
    'bool'     => sub { $_[0]->_isTrue() },
    "fallback" => 0;

  field $id : reader;
  field $name : reader : param;

  field $levels : reader;

  ADJUST {
    my $step1 = {};

    foreach my $key ($self->specialtyLevels->@*) {
      $step1->{$key} = {
        level => $key,
        buffs => [],     # Empty arrayref that can be modified later
      };
    }

# Use Readonly::Hash1 for shallow readonly - hash structure is fixed but array contents can change
    Readonly::Hash1 my %step2 => %{$step1};
    $levels = \%step2;
  }

  ADJUST {
    my $specialtybase = uuid5($self->UUID5_base, 'Specialty');
    $id = uuid5($specialtybase, $name);
  }

  method get_buffs_at_level (
    $level, $attribute,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    $level = lc($level)
      ;    # sanitize the data from the user - level names must be lower case
    my $logger = $self->logger;
    $logger->debug(
      "Calculating buffs for level: $level, attribute: $attribute");

    return 0 if not defined $level or $level =~ /none/i;

    # an list's values function will always return in the same order.
    my $valid_levels = $self->specialtyLevels;
    # Define the hierarchy of levels
    my @level_hierarchy = $self->specialtyLevels->@*;

    # Find the index of the requested level
    my $level_index = -1;
    for my $i (0 .. $#level_hierarchy) {
      if ($level_hierarchy[$i] eq $level) {
        $level_index = $i;
        last;
      }
    }

    return 0 if $level_index <= 0;    # 'none' or invalid level

    my $total          = 0;
    my $levels_by_name = $self->levels;

    # Accumulate buffs from all levels up to and including the specified level
    for my $i (1 .. $level_index) {    # Start from 1 to skip 'none'
      my $current_level = $level_hierarchy[$i];
      my $buffs         = $levels_by_name->{$current_level}->{buffs} // [];

      $logger->debug(
        "Checking level $current_level with " . scalar(@{$buffs}) . " buffs");

      foreach my $buff (@$buffs) {
        if ($buff->match_buff(
          $attribute, $targetedType, $conditions, $debuffConditions
        )) {
          my $val = $buff->value->number;
          $logger->debug(
            "  ➤ Match found at level $current_level. Adding $val to total.");
          $total += $val;
        }
      }
    }

    $logger->debug("Total for $level/$attribute: $total");
    return $total;

  }

  method addBuff ($level, $nb) {

    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      $self->logger->error(sprintf(
        'attempting to add buff of type %s not "Game::EvonyTKR::Model::Buff"',
        !blessed($nb) ? Scalar::Util::reftype($nb) : blessed($nb)));
      exit 0;
    }

    # the data files apparently have bad cases in them for level names.
    if (none { $_ =~ /$level/i } $self->specialtyLevels->@*) {
      $self->logger->error(sprintf(
        'level should be one of %s, not %s',
        Data::Printer::np($self->specialtyLevels->@*), $level
      ));
      return 0;
    }
    $level = lc($level);
    push @{ $levels->{$level}->{buffs} }, $nb;
    $self->logger->debug(sprintf(
      'specialty %s at %s now has buffs %s.',
      $name, $level, Data::Printer::np($levels->{$level}->{buffs})
    ));
    return scalar @{ $levels->{$level}->{buffs} };
  }

  method to_hash {
    return {
      id     => $id,
      name   => $name,
      levels => $levels,
    };
  }

  # Method for JSON serialization
  method TO_JSON {
    return $self->to_hash();
  }

  # Stringification method using JSON
  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

}

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR Specialties.

=head1 DESCRIPTION

A Specialty is one of several ways that a General can provide Buffs for Troops.

=cut

=method name()

returns the name field from the Specialty.
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Specialty at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method levels()

Returns a hash with the levels None, Green, Blue, Purple, Orange, or Gold as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

The levels are *not* cumulative, and must be added by the consumer.
=cut

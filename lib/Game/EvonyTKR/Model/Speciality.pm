use v5.40.0;
use experimental qw(class);
use utf8::all;

use FindBin;
require JSON::PP;
use lib "$FindBin::Bin/../../../lib";
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;

class Game::EvonyTKR::Model::Speciality : isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::Speciality

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
    my $step1                 = {};
    my $specialityLevelsNames = $self->specialityLevels->unique_values;
    foreach my $key (@{$specialityLevelsNames}) {
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
    my $specialitybase = uuid5($self->UUID5_base, 'Speciality');
    $id = uuid5($specialitybase, $name);
  }

  method get_buffs_at_level (
    $level, $attribute,
    $targetedType = '',
    $conditions   = [],
    $debuff       = 0
  ) {
    $level = lc($level)
      ;    # sanitize the data from the user - level names must be lower case
    my $logger = $self->logger;
    $logger->debug(
      "Calculating buffs for level: $level, attribute: $attribute");

    # an enum's values function will always return in the same order.
    my $valid_levels = $self->specialityLevels->values;
    my %level_index  = map { $valid_levels->[$_] => $_ } 0 .. $#$valid_levels;

    unless (exists $level_index{$level}) {
      $logger->debug("Invalid level: $level");
      return 0;
    }

    return 0 if $level eq 'none';

    my $buffs_by_level = $self->levels;
    my $target_index   = $level_index{$level};

    my $total = 0;

    for my $i (1 .. $target_index) {    # skip index 0 ('none')
      my $level_name = $valid_levels->[$i];
      my $buffs      = $buffs_by_level->{$level_name}->{buffs} // [];

      $logger->debug(
        "Checking level '$level_name' with " . scalar(@$buffs) . " buffs");

      foreach my $buff (@$buffs) {
        if ($buff->match_buff($attribute, $targetedType, $conditions, $debuff))
        {
          my $val = $buff->value->number;
          $logger->debug("  ➤ Match found. Adding $val to total.");
          $total += $val;
        }
      }
    }

    $logger->debug(
      "$name has Total for level '$level' and attribute '$attribute': $total");
    return $total;
  }

  method addBuff ($level, $nb) {

    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      $self->logger->error(sprintf(
        'attempting to add buff of type %s not "Game::EvonyTKR::Model::Buff"',
        !blessed($nb) ? Scalar::Util::reftype($nb) : blessed($nb)));
      exit 0;
    }
    my $specialityLevelsNames = $self->specialityLevels->unique_values;

    # the data files apparently have bad cases in them for level names.
    if (none { $_ =~ /$level/i } @{$specialityLevelsNames}) {
      $self->logger->error(sprintf(
        'level should be one of %s, not %s',
        Data::Printer::np(@{$specialityLevelsNames}), $level
      ));
      return 0;
    }
    $level = lc($level);
    push @{ $levels->{$level}->{buffs} }, $nb;
    $self->logger->debug(sprintf(
      'speciality %s at %s now has buffs %s.',
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

# ABSTRACT: Module for processing information about Evony TKR Specialities.

=head1 DESCRIPTION

A Speciality is one of several ways that a General can provide Buffs for Troops.

=cut

=method name()

returns the name field from the Speciality.
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Speciality at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method levels()

Returns a hash with the levels None, Green, Blue, Purple, Orange, or Gold as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

The levels are *not* cumulative, and must be added by the consumer.
=cut

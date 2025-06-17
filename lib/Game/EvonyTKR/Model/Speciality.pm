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
    "fallback" => 1;

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

  field $activeLevel : reader : param //= 'None';

  ADJUST {
    my $specialitybase = uuid5($self->UUID5_base, 'Speciality');
    $id = uuid5($specialitybase, $name);
  }

  method setActiveLevel ($newLevel) {
    my $specialityLevelsNames = $self->specialityLevels->unique_values;
    if (none { $_ eq $newLevel } @{$specialityLevelsNames}) {
      return 0;
    }
    $activeLevel = $newLevel;
    return 1;
  }

  method addBuff ($level, $nb) {
    my $red = 1;
    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      exit 0;
    }
    my $specialityLevelsNames = $self->specialityLevels->unique_values;
    if (none { $_ eq $level } @{$specialityLevelsNames}) {
      return 0;
    }
    push @{ $levels->{$level}->{buffs} }, $nb;
    return scalar @{ $levels->{$level}->{buffs} };
  }

  method toHashRef {
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
      my $json = JSON::PP->new->utf8->pretty->allow_blessed(1)->convert_blessed(1)->encode($self->to_hash());
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

=method activeLevel

returns the level, values None, Green, Blue, Purple, Orange, or Gold, that is active at this time.
=cut

=method setActiveLevel($newLevel)

sets the activeLevel to newLevel presuming it is a valid value None, Green, Blue, Purple, Orange, or Gold.

=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Speciality at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method levels()

Returns a hash with the levels None, Green, Blue, Purple, Orange, or Gold as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

The levels are *not* cumulative, and must be added by the consumer.
=cut

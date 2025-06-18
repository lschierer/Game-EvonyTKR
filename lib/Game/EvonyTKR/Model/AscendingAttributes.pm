use v5.40.0;
use experimental qw(class);
use utf8::all;

use FindBin;
require JSON::PP;
use lib "$FindBin::Bin/../../../lib";
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;

class Game::EvonyTKR::Model::AscendingAttributes :
  isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::AscendingAttributes

  use Carp;
  use Data::Printer;
  require Readonly;
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
  field $general : reader : param;

  field $ascending : reader;

  ADJUST {
    my $step1 = {};
    foreach my $key (@{ $self->AscendingLevelNames(0) },
      @{ $self->AscendingLevelNames(1) }) {
      $step1->{$key} = {
        level => $key,
        buffs => [],     # Empty arrayref that can be modified later
      };
    }

# Use Readonly::Hash1 for shallow readonly - hash structure is fixed but array contents can change
    Readonly::Hash1 my %step2 => %{$step1};
    $ascending = \%step2;
  }

  ADJUST {
    my $ascendingbase = uuid5($self->UUID5_base, 'Ascending Attributes');
    $id = uuid5($ascendingbase, $general);
  }

  method get_buffs_at_level (
    $level, $attribute,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = []
  ) {
    my $logger = $self->logger;
    $logger->debug(
      "Calculating ascending buffs for level: $level, attribute: $attribute");

    return 0 if not defined $level or $level eq 'none';

    my ($is_red, $valid_levels);
    if ($level =~ /^red\d$/) {
      $is_red       = 1;
      $valid_levels = $self->AscendingLevelNames(1);    # red
    }
    elsif ($level =~ /^purple\d$/) {
      $is_red       = 0;
      $valid_levels = $self->AscendingLevelNames(0);    # purple
    }
    else {
      $logger->warn("Invalid ascending level: $level");
      return 0;
    }

    my %level_index = map { $valid_levels->[$_] => $_ } 0 .. $#$valid_levels;
    return 0 unless exists $level_index{$level};

    my $target_index = $level_index{$level};
    my $levels_by_name =
      $self->ascending;    # { red1 => [...], purple1 => [...], ... }

    my $total = 0;
    for my $i (1 .. $target_index) {    # skip index 0 ('None')
      my $lvl   = $valid_levels->[$i];
      my $buffs = $levels_by_name->{$lvl}->{buffs} // [];

      $logger->debug(
        "Checking level $lvl with " . scalar(@{$buffs}) . " buffs");

      foreach my $buff (@$buffs) {
        if ($buff->match_buff(
          $attribute, $targetedType, $conditions, $debuffConditions
        )) {
          my $val = $buff->value->number;
          $logger->debug("  ➤ Match found. Adding $val to total.");
          $total += $val;
        }
      }
    }

    $logger->debug("Total for $level/$attribute: $total");
    return $total;
  }

  method addBuff ($level, $nb) {
    my $red = 1;
    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      $self->logger->error(sprintf(
        'attempting to add buff of type %s not "Game::EvonyTKR::Model::Buff"',
        !blessed($nb) ? Scalar::Util::reftype($nb) : blessed($nb)));
      exit 0;
    }

    if ($level !~ /(purple|red)[0-9]{1}/i) {
      $self->logger->error(sprintf(
        'level should be one of %s, not %s',
        Data::Printer::np((
          $self->AscendingLevelNames(0), $self->AscendingLevelNames(1))),
        $level
      ));
      return 0;
    }
    if ($level =~ /purple/i) {
      $red = 0;
    }
    if (none { $_ eq $level } @{ $self->AscendingLevelNames($red) }) {
      return 0;
    }

    push @{ $ascending->{$level}->{buffs} }, $nb;
    return scalar @{ $ascending->{$level}->{buffs} };
  }

  method to_hash {
    return {
      id        => $id,
      general   => $general,
      ascending => $ascending,
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

Ascending is one of several ways that a General can provide Buffs for Troops.

Ascending works similarly to Specialities in that there are multiple levels, however there is only one set of Ascending Buffs per general, instead of the four possible Specialities (plus possible Flex Speciality).

=for :List

* Ascending Buffs can be either values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

* Ascending Buffs 1Red through 5Red provide ehancements to Basic skills and affect the General's effective scores for the basic attributes.

* Ascending a Historic (Gold/Red) General costs both general fragments and Blood of Ares.  Ascending a General that is merely Historic but not Legendary (Purple) requires general fragments, but not Blood of Ares.

* For low spenders, Blood of Ares is a severe limiting factor on ascending generals.  As you grow in spending, it limits the I<rate> at which you ascend, but not I<if> you ascend a general.  Fragments are the true limiting concern for players of all levels.  It is not possible to get sufficient fragments of all generals to fully ascend them without spending in any reasonable amount of time.  It is not possible to ascend certain "retired" generals I<at all> if you have not already done so.  These "retired" Generals are included in the distribution data to allow players who I<do> have them to compare effectiveness in using them.
=cut

=method name()

returns the name field from the Speciality.
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Speciality at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method Buffs()

Returns a hash with the levels 1Purple through 5Purple, and 1Red through 5Red as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

Each level is cumulative, you never need to read more than the array for the currently active level.
=cut

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR Specialities.

=head1 DESCRIPTION

Ascending is one of several ways that a General can provide Buffs for Troops.

Ascending works similarly to Specialities in that there are multiple levels, however there is only one set of Ascending Buffs per general, instead of the four possible Specialities (plus possible Flex Speciality).

=for :List

* Ascending Buffs can be either values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

* Ascending Buffs 1Red through 5Red provide ehancements to Basic skills and affect the General's effective scores for the basic attributes.

* Ascending a Historic (Gold/Red) General costs both general fragments and Blood of Ares.  Ascending a General that is merely Historic but not Legendary (Purple) requires general fragments, but not Blood of Ares.

* For low spenders, Blood of Ares is a severe limiting factor on ascending generals.  As you grow in spending, it limits the I<rate> at which you ascend, but not I<if> you ascend a general.  Fragments are the true limiting concern for players of all levels.  It is not possible to get sufficient fragments of all generals to fully ascend them without spending in any reasonable amount of time.  It is not possible to ascend certain "retired" generals I<at all> if you have not already done so.  These "retired" Generals are included in the distribution data to allow players who I<do> have them to compare effectiveness in using them.
=cut

=method name()

returns the name field from the Speciality.
=cut

=method activeLevel

returns the level, values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals), that is active at this time.
=cut

=method setActiveLevel($newLevel)

sets the activeLevel to newLevel presuming it is a valid value 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

Todo: Make sure the value is valid I<for the General's quality>.
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Speciality at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method Buffs()

Returns a hash with the levels 1Purple through 5Purple, and 1Red through 5Red as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

Each level is cumulative, you never need to read more than the array for the currently active level.
=cut

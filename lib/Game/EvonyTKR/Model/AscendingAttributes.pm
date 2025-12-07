use v5.42.0;
use utf8::all;

use File::FindLib 'lib';
require JSON::PP;
require Data::Printer;
require List::AllUtils;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::Buff::Matcher;
use namespace::autoclean;

package Game::EvonyTKR::Model::AscendingAttributes {
  use Mojo::Base "Game::EvonyTKR::Model::Base";
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',       -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use Carp;
  use Data::Printer;
  use List::AllUtils qw (none);
  use UUID           qw(uuid5);
  use Hash::Util     qw(lock_keys);
  use namespace::autoclean;
# VERSION
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    "fallback" => 0;

  has 'id' => sub ($self) {
    if (defined($self) && defined($self->UUID5_base)) {
      my $aabase = uuid5($self->UUID5_base, 'Ascending Attributes');
      if (defined($self->general)) {
        return uuid5($aabase, $self->general);
      }
    }
    return '';
  };

  has 'general' => '';

  has 'attributes' => sub ($self) {
    return $self->_init_empty_levels();
  };

  sub _init_empty_levels ($self) {
    my %h = map { $_ => { text => '', buffs => [], } } (
      $self->AscendingAttributeLevelValues(0),
      $self->AscendingAttributeLevelValues(1)
    );
    lock_keys(%h);
    $self->log_debug('h with empty levels is ' . Data::Printer::np(%h));
    return \%h;
  }

  sub get_buffs_at_level (
    $self, $level, $attribute,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    my $logger = $self->logger;
    $logger->debug(sprintf(
'Calculating ascending buffs for level: %s, attribute: %s, matching_type: %s',
      $level, $attribute, $matching_type,
    ));

    return 0 if not defined $level or $level eq 'none';

    my $is_red;
    my @valid_levels;
    if ($level =~ /^red\d$/) {
      $is_red       = 1;
      @valid_levels = $self->AscendingAttributeLevelValues(1);    # red
    }
    elsif ($level =~ /^purple\d$/) {
      $is_red       = 0;
      @valid_levels = $self->AscendingAttributeLevelValues(0);    # purple
    }
    else {
      $logger->warn("Invalid ascending level: $level");
      return 0;
    }

    my %level_index = map { $valid_levels[$_] => $_ } 0 .. $#valid_levels;
    return 0 unless exists $level_index{$level};

    my $target_index = $level_index{$level};
    $self->log_debug(
      "my ascending hash looks like " . Data::Printer::np($self->attributes));
    my $total = 0;
    for my $i (1 .. $target_index) {    # skip index 0 ('None')
      my $lvl = $valid_levels[$i];
      if (not exists $self->attributes->{$level}) {
        $logger->error(sprintf(
          '%s is not a valid level, must be one of %s',
          $lvl, join(', ', keys $self->attributes->%*),
        ));
      }
      my $buffs = $self->attributes->{$lvl}->{buffs} // [];

      $logger->debug(
        "Checking level $lvl with " . scalar(@{$buffs}) . " buffs");

      foreach my $buff (@$buffs) {
        my $logID = int(rand(9e12)) + 1e12;
        my $matcher =
          Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $buff);

        # Debug: Log what we're about to test
        $logger->debug(
          $logID
            . sprintf(
            'Testing buff: attr=%s, targetType=%s, '
              . 'buffConds=%s, debuffConds=%s, matching_type=%s',
            $buff->attribute,
            $buff->targetedType // 'no targetted type',
            join(',', @{ $buff->buffConditions   // [] }),
            join(',', @{ $buff->debuffConditions // [] }),
            $matching_type // 'no matching type'
            )
        );

        # For buff matching, don't pass debuff conditions
        # For debuff matching, don't pass buff conditions
        my ($match_buff_conditions, $match_debuff_conditions);
        if ($matching_type eq 'buff') {
          $match_buff_conditions   = $conditions;
          $match_debuff_conditions = [];
        }
        else {
          $match_buff_conditions   = $conditions;
          $match_debuff_conditions = $debuffConditions;
        }

        $logger->debug(
          $logID
            . sprintf(
            "Calling matcher with: buffConds=%s, debuffConds=%s",
            join(',', @$match_buff_conditions),
            join(',', @$match_debuff_conditions)
            )
        );

        if ($matcher->match(
          $attribute,             $targetedType,
          $match_buff_conditions, $match_debuff_conditions,
          $logID
        )) {
          my $val = $buff->value->number;
          $logger->debug("$logID  ➤ Match found. Adding $val to total.");
          $total += $val;
        }
        else {
          $logger->debug("$logID  ✗ No match found.");
        }
      }
    }

    $logger->debug("Total for $level/$attribute: $total");
    return $total;
  }

  sub addBuff ($self, $level, $nb) {
    my $red = 1;
    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      $self->log_error(sprintf(
        'attempting to add buff of type %s not "Game::EvonyTKR::Model::Buff"',
        !blessed($nb) ? Scalar::Util::reftype($nb) : blessed($nb)));
      exit 0;
    }

    if ($level !~ /(purple|red)[0-9]{1}/i) {
      $self->log_error(sprintf(
        'level should be one of %s, not %s',
        join(
          ', ',
          (
            $self->AscendingAttributeLevelValues(0),
            $self->AscendingAttributeLevelValues(1)
          )
        ),
        $level
      ));
      return 0;
    }
    if ($level =~ /purple/i) {
      $red = 0;
    }
    if (none { $_ eq $level } $self->AscendingAttributeLevelValues($red)) {
      $self->log_debug(
        "$level must be one of "
          . join(
          ', ',
          (
            $self->AscendingAttributeLevelValues(0),
            $self->AscendingAttributeLevelValues(1)
          )
          )
      );
      return 0;
    }

    push @{ $self->attributes->{$level}->{buffs} }, $nb;
    $self->log_debug("$level now has "
        . scalar @{ $self->attributes->{$level}->{buffs} }
        . " buffs");
    return scalar @{ $self->attributes->{$level}->{buffs} };
  }

  sub to_wire_hash ($self) {
    my $ascending_data = {};
    foreach my $lv (keys $self->attributes->%*) {
      my $l = $self->attributes->{$lv};
      $ascending_data->{$lv} = {
        text  => $l->{text} // '',
        buffs => [map { $_->to_wire_hash() } $l->{buffs}->@*],
      };
    }
    return {
      _v        => 1,
      id        => $self->id,
      general   => $self->general,
      ascending => $ascending_data,
    };
  }

  sub to_hash {
    my $self = shift;
    return {
      id        => $self->id,
      general   => $self->general,
      ascending => $self->attributes,
    };
  }

  # Method for JSON serialization
  sub TO_JSON {
    my $self = shift;
    return $self->to_hash();
  }

  # Stringification method using JSON
  sub as_string {
    my $self = shift;
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

  sub from_wire_hash($class, $h) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);

    # Convert wire format (hash-based ascending) back to array format
    my $converted_h = {%$h};    # shallow copy
    if (exists $h->{ascending} && ref($h->{ascending}) eq 'HASH') {
      # Convert hash format to array format expected by from_hash
      my @ascending_array = ();
      foreach my $level (sort keys %{ $h->{ascending} }) {
        next if $level eq 'none';    # Skip 'none' level
        my $level_data = $h->{ascending}->{$level};
        if ($level_data->{buffs} && @{ $level_data->{buffs} }) {
          push @ascending_array,
            {
            level => $level,
            buffs => $level_data->{buffs},
            text  => $level_data->{text} // '',
            };
        }
      }
      $converted_h->{ascending} = \@ascending_array;
    }

    my $aa = $class->from_hash($converted_h);
    unless (defined($aa)) {
      $logger->error(sprintf(
        'invalid hash object %s for %s->from_wire_hash',
        Data::Printer::np($h), __PACKAGE__
      ));
      return;
    }
    $aa->id($h->{id});
    return $aa;
  }

  sub from_hash($class, $object) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    unless (exists $object->{ascending}
      && ref($object->{ascending}) eq 'ARRAY') {
      $logger->error(sprintf(
        'object has unexpected format for an '
          . 'Ascending Attribute. Object is "%s"',
        Data::Printer::np($object, multiline => 0)
      ));
      return;
    }
    my $an = $object->{general};
    unless (defined($an) && length($an)) {
      $logger->error('object must have a "general" attribute');
      return;
    }
    $logger->debug(
      sprintf('starting import for ascending attribute "%s"', $an));
    my $aa = Game::EvonyTKR::Model::AscendingAttributes->new(general => $an);
    foreach my $oa (@{ $object->{ascending} }) {
      my $level = $oa->{level};
      unless (defined($level) && length($level)) {
        $logger->error('undefined or zero length level in ascending object %s',
          Data::Printer::np($oa, multiline => 0));
        next;
      }
      foreach my $ob (@{ $oa->{buffs} }) {
        my $b = Game::EvonyTKR::Model::Buff->from_hash($ob);
        $aa->addBuff($level, $b);
        $logger->debug(sprintf(
          '%s now has %s buffs at level %s',
          $an, scalar($aa->attributes->{$level}->{buffs}->@*), $level,
        ));
      }
    }
    return $aa;
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

# ABSTRACT: Module for processing information about Evony TKR Specialties.

=head1 DESCRIPTION

Ascending is one of several ways that a General can provide Buffs for Troops.

Ascending works similarly to Specialties in that there are multiple levels, however there is only one set of Ascending Buffs per general, instead of the four possible Specialties (plus possible Flex Specialty).

=for :List

* Ascending Buffs can be either values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

* Ascending Buffs 1Red through 5Red provide ehancements to Basic skills and affect the General's effective scores for the basic attributes.

* Ascending a Historic (Gold/Red) General costs both general fragments and Blood of Ares.  Ascending a General that is merely Historic but not Legendary (Purple) requires general fragments, but not Blood of Ares.

* For low spenders, Blood of Ares is a severe limiting factor on ascending generals.  As you grow in spending, it limits the I<rate> at which you ascend, but not I<if> you ascend a general.  Fragments are the true limiting concern for players of all levels.  It is not possible to get sufficient fragments of all generals to fully ascend them without spending in any reasonable amount of time.  It is not possible to ascend certain "retired" generals I<at all> if you have not already done so.  These "retired" Generals are included in the distribution data to allow players who I<do> have them to compare effectiveness in using them.
=cut

=method name()

returns the name field from the Specialty.
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Specialty at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method Buffs()

Returns a hash with the levels 1Purple through 5Purple, and 1Red through 5Red as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

Each level is cumulative, you never need to read more than the array for the currently active level.
=cut

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR Specialties.

=head1 DESCRIPTION

Ascending is one of several ways that a General can provide Buffs for Troops.

Ascending works similarly to Specialties in that there are multiple levels, however there is only one set of Ascending Buffs per general, instead of the four possible Specialties (plus possible Flex Specialty).

=for :List

* Ascending Buffs can be either values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

* Ascending Buffs 1Red through 5Red provide ehancements to Basic skills and affect the General's effective scores for the basic attributes.

* Ascending a Historic (Gold/Red) General costs both general fragments and Blood of Ares.  Ascending a General that is merely Historic but not Legendary (Purple) requires general fragments, but not Blood of Ares.

* For low spenders, Blood of Ares is a severe limiting factor on ascending generals.  As you grow in spending, it limits the I<rate> at which you ascend, but not I<if> you ascend a general.  Fragments are the true limiting concern for players of all levels.  It is not possible to get sufficient fragments of all generals to fully ascend them without spending in any reasonable amount of time.  It is not possible to ascend certain "retired" generals I<at all> if you have not already done so.  These "retired" Generals are included in the distribution data to allow players who I<do> have them to compare effectiveness in using them.
=cut

=method name()

returns the name field from the Specialty.
=cut

=method activeLevel

returns the level, values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals), that is active at this time.
=cut

=method setActiveLevel($newLevel)

sets the activeLevel to newLevel presuming it is a valid value 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

Todo: Make sure the value is valid I<for the General's quality>.
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the buffs this Specialty at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>.
=cut

=method Buffs()

Returns a hash with the levels 1Purple through 5Purple, and 1Red through 5Red as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists.

Each level is cumulative, you never need to read more than the array for the currently active level.
=cut

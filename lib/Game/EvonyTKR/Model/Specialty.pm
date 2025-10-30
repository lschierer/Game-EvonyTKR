use v5.42.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require JSON::PP;
require Data::Printer;

require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::Buff::Matcher;
use namespace::autoclean;

package Game::EvonyTKR::Model::Specialty {
  use Mojo::Base -base,                               -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',      -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Specialties', -role;
  use List::AllUtils qw( any none all );
  use UUID qw(uuid5);
  use Hash::Util qw(lock_keys);
  use Carp;
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  has ['id', 'name'] => '';

  has 'levels' => sub { _init_empty_levels() };

  sub _init_empty_levels () {
    my %h = map { $_ => { text => '', buffs => [] } } $self->SpecialtyLevelValues->@*;
    lock_keys(%h);
    return \%h;
  }

  sub is_valid_level ($class, $level) { exists $class->SpecialtyLevels{$level} }

  # --- YAML -> object (input shape: levels = [ {level, text, buffs}, ... ]) ---
  sub from_hash ($class, $h) {
    croak "from_hash expects hashref" unless ref($h) eq 'HASH';
    my $name = $h->{name} // '';
    my $levels = _empty_levels();

    my $arr = $h->{levels};
    croak "levels must be an arrayref" unless ref($arr) eq 'ARRAY';

    foreach my $lv ($h->{levels}->@*){
      croak "level must have a level attribute" unless (defined($lv->{level}) && length($lv->{level}));
      croak sprintf('invalid level %s, level must be one of %s', $lv->{level}, join ', ', $class->SpecialtyLevelValues->@*) unless (exists $class->SpecialtyLevels{$lv->{level}});
      foreach my $b ($lv->{buffs}->@*){
        push @{ $levels->{ $lv->{level} }{buffs} }, Game::EvonyTKR::Model::Buff->from_hash($b);
      }
      $levels->{$lv->{level}}{text}   = $lv->{text} // '';
    }
    croak sprintf('extra unexpected levels present.  [%s] must equal [%s]',
    join ', ' map { $_->{level} } $h->{levels}->@*, join ', '  $class->SpecialtyLevelValues->@* ) unless(scalar($h->{levels}->@*) == scalar(keys $levels->%*));

    return $class->new(name => $name, levels => $levels);
  }

  # --- object -> YAML (output shape compatible with your current files) ---
  sub to_hash ($self) {
    my $hash = {
      name    => $self->name,
      id      => $self->id,
      levels  => [],
    };
    foreach my $lv ($self->levels->@*){
      my $lh = {
        text  => $lv->{text} // '',
        buffs => [],
      };
      foreach my $b ($lv->{buffs}->@*) {
        push @{ $lh->{buffs} }, $b->to_hash();
      }
      push @{ $hash->{levels} }, $lh;
    }
    return $hash;
  }

  # --- ergonomic helpers ---

  sub get_level ($self, $level) {
    Carp::croak "unknown level '$level'" unless $LEVEL{$level};
    return $self->levels->{$level};  # { text => ..., buffs => [...] }
  }

  sub set_text ($self, $level, $text) {
    $self->get_level($level)->{text} = $text // '';
    return $self;
  }

  sub add_buff ($self, $level, $buff) {
    push @{ $self->get_level($level)->{buffs} }, $buff;
    return $self;
  }


}
1;
__END__
{

   ADJUST {


  ADJUST {
    if (defined($self) && defined($self->UUID5_base)) {
      my $specialtybase = uuid5($self->UUID5_base, 'Specialty');
      if (defined($name)) {
        $id = uuid5($specialtybase, $name);
      }
    }
  }

  method get_buffs_at_level (
    $level, $attribute, $matching_type,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    $level = lc($level)
      ;    # sanitize the data from the user - level names must be lower case
    $self->logger->debug(
      "Calculating buffs for $name level: $level, attribute: $attribute");

    return 0 if not defined $level or $level =~ /none/i;

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

    # an list's values function will always return in the same order.
    my $valid_levels = @{ $self->SpecialtyLevelValues };
    # Define the hierarchy of levels
    my @level_hierarchy = $self->SpecialtyLevelValues->@*;

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

      $self->logger->debug("Checking $name level $current_level with "
          . scalar(@{$buffs})
          . " buffs");

      foreach my $buff (@$buffs) {
        my $matcher =
          Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $buff);
        my $logID = int(rand(9e12)) + 1e12;
        if ($matcher->match(
          $attribute,             $targetedType,
          $match_buff_conditions, $match_debuff_conditions,
          $logID
        )) {
          my $val = $buff->value->number;
          $self->logger->debug(sprintf(
            '%s  ➤ Match found at %s level %s. Adding %s to total.',
            $logID, $name, $current_level, $val
          ));
          $total += $val;
        }
        else {
          $self->logger->debug("$logID  ✗ No match found.");
        }
      }
    }

    $self->logger->debug(
      "Total for $name $level/$attribute/$targetedType/$matching_type: $total");
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
    if (none { $_ =~ /$level/i } $self->SpecialtyLevelValues->@*) {
      $self->logger->error(sprintf(
        'level should be one of %s, not %s',
        join(', ', $self->SpecialtyLevelValues->@*), $level
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

  method concat($other, $swap) {
    if ($swap) {
      return $other . $self->as_string();
    }
    else {
      return $self->as_string() . $other;
    }
  }

  sub from_hash ($class, $object,) {
    my $logger = $log;
    unless (exists($object->{name}) && length($object->{name})) {
      $logger->error('Name is required to create a Specialty.');
      return undef;
    }

    my $s = Game::EvonyTKR::Model::Specialty->new(name => $object->{name});
    foreach my $ol (@{ $object->{levels} }) {
      my $level = $ol->{level};
      $logger->debug(
        sprintf('attempting import of %s for %s', $level, $object->{name}));
      my @buffs;
      if (exists $ol->{buff}) {
        @buffs = @{ $ol->{buff} };
      }
      elsif (exists $ol->{buffs}) {
        @buffs = @{ $ol->{buffs} };
      }
      foreach my $ob (@buffs) {
        my $b = Game::EvonyTKR::Model::Buff->from_hash($ob);
        $s->addBuff($level, $b);
      }
      $logger->debug(sprintf(
        'added %s buffs to level %s for specialty %s ',
        scalar @{ $s->levels->{ lc($level) }->{buffs} }, $level,
        $object->{name}
      ));
    }
    return $s;
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

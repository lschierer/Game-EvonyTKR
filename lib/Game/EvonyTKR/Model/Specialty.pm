use v5.42.0;
use utf8::all;

use File::FindLib 'lib';
require JSON::PP;
require Data::Printer;

require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::Buff::Matcher;
use namespace::autoclean;

package Game::EvonyTKR::Model::Specialty {
  use Mojo::Base -base,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Specialties',      -role;
  use List::AllUtils qw( any none all );
  use UUID           qw(uuid5);
  use Hash::Util     qw(lock_keys);
  use Log::Any       qw($log);
  use Carp;
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  has ['name'] => '';

  has 'id' => sub ($self) {
    if (defined($self) && defined($self->UUID5_base)) {
      my $specialtybase = uuid5($self->UUID5_base, 'Specialty');
      if (defined($self->name)) {
        return uuid5($specialtybase, $self->name);
      }
    }
    return '';
  };

  has 'levels' => sub ($self) {
    return $self->_init_empty_levels();
  };

  sub _init_empty_levels ($self) {
    my %h =
      map { $_ => { text => '', buffs => [] } } $self->SpecialtyLevelValues->@*;
    lock_keys(%h);
    return \%h;
  }

  # --- YAML -> object (input shape: levels = [ {level, text, buffs}, ... ]) ---
  sub from_hash ($class, $h) {
    my $logger = Log::Any->get_logger(category => __PACKAGE__);
    croak "from_hash expects hashref" unless ref($h) eq 'HASH';
    my $name = $h->{name} // '';
    $logger->debug(sprintf(
      'starting import of %s, class is %s',
      $name, ref($class) // 'scalar'
    ));
    my $arr = $h->{levels};
    croak "levels must be an arrayref" unless ref($arr) eq 'ARRAY';
    my $s = $class->new(name => $name);

    foreach my $lv ($h->{levels}->@*) {
      croak "level must have a level attribute"
        unless (defined($lv->{level}) && length($lv->{level}));

      unless ($s->is_valid_level(lc($lv->{level}))) {
        my $errmessg = sprintf('invalid level %s, level must be one of %s',
          $lv->{level}, join ', ', $s->SpecialtyLevelValues->@*);
        $s->logger->logcroak($errmessg);
      }

      foreach my $b ($lv->{buffs}->@*) {
        push @{ $s->levels->{ lc($lv->{level}) }{buffs} },
          Game::EvonyTKR::Model::Buff->from_hash($b);
      }

      $s->levels->{ lc($lv->{level}) }->{text} = $lv->{text} // '';
    }
    unless (scalar($h->{levels}->@*) <= scalar(keys $s->levels->%*)) {
      croak sprintf(
        'extra unexpected levels present.  [%s] must equal [%s]',
        join(',',  map { $_->{level} } $h->{levels}->@*),
        join(', ', $s->SpecialtyLevelValues->@*)
      );
    }
    $s->logger->debug(sprintf(
      's is %s, stringifies as %s',
      Data::Printer::np($s, class => { stringify => 0 }),
      Data::Printer::np($s)
    ));
    return $s;
  }

  # --- object -> YAML (output shape compatible with your current files) ---
  sub to_hash ($self) {
    my $hash = {
      name   => $self->name,
      id     => $self->id,
      levels => $self->_init_empty_levels(),
    };
    foreach my $lv (keys $self->levels->%*) {
      next if $lv eq 'none';
      my $l = $self->levels->{$lv};
      $hash->{levels}->{$lv}->{text} = $l->{text} // '';
      foreach my $b ($l->{buffs}->@*) {
        push @{ $hash->{levels}->{$lv}->{buffs} }, $b->to_hash();
      }
    }
    return $hash;
  }

  sub to_wire_hash ($self) {
    my $hash = {
      _v     => 1,
      id     => $self->id,
      name   => $self->name,
      levels => $self->levels,
    };
    return $hash;
  }

  sub from_wire_hash ($class, $w) {
    die "unknown wire version" unless ($w->{_v} // 1) == 1;

    my $specialty = $class->new(
      id   => $w->{id} // '',
      name => $w->{name}
    );

    # Set levels if they exist
    if ($w->{levels}) {
      $specialty->{levels} =
        $w->{levels};    # Direct assignment since levels is complex
    }

    return $specialty;
  }

  sub TO_JSON ($self) {
    return $self->to_wire_hash();
  }

  sub addBuff ($self, $level, $nb) {

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
    push @{ $self->levels->{$level}->{buffs} }, $nb;
    $self->logger->debug(sprintf(
      'specialty %s at %s now has buffs %s.',
      $self->name, $level,
      Data::Printer::np($self->levels->{$level}->{buffs})
    ));
    return scalar @{ $self->levels->{$level}->{buffs} };
  }

  sub get_buffs_at_level (
    $self, $level, $attribute, $matching_type,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    $level = lc($level)
      ;    # sanitize the data from the user - level names must be lower case
    $self->logger->debug(
      "Calculating buffs for $self->name level: $level, attribute: $attribute");

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

      $self->logger->debug("Checking $self->name level $current_level with "
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
            $logID, $self->name, $current_level, $val
          ));
          $total += $val;
        }
        else {
          $self->logger->debug("$logID  ✗ No match found.");
        }
      }
    }

    $self->logger->debug(
"Total for $self->name $level/$attribute/$targetedType/$matching_type: $total"
    );
    return $total;
  }

  # --- ergonomic helpers ---

  sub as_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub concat($self, $other, $swap) {
    return "$self" . "$other";
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

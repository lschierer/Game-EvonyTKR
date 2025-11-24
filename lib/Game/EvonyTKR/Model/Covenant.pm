use v5.42.0;
use utf8::all;

use File::FindLib 'lib';
require JSON::PP;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::Buff::Matcher;
require Data::Printer;

package Game::EvonyTKR::Model::Covenant {
  use Mojo::Base "Game::EvonyTKR::Model::Base";
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Covenants',        -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role,
    -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Covenants', -role,
    -signatures;
  use builtin qw(indexed);
  use File::FindLib 'lib';
  use List::AllUtils qw(first any all none uniq);
  use Hash::Util     qw(lock_keys);
  use Carp;
  use overload
    '""'       => \&as_string,
    'bool'     => \&isTrue,
    "fallback" => 1;

  my $debug = 1;

  has ['primary', 'one', 'two', 'three'];

  has 'secondaryKeys' => sub {qw(one two three)};

  has 'categories' => sub ($self) {
    my $h  = {};
    my $cv = $self->CovenantCategoryValues;
    $self->logger->debug(sprintf('cv is %s', Data::Printer::np($cv)));
    foreach my $index (0 .. scalar($self->CovenantCategoryValues->@*) - 1) {
      my $key = $self->CovenantCategoryValues->[$index];
      $self->logger->debug(sprintf('key at index %s is %s', $index, $key));
      if ($key eq 'none') {
        next;
      }
      my $al = 10000 + $index * 2 * 1000;
      $self->logger->debug("setting activationLevel for $key to $al");
      $h->{$key} = {
        activationLevel => $al,
        buffs           => [],
      };
    }
    lock_keys(%{$h});

    return $h;
  };

  sub get_buffs_at_level (
    $self, $level, $attribute, $matching_type,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
    $includePassive   = 0,
  ) {
    $self->logger->debug(sprintf(
      'Calculating ascending buffs for level: %s, attribute: %s',
      $level, $attribute
    ));
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
    return 0 if not defined $level or $level =~ /None/i;

    my $valid_levels = $self->CovenantCategoryValues;
    my %level_index  = map { $valid_levels->[$_] => $_ } 0 .. $#$valid_levels;

    unless (exists $level_index{$level}) {
      $self->logger->debug("Invalid level: $level");
      return 0;
    }

    my $total = 0;

    my $target_index = $level_index{$level};

    for my $i (1 .. $target_index) {    # skip index 0 for 'None'
      my $level_name = $valid_levels->[$i];
      my $buffs      = $self->categories->{$level_name}->{buffs};
      $self->logger->debug(sprintf(
        'Checking level "%s" with %s buffs.',
        $level_name, scalar(@$buffs)
      ));
      foreach my $buff (@$buffs) {
        if ($buff->passive & !$includePassive) {
          next;
        }
        my $matcher =
          Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $buff);
        my $logID = int(rand(9e12)) + 1e12;
        if ($matcher->match(
          $attribute,             $targetedType,
          $match_buff_conditions, $match_debuff_conditions,
          $logID
        )) {
          my $val = $buff->value->number;
          $self->logger->debug("  ➤ Match found. Adding $val to total.");
          $total += $val;
        }
        else {
          $self->logger->debug("  ✗ No match found.");
        }
      }
      $self->logger->debug(sprintf(
        '%s has Total %s for level "%s" and attribute "%s"',
        $self->primary->name, $level, $attribute
      ));
      return $total;
    }
  }

  sub addBuff ($self, $level, $nb) {
    my $red = 1;
    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      $self->logger->error(sprintf(
        'attempting to add buff of type %s not "Game::EvonyTKR::Model::Buff"',
        !blessed($nb) ? Scalar::Util::reftype($nb) : blessed($nb)));
      exit 0;
    }

    if (none { $level =~ /$_/i } $self->CovenantCategoryValues->@*) {
      $self->logger->error(sprintf(
        'level should be one of %s, not %s',
        join(', ', @{ $self->CovenantCategoryValues }), $level
      ));
      return 0;
    }
    if ($level eq 'None') {
      return 0;
    }
    my $count = -1;
    $level = lc($level);
    if (!exists $self->categories->{$level}) {
      $self->logger->error(
        "category $level is not a valid key for covenantlevels!!");
    }
    else {
      push @{ $self->categories->{$level}->{buffs} }, $nb;
      $count = scalar @{ $self->categories->{$level}->{buffs} };
      $self->logger->debug("Added buff for attribute '"
          . $nb->attribute
          . "' to covenant level '$level', now has $count buffs");
    }

    return $count;
  }

  sub from_hash($class, $object) {
    my $logger = Game::EvonyTKR::Log::Config->get_logger(__PACKAGE__);
    if (!exists $object->{name}) {
      $logger->error('object must have name attribute.');
      return;
    }
    $logger->info(
      sprintf('attempting import of covenant for "%s"', $object->{name}));
    my $name = $object->{name};
    state $general_helper //= do {
      my $helper = eval {
        Game::EvonyTKR::Model::Base->new->with_roles(
          'Game::EvonyTKR::Controller::Role::Generals');
      };
      if ($@) {
        $logger->error("Cannot create general helper: $@");
        return;
      }
      $helper;
    };

    return unless $general_helper;

    my $primary = $general_helper->get_general($name);
    unless (defined($primary)
      && ref($primary)
      && $primary->isa('Game::EvonyTKR::Model::General')) {
      $logger->error(
        sprintf('Could not find general for covenant with name %s',
          $object->{name})
      );
      return;
    }
    $logger->debug("found primary general for $name, starting import.");
    $logger->debug(
      "generals for $name are " . join(", ", @{ $object->{generals} }));

    my $o = Game::EvonyTKR::Model::Covenant->new(
      primary => $primary,
      one     => $object->{generals}->[0],
      two     => $object->{generals}->[1],
      three   => $object->{generals}->[2],
    );

    foreach my $oc (@{ $object->{levels} }) {
      my $category = $oc->{category};
      unless (defined($category) && length($category)) {
        $logger->error(
          'invalid category!! ' . Data::Printer::np($object, multiline => 0));
        return;
      }

      # Find buffs for this category
      my @buffs;
      if (exists $oc->{buff}) {
        @buffs = @{ $oc->{buff} };
      }
      elsif (exists $oc->{buffs}) {
        @buffs = @{ $oc->{buffs} };
      }
      $logger->debug(sprintf(
        'found %s buffs for convenant %s at level %s',
        scalar(@buffs), $name, $oc->{category}
      ));

      foreach my $ob (@buffs) {
        my $b = Game::EvonyTKR::Model::Buff->from_hash($ob);
        $o->addBuff($category, $b);
      }
    }
    return $o;
  }

  sub from_wire_hash ($class, $h) {
    my $logger = Game::EvonyTKR::Log::Config->get_logger(__PACKAGE__);
    unless (ref($h) && ref($h) eq 'HASH') {
      my $errmessage = 'from_wire_hash requires a valid hashref';
      $logger->error($errmessage);
      croak($errmessage);
      return;
    }
    unless ($h->{primary} && length($h->{primary})) {
      my $errmessage =
        'hash provided to from_wire_hash must have an attribute "primary"';
      $logger->error($errmessage);
      croak($errmessage);
      return;
    }

    state $general_helper //= do {
      my $helper = eval {
        Game::EvonyTKR::Model::Base->new->with_roles(
          'Game::EvonyTKR::Controller::Role::Generals');
      };
      if ($@) {
        $logger->error("Cannot create general helper: $@");
        return;
      }
      $helper;
    };

    return unless $general_helper;

    my $primary = $general_helper->get_general($h->{primary});
    unless (defined($primary)
      && ref($primary)
      && $primary->isa('Game::EvonyTKR::Model::General')) {
      $logger->error(
        sprintf('Could not find general for covenant with name %s',
          $h->{primary})
      );
      return;
    }
    $logger->debug(sprintf('found primary general for "%s", starting import.',
      $h->{primary}));
    $logger->debug(sprintf(
      'generals for "%s" are %s',
      $h->{primary}, join(", ", @{ $h->{generals} })
    ));

    my $o = Game::EvonyTKR::Model::Covenant->new(
      primary => $primary,
      one     => $h->{generals}->[0],
      two     => $h->{generals}->[1],
      three   => $h->{generals}->[2],
    );

    foreach my $category (keys $h->{categories}->%*) {
      unless (defined($category) && length($category)) {
        $logger->error(
          'invalid category!! ' . Data::Printer::np($h, multiline => 0));
        return;
      }

      my $oc = $h->{categories}->{$category};
      $o->{categories}->{$category}->{text} =
        $h->{categories}->{$category}->{text} // '';

      # Find buffs for this category
      my @buffs;
      if (exists $oc->{buff}) {
        @buffs = @{ $oc->{buff} };
      }
      elsif (exists $oc->{buffs}) {
        @buffs = @{ $oc->{buffs} };
      }
      $logger->debug(sprintf(
        'found %s buffs for convenant %s at level %s',
        scalar(@buffs), $h->{primary}, $category
      ));

      foreach my $ob (@buffs) {
        my $b = Game::EvonyTKR::Model::Buff->from_hash($ob);
        $o->addBuff($category, $b);
      }
    }
    return $o;
  }

  sub to_wire_hash ($self) {
    my $h = {
      primary  => $self->primary->name,
      generals => [
        one   => $self->one,
        two   => $self->two,
        three => $self->three,
      ],
    };
    foreach my $key (keys $self->categories->%*) {
      $h->{categories}->{$key}->{activationLevel} =
        $self->categories->{$key}->{activationLevel} // 0;
      foreach my $b ($self->categories->{$key}->{buffs}->@*) {
        push @{ $h->{categories}->{$key}->{buffs} }, $b->to_wire_hash();
      }
    }
    return $h;
  }

  sub to_hash ($self) {
    my $h = {
      primary  => $self->primary->name,
      generals => {
        #one   => $secondary->{'one'}->name,
        #two   => $secondary->{'two'}->name,
        #three => $secondary->{'three'}->name,
        one   => $self->one,
        two   => $self->two,
        three => $self->three,
      },

    };
    foreach my $key (keys $self->categories->%*) {
      $h->{categories}->{$key}->{activationLevel} =
        $self->categories->{$key}->{activationLevel} // 0;
      foreach my $b ($self->categories->{$key}->{buffs}->@*) {
        push @{ $h->{categories}->{$key}->{buffs} }, $b->to_wire_hash();
      }
    }
    return $h;
  }

  sub TO_JSON ($self) {
    return $self->to_hash();
  }

  sub as_string ($self) {
    my $json =
      JSON::PP->new->utf8->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub isTrue ($self, $other = undef, $swap = undef) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa(__PACKAGE__);
  }

};
1;

__END__

# ABSTRACT: Module for processing information about Evony TKR General Covenants

=head1 DESCRIPTION

Covenants are one of several ways that a General can provide Buffs for Troops.

Covenants differ from other ways in that a Covenant, while associated with a particular General, requires the possession and development of at least one other, and possibly as many as three other, Generals.  It also differs in that some of the effects of the Covenants are what I refer to as "Passive" Buffs, that is the buff is active all the time, not just when that particular General is in use.

To get the Buff for a given Covenant Level, you must

=for :List

* Have the primary General

* Activate the Covenant to the desired level

* The Primary and Secondary Generals' Basic attributes added together must equal or exceed the minimum value for that level.

Thus Cultivating and Ascending Generals from the list for each Covenant will help obtain the Buffs provided by that Covenant, but Specialties, Armor, and Beasts/Dragons will not. because while these increase the General's power, they do only Cultivating and Ascending increase the General's Basic Attributes.

This makes figuring out when to include these buffs I<much> more complicated than any other Buff so far considered.  Typically I look solely at Pairs of Generals.  For a Covenant, I need to consider your I<entire inventory> of Generals,
as there are passive buffs always active, and the supporting Generals may not be part of the Pair, but their basic attributes are still critical to determine the activation conditions.

Todo:  actually get the activation conditions for Covenants right.  For now I am depending on the user to set activated or not which while required is not sufficient.
=cut

=method primary()

Returns the Primary Game::EvonyTKR::Model::General with whom this Covenant is associated.

=cut

=method secondary()

returns a hash containing the secondary or supporting Game::EvonyTKR::Model::General objects for this covenant.
=cut

=method secondaryKeys()

returns an array of the keys that are allowed values for the hash in secondary()
or in the setSecondary() method.
=cut

=method setSecondary($position, $general)

sets the Game::EvonyTKR::Model::General $general as a supporting general in $position in the %secondary hash.
the $position field must use one of the values returned by secondaryKeys().
=cut

=method Buffs()

returns a hash of the Buffs for this Covenant
The primary keys are the values from $self->covenantLevels from the ::Data class, each of which returns
a HashRef.
=cut

=method addBuff($level, $nb, $inherited = 0)

adds the Game::EvonyTKR::Model::Buff to the specified $level so long as $level is a valid selection from $self->covenantLevels.

$inherited should not be used by external callers, it is used internally to set up the Buff structure such that each Level contains all Buffs from the previous levels, but marked as such.

Todo:  This approach is flawed, see the note about properly accounting for the complicated activation.
=cut

method toHashRef()

returns a HashRef that can be passed a JSON serializer successfully.
=cut

method ""

calls a JSON serializer on the HashRef from toHashRef()
=cut

method readFromFile()

reads the convenant data into memory from the YAML representation in the distro's shared data directory.
=cut

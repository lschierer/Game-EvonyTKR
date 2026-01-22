use v5.42.0;
use experimental qw(class);
use utf8::all;

require Scalar::Util;

require Game::EvonyTKR::Model::General;

package Game::EvonyTKR::Model::General::Pair {
  use Moo;
  extends 'Game::EvonyTKR::Model::Base';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';
  with 'Game::EvonyTKR::Role::Constants::AscendingAttributes';
  use UUID           qw(uuid5);
  use List::AllUtils qw( any none );
  use File::FindLib 'lib';
  use Carp;
  use overload
    '""'       => \&to_string,
    '<=>'      => \&compare,
    'cmp'      => \&compare,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  has [qw(primary secondary type)] => (is => 'rw');

# Precomputed generic book buff values by activation type and level
# Structure: { Attacking => { level3 => {march_size => 12, ...}, level6 => {...}, ... }, ... }
# For pairs, level3 = single general's 3 books, level6 = both generals' 6 books combined
  has 'genericBookBuffs' => (is => 'rw', default => sub { {} });

  sub persistenceHelper ($self) {
    state $persistence_helper //= do {
      my $helper = eval { Game::EvonyTKR::Model::Base->new(); };
      if ($@) {
        $self->logger->error(
          sprintf('Cannot create Persistence Helper: %s', $@));
        return;
      }
      $helper;
    };
    return $persistence_helper;
  }

  sub to_hash ($self) {
    my $h = {
      primary   => $self->primary->to_hash(),
      secondary => $self->secondary->to_hash(),
      type      => $self->type,
    };
    return $h;
  }

  sub to_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub to_wire_hash ($self) {
    my $h = {
      primary   => { name => $self->primary->name },
      secondary => { name => $self->secondary->name },
      type      => $self->type,
    };
    return $h;
  }

  sub from_wire_hash ($class, $h, %opts) {
    my $logger = Log::Handler->get_logger(__PACKAGE__);

    my $primary_name =
      ref($h->{primary}) eq 'HASH'
      ? $h->{primary}->{name}
      : $h->{primary};
    my $secondary_name =
      ref($h->{secondary}) eq 'HASH'
      ? $h->{secondary}->{name}
      : $h->{secondary};

    unless (defined($primary_name) && length($primary_name)) {
      $logger->error('hash object must contain a primary with a name in it.');
      return;
    }

    state $general_helper;
    unless ($general_helper) {
      eval { $general_helper = Game::EvonyTKR::Model::Base->new(); } or do {
        $logger->error(
          sprintf('eval failed; cannot define general helper: "%s"', $@));
        return;
      };
    }

    # Pass options down to get_general (e.g., populateGenericBooks => 0)
    my $primary = $general_helper->get_general($primary_name, \%opts);
    unless ($primary) {
      $logger->error(
        sprintf('cannot retrieve general for %s when creating a pair.',
          $primary_name)
      );
      return;
    }
    my $secondary = $general_helper->get_general($secondary_name, \%opts);
    unless ($secondary) {
      $logger->error(
        sprintf('cannot retrieve general for %s when creating a pair.',
          $secondary_name)
      );
      return;
    }

    unless ($h
      && ref($h)
      && ref($h) eq 'HASH'
      && exists $h->{type}
      && length($h->{type})) {
      $logger->error('hash object must contain a type.');
      return;
    }

    my $pair = $class->new(
      primary   => $primary,
      secondary => $secondary,
      type      => $h->{type},
    );

    # Populate generic books for the pair
    $pair->populateGenericBooks();

    return $pair;
  }

  sub populateGenericBooks ($self) {
    $self->logger->debug(sprintf(
      'populateGenericBooks called for pair: %s + %s',
      $self->primary->name, $self->secondary->name
    ));

    unless ($self->persistenceHelper) {
      $self->logger->error('No persistenceHelper available');
      return 0;
    }

# Ensure both generals have builtin books populated (needed for conflict detection)
    unless ($self->primary->builtInBook) {
      $self->logger->debug('Primary builtInBook not populated');
      $self->primary->populateBuiltinBook();
      unless ($self->primary->builtInBook) {
        $self->logger->error(sprintf(
          'Failed to populate builtInBook for primary %s',
          $self->primary->name));
        return 0;
      }
    }

    unless ($self->secondary->builtInBook) {
      $self->logger->debug('Secondary builtInBook not populated');
      $self->secondary->populateBuiltinBook();
      unless ($self->secondary->builtInBook) {
        $self->logger->error(sprintf(
          'Failed to populate builtInBook for secondary %s',
          $self->secondary->name));
        return 0;
      }
    }

# Get troop type from the primary general (not the pair's type field)
# The pair's type is "mounted" but we need "mounted_specialist" for book lookups
    my $troop_type =
      ref($self->primary->type) eq 'ARRAY'
      ? $self->primary->type->[0]
      : $self->primary->type;

    unless ($troop_type) {
      $self->logger->warn(sprintf(
        'No troop type defined for pair %s + %s',
        $self->primary->name, $self->secondary->name
      ));
      return 0;
    }

    $self->logger->debug(sprintf(
      'Populating generic books for pair %s + %s (%s)',
      $self->primary->name, $self->secondary->name, $troop_type
    ));

    my @activations = qw(Attacking PvM Mayor Defending);

    # Initialize conflict detector
    my $comparator = eval {
      require Game::EvonyTKR::Service::Conflicts::BookComparator;
      Game::EvonyTKR::Service::Conflicts::BookComparator->new(service => $self);
    };
    if ($@) {
      $self->logger->error(sprintf('Failed to load BookComparator: %s', $@));
      return 0;
    }

    foreach my $activation (@activations) {
      $self->logger->debug(sprintf('Processing activation: %s', $activation));

      # For pairs, compute level3 (single general) and level6 (both generals)
      foreach my $count (3, 6) {
        my $level = "level$count";

        $self->logger->debug(sprintf(
          'Computing %s for %s: %d books', $level, $activation, $count
        ));

        # Replicate load_best_skill_books logic with conflict detection
        my $key = $activation eq 'PvM' ? 'PvM' : 'default';
        $key = 'default' if ($troop_type eq 'wall');

        my @sorted_book_names;
        if ( exists $self->BestSkillBooks->{$troop_type}
          && exists $self->BestSkillBooks->{$troop_type}->{$key}) {
          @sorted_book_names = sort {
            $self->BestSkillBooks->{$troop_type}->{$key}->{$a}
              <=> $self->BestSkillBooks->{$troop_type}->{$key}->{$b}
          } keys %{ $self->BestSkillBooks->{$troop_type}->{$key} };
        }
        elsif (exists $self->BestSkillBooks->{$troop_type}) {
          @sorted_book_names = sort {
            $self->BestSkillBooks->{$troop_type}->{'default'}->{$a}
              <=> $self->BestSkillBooks->{$troop_type}->{'default'}->{$b}
          } keys %{ $self->BestSkillBooks->{$troop_type}->{'default'} };
        }
        else {
          $self->logger->error(sprintf(
            'targetType "%s" is not supported by BestSkillBooks',
            $troop_type));
          next;
        }

        my $best_level = $self->bestLevel;
        my @books;
        my $skipped_full_conflicts = 0;
        my $skipped_dual_partial   = 0;

        foreach my $book_name (@sorted_book_names) {
          my $base_name = $book_name =~ s/^Level \d+ //r;
          my $book = eval { $self->get_generic_book($base_name, $best_level) };

          unless ($book
            && ref($book)
            && $book->isa('Game::EvonyTKR::Model::Book')) {
            $self->logger->error("Cannot find $book_name");
            next;
          }

  # Check conflicts with both generals
  # Use same_side=0 to get "raw" conflict levels (including partial)
  # Use delta_threshold => 15 for general-to-book (vs 25 for general-to-general)
          my $primary_conflict = eval {
            $comparator->conflicts($self->primary, $book,
              { same_side => 0, delta_threshold => 15 });
          };
          my $secondary_conflict = eval {
            $comparator->conflicts($self->secondary, $book,
              { same_side => 0, delta_threshold => 15 });
          };

          if ( $@
            || !defined($primary_conflict)
            || !defined($secondary_conflict)) {
            $self->logger->error(sprintf(
              'Error checking conflicts for %s: %s', $book_name, $@));
            next;
          }

          # Full conflict with either general - skip
          if ($primary_conflict == 2 || $secondary_conflict == 2) {
            $self->logger->debug(sprintf(
              'Skipping %s due to full conflict (primary=%d, secondary=%d)',
              $book_name, $primary_conflict, $secondary_conflict
            ));
            $skipped_full_conflicts++;
            next;
          }

          # Both have partial conflicts - skip (acts as full for pairs)
          if ($primary_conflict == 1 && $secondary_conflict == 1) {
            $self->logger->debug(sprintf(
'Skipping %s due to dual partial conflict (both generals conflict)',
              $book_name));
            $skipped_dual_partial++;
            next;
          }

          $self->logger->debug(sprintf(
'Picked book "%s" for pair (primary_conflict=%d, secondary_conflict=%d)',
            $book_name, $primary_conflict, $secondary_conflict
          ));

          push @books, $book;
          last if (scalar @books >= $count);
        }

        $self->logger->debug(sprintf(
          'Selected %d books for %s/%s (skipped %d full, %d dual-partial)',
          scalar(@books), $activation,
          $level,         $skipped_full_conflicts,
          $skipped_dual_partial
        ));

        # Sum buff values from all books
        my %buffs = ();
        foreach my $book (@books) {
          foreach my $buff (@{ $book->buffs }) {
            # Use primary's _map_buff_to_column method
            my $column_key = $self->primary->_map_buff_to_column($buff);
            next unless $column_key;

            my $value = $buff->{value}{number} || 0;
            $buffs{$column_key} += $value;
          }
        }

        $self->genericBookBuffs->{$activation}{$level} = \%buffs;

        $self->logger->debug(sprintf(
          'Computed %s/%s: %d buff types (%s)',
          $activation,         $level,
          scalar(keys %buffs), join(', ', map {"$_=$buffs{$_}"} keys %buffs)
        ));
      }
    }

    $self->logger->debug(sprintf(
      'populateGenericBooks complete for pair. Activations: %s',
      join(', ', keys %{ $self->genericBookBuffs })));

    return 1;
  }

  sub compare ($self, $other, $swapped = undef) {
    my ($a, $b) = $swapped ? ($other, $self) : ($self, $other);
    if ($a
      && Scalar::Util::blessed($a) =~ /Game::EvonyTKR::Model::General::Pair/) {
      if ($b
        && Scalar::Util::blessed($b) =~ /Game::EvonyTKR::Model::General::Pair/)
      {
        return $a->primary->name cmp $b->primary->name
          || $a->secondary->name cmp $b->secondary->name;
      }
      else {
        return $a->primary->name cmp "$b"
          || $a->secondary->name cmp "$b";
      }
    }
    elsif ($b
      && Scalar::Util::blessed($b) =~ /Game::EvonyTKR::Model::General::Pair/) {
      return "$a" cmp $b->primary->name
        || "$a" cmp $b->secondary->name;
    }
    else {
      return "$a" cmp "$b";
    }
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

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require UUID;

require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;

package Game::EvonyTKR::Model::General {
  use Mojo::Base "Game::EvonyTKR::Model::Base";
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',       -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  with 'Game::EvonyTKR::Role::Constants::AscendingAttributes';
  use JSON::PP;
  use UUID           qw(uuid5);
  use List::AllUtils qw( any none all );
  use File::FindLib 'lib';
  use Carp;
  use overload
    '""'       => \&as_string,
    'eq'       => \&equality,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  our $VERSION = 'v0.40.0';

  has 'id' => sub ($self) {
    my $general_type =
      ref($self->type) eq 'ARRAY' ? $self->type->[0] : $self->type;
    if (exists $self->UUID5_Generals->{$general_type}) {
      return uuid5($self->UUID5_Generals->{$general_type}, $self->name);
    }
    return $self->name;
  };

  has ['name', 'type', 'ascendingAttributes', 'builtInBookName',
    'builtInBook'] => undef;

  has ['specialtyNames', 'specialties'] => sub { [] };
  has 'ascending'                       => 0;
  has 'stars'                           => 'none';
  has 'basicAttributes' =>
    sub { return Game::EvonyTKR::Model::BasicAttributes->new() };

# Precomputed generic book buff values by activation type and level
# Structure: { Attacking => { level1 => {march_size => 3, ...}, level2 => {...}, ... }, ... }
  has 'genericBookBuffs' => sub { {} };

  sub set_general_id($self) {
    if (ref $self->type) {
      my @ts;
      push @ts, $self->type->@*;
      my $ut = $ts[0];
      $self->log_debug("using type $ut");
      my $uuid5base = $self->UUID5_Generals->{$ut};
      $self->id = uuid5($uuid5base, $self->name);
    }
    else {
      my $uuid5base = $self->UUID5_Generals->{ $self->type };
      $self->id = uuid5($uuid5base, $self->normalize($self->name));
    }
  }

  sub validate($self) {
    my @errors;
    if (not defined $self->GeneralKeys) {
      $self->log_logcroak('GeneralKeys is not defined in a General');
    }
    if (not defined $self->type) {
      push @errors,
        sprintf('type must be one of %s', join(', ', @{ $self->GeneralKeys }));
    }
    elsif (not $self->ValidateGeneralType($self->type)) {
      push @errors, 'General Type Failed Validation';
    }

    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (none { $self->stars =~ /$_/ } @valv) {
      push @errors,
        sprintf(
        'stars must be one of %s, not "%s"',
        join(',', @valv),
        $self->stars
        );
    }

    if (@errors) {
      $self->log_logcroak(join ', ', @errors);
      return;
    }
    return 1;
  }

  sub persistenceHelper ($self) {
    state $persistence_helper //= do {
      my $helper = eval { Game::EvonyTKR::Model::Base->new(); };
      if ($@) {
        $self->log_error(sprintf('Cannot create Persistence Helper: %s', $@));
        return;
      }
      $helper;
    };
    return $persistence_helper;
  }

  sub populateAscendingAttributes ($self,) {
    return unless $self->ascending;

    return unless $self->persistenceHelper();

    # Don't convert spaces to underscores - persistence stores with spaces
    my $key = $self->normalize($self->name);

    my $aa = $self->persistenceHelper->get_ascending_attributes($key);
    if ($aa) {
      $self->ascendingAttributes($aa);
    }
    else {
      $self->log_warn(sprintf(
        'failed to find expected ascending attributes '
          . 'for %s. expected keys are %s',
        $self->name,
        join ', ',
        map { sprintf('"%s"', %_ // 'undef file') }
          sort $self->persistenceHelper->list_ascending_attributes
      ));
      return;
    }
    return 1;
  }

  sub populateBuiltinBook ($self) {

    return unless $self->persistenceHelper;

    my $book;

    eval {
      $book =
        $self->persistenceHelper->get_builtin_book($self->builtInBookName);
    } or do {
      $self->log_error(
        sprintf('eval failed; cannot get book from helper: %s', $@));
      my $ab = $self->persistenceHelper->list_builtin_books;
      $self->log_debug(sprintf(
        'available books: %s',
        scalar(@{$ab})
        ? join ', ',
          map { sprintf('"%s"', $_) } $ab->@*
        : 'no books available'
      ));
      return;
    };

    if (defined($book)) {
      $self->log_debug(sprintf(
        'fetch returned book "%s" with name "%s" for builtInBookName "%s"',
        blessed($book), $book->can('name') ? $book->name : 'no name method',
        $self->builtInBookName
      ));
    }
    else {
      $self->log_error(
        sprintf('failed to fetch book for builtin book "%s" from cache',
          $self->builtInBookName)
      );
    }
    $self->builtInBook($book);
    return 1;
  }

  sub populateSpecialties ($self,) {

    return unless $self->persistenceHelper;

    foreach my $sn_index (0 .. scalar($#{ $self->specialtyNames })) {
      my $sn = $self->specialtyNames->[$sn_index];
      if (!defined($sn) || !length($sn)) {
        $self->log_error(sprintf(
          'invalid undef specialty in general %s at index %s',
          $self->name, $sn_index
        ));
        next;
      }
      $self->log_debug(sprintf(
        'populating speciality at index %s, name %s',
        $sn_index, defined($sn) && length($sn) ? $sn : 'undefined'
      ));
      my $specialty = $self->persistenceHelper->get_specialty($sn);
      if ($specialty) {
        $self->specialties->[$sn_index] = $specialty;
      }
      else {
        $self->log_error(sprintf(
          'Missing specialty at index %s for general "%s": "%s" ',
          $sn_index, $self->name, $sn
        ));
      }
    }
    unless (
      scalar(@{ $self->specialtyNames }) eq scalar(@{ $self->specialtyNames }))
    {
      return 0;
    }
    unless (
      all {
             $_
          && ref($_)
          && blessed($_)
          && $_->isa('Game::EvonyTKR::Model::Specialty')
      } $self->specialties->@*
    ) {
      return 0;
    }
    return 1;
  }

  sub populateGenericBooks ($self) {
    $self->log_debug(
      sprintf('populateGenericBooks called for %s', $self->name));

    unless ($self->persistenceHelper) {
      $self->log_error('No persistenceHelper available');
      return 0;
    }
    $self->log_debug('persistenceHelper is available');

    # Ensure builtin book is populated (needed for conflict detection)
    unless ($self->builtInBook) {
      $self->log_debug(
        'BuiltInBook not populated, calling populateBuiltinBook()');
      $self->populateBuiltinBook();
      unless ($self->builtInBook) {
        $self->log_error(sprintf(
          'Failed to populate builtInBook "%s" for %s',
          $self->builtInBookName // 'undef',
          $self->name
        ));
        return 0;
      }
    }

    # Get troop type (type is stored as array)
    my $troop_type =
      ref($self->type) eq 'ARRAY' ? $self->type->[0] : $self->type;

    unless ($troop_type) {
      $self->log_warn(
        sprintf('No troop type defined for general %s', $self->name));
      return 0;
    }

    $self->log_debug(
      sprintf('Populating generic books for %s (%s)', $self->name, $troop_type)
    );

    my @activations = qw(Attacking PvM Mayor Defending);

    # Initialize conflict detector
    my $comparator = eval {
      require Game::EvonyTKR::Service::Conflicts::BookComparator;
      Game::EvonyTKR::Service::Conflicts::BookComparator->new(service => $self);
    };
    if ($@) {
      $self->log_error(sprintf('Failed to load BookComparator: %s', $@));
      return 0;
    }

    foreach my $activation (@activations) {
      $self->log_debug(sprintf('Processing activation: %s', $activation));

      foreach my $count (1 .. 6)
      {    # Compute up to 6 levels (singles use 3, pairs use 6)
        my $level = "level$count";

        $self->log_debug(sprintf(
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
          $self->log_error(sprintf(
            'targetType "%s" is not supported by BestSkillBooks',
            $troop_type));
          next;
        }

        my $best_level = $self->bestLevel;
        my @books;
        my $skipped_conflicts = 0;

        foreach my $book_name (@sorted_book_names) {
          my $base_name = $book_name =~ s/^Level \d+ //r;
          my $book = eval { $self->get_generic_book($base_name, $best_level) };

          unless ($book
            && ref($book)
            && $book->isa('Game::EvonyTKR::Model::Book')) {
            $self->log_error("Cannot find $book_name");
            next;
          }

  # Check for full conflicts (partial conflicts OK for singles via same_side)
  # Use delta_threshold => 15 for general-to-book (vs 25 for general-to-general)
          my $conflict_level = eval {
            $comparator->conflicts($self, $book,
              { same_side => 1, delta_threshold => 15 });
          };

          if ($@) {
            $self->log_error(sprintf(
              'Error checking conflict for %s with %s: %s',
              $self->name, $book_name, $@
            ));
            next;
          }

          if ($conflict_level == 2) {
            # Full conflict - skip this book
            $self->log_debug(sprintf(
              'Skipping %s due to full conflict with %s',
              $book_name, $self->name
            ));
            $skipped_conflicts++;
            next;
          }

          $self->log_debug(sprintf(
            'Picked book "%s" for "%s" (conflict_level=%d)',
            $book_name, $self->name, $conflict_level
          ));

          push @books, $book;
          last if (scalar @books >= $count);
        }

        $self->log_debug(sprintf(
          'Selected %d books for %s/%s (skipped %d conflicts)',
          scalar(@books), $activation, $level, $skipped_conflicts
        ));

        # Sum buff values from all books
        my %buffs = ();
        foreach my $book (@books) {
          foreach my $buff (@{ $book->buffs }) {
            # Map buff to column name
            my $column_key = $self->_map_buff_to_column($buff);
            next unless $column_key;

            my $value = $buff->{value}{number} || 0;
            $buffs{$column_key} += $value;
          }
        }

        $self->genericBookBuffs->{$activation}{$level} = \%buffs;

        $self->log_debug(sprintf(
          'Computed %s/%s: %d buff types (%s)',
          $activation,         $level,
          scalar(keys %buffs), join(', ', map {"$_=$buffs{$_}"} keys %buffs)
        ));
      }
    }

    $self->log_debug(sprintf('populateGenericBooks complete. Activations: %s',
      join(', ', keys %{ $self->genericBookBuffs })));

    return 1;
  }

  # Helper method to map a buff to a column name (same logic as PDL Compiler)
  sub _map_buff_to_column ($self, $buff) {
    my $attribute     = lc($buff->{attribute} || '');
    my $targeted_type = $buff->{targetedType} || '';
    my $conditions    = $buff->{conditions}   || [];

    # Normalize attribute names
    $attribute =~ s/\s+/_/g;

    # Check if it's a debuff
    my $is_debuff = grep { $_ eq 'Enemy' } @$conditions;

    # Simple attributes that don't need troop type
    return 'march_size'       if $attribute eq 'march_size';
    return 'death_to_wounded' if $attribute eq 'death_to_wounded';
    return 'marching_speed'   if $attribute eq 'marching_speed';

    # Combat attributes need troop type suffix
    if ($attribute =~ /^(attack|defense|hp)$/) {
      my $buff_type = $attribute;

      # Determine troop type suffix
      my $suffix = 'all';    # Default to 'all' if no specific type

      if ($targeted_type =~ /ground/i) {
        $suffix = 'ground';
      }
      elsif ($targeted_type =~ /mounted/i) {
        $suffix = 'mounted';
      }
      elsif ($targeted_type =~ /ranged/i) {
        $suffix = 'ranged';
      }
      elsif ($targeted_type =~ /siege/i) {
        $suffix = 'siege';
      }

      # Add enemy prefix for debuffs
      my $prefix = $is_debuff ? 'enemy_' : '';
      return "${prefix}${buff_type}_${suffix}";
    }

    # Unknown attribute
    return undef;
  }

  sub from_hash ($class, $hashObject) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);

    if (!exists $hashObject->{name}) {
      $logger->error('hash object must contain a name attribute.');
      return undef;
    }

    my $g = Game::EvonyTKR::Model::General->new(
      name            => $hashObject->{name},
      type            => $hashObject->{type},
      ascending       => $hashObject->{ascending},
      stars           => $hashObject->{stars},
      builtInBookName => $hashObject->{book},
      specialtyNames  => $hashObject->{specialties},
    );
    unless ($g->validate()) {
      $logger->error('Invalid Hash Object.');
      return;
    }

    foreach my $baKey (keys %{ $hashObject->{basic_attributes} }) {
      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        attribute_name => $baKey,
        base           => $hashObject->{basic_attributes}->{$baKey}->{base},
        increment => $hashObject->{basic_attributes}->{$baKey}->{increment},
      );
      $g->basicAttributes->setAttribute($baKey, $ba);
    }

    return $g;
  }

  sub to_hash ($self) {
    return {
      __CLASS__       => __PACKAGE__,
      id              => $self->id,
      name            => $self->name,
      type            => $self->type,
      basicAttributes => $self->basicAttributes,
      ascending       => $self->ascending,
      builtInBookName => $self->builtInBookName,
      specialtyNames  => $self->specialtyNames,
    };
  }

  sub to_wire_hash ($self) {
    my $hash = {
      _v                  => 1,
      id                  => $self->id,
      name                => $self->name,
      type                => $self->type,
      ascending           => $self->ascending,
      builtInBookName     => $self->builtInBookName,
      specialtyNames      => $self->specialtyNames,
      stars               => $self->stars,
      ascendingAttributes => $self->ascendingAttributes,
    };

    # Handle basicAttributes if it exists
    if ($self->basicAttributes) {
      $hash->{basicAttributes} = $self->basicAttributes->to_hash();
    }

    return $hash;
  }

  sub from_wire_hash ($class, $w, $opts = {}) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    unless (($w->{_v} // 1) == 1) {
      $logger->error('unknown wire version');
      die "unknown wire version";
    }

    my $general = $class->new(
      name            => $w->{name},
      type            => $w->{type},
      ascending       => $w->{ascending} // 0,
      builtInBookName => $w->{builtInBookName},
      specialtyNames  => $w->{specialtyNames} // [],
      stars           => $w->{stars}          // 'none',
    );
    $general->populateBuiltinBook()
      unless (exists $opts->{populateBuiltinBook}
      && $opts->{populateBuiltinBook} == 0);
    $general->populateAscendingAttributes()
      unless (exists $opts->{populateAscendingAttributes}
      && $opts->{populateAscendingAttributes} == 0);
    $general->populateSpecialties()
      unless (exists $opts->{populateSpecialties}
      && $opts->{populateSpecialties} == 0);
    $general->populateGenericBooks()
      unless (exists $opts->{populateGenericBooks}
      && $opts->{populateGenericBooks} == 0);

    # Handle basicAttributes if it exists
    if ($w->{basicAttributes}) {
      $general->basicAttributes(
        Game::EvonyTKR::Model::BasicAttributes->from_hash(
          $w->{basicAttributes}
        )
      );
    }

    return $general;
  }

  sub TO_JSON {
    my $self = shift;
    return $self->to_wire_hash();
  }

  sub as_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub equality($self, $other, $swap = 0) {
    my $one = $swap ? $other : $self;
    my $two = $swap ? $self  : $other;
    my $on  = '';
    my $tn  = '';
    if (ref($one) && $one->isa('Game::EvonyTKR::Model::General')) {
      $on = $one->name;
    }
    else {
      $on = "$one";
    }
    if (ref($two) && $two->isa('Game::EvonyTKR::Model::General')) {
      $tn = $two->name;
    }
    else {
      $tn = "$two";
    }
    return $on eq $tn;
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

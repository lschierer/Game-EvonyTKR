package Game::EvonyTKR::Model::Buff::Summarizer::Pair;
use v5.42.0;
use utf8::all;
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Service::Conflicts;
use Mojo::Base 'Game::EvonyTKR::Model::Buff::Summarizer', -signatures;
use List::AllUtils qw(first any all none uniq);
use Scalar::Util   qw(blessed);
use Carp;
use diagnostics;

has pair => undef;

# Secondary general has its own covenant and specialty levels
has 'secondaryCovenantLevel' => 'Civilization';
has 'secondarySpecialty1'    => 'gold';
has 'secondarySpecialty2'    => 'gold';
has 'secondarySpecialty3'    => 'gold';
has 'secondarySpecialty4'    => 'gold';

has pairBuffValues => sub {
  {
    'Ground Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };
};

has pairDebuffValues => sub {
  {
    'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };
};

sub updatePrimaryBuffs ($self, $precomputed = undef) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->log_error(sprintf(
      '%s requires a Game::EvonyTKR::Model::General::Pair',
      __PACKAGE__));
    return;
  }

  if ($precomputed && ref($precomputed) && ref($precomputed) eq 'HASH') {
    $self->buffValues($precomputed);
  }
  else {
    $self->general($self->pair->primary);
    $self->isPrimary(1);
    $self->SUPER::updateBuffs();
  }

  $self->log_debug(sprintf('After primary updateBuffs: %s',
    Data::Printer::np($self->buffValues, max_depth => 2)));

  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }
}

sub updateSecondaryBuffs ($self, $precomputed = undef) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->log_error(sprintf(
      '%s requires a Game::EvonyTKR::Model::General::Pair',
      __PACKAGE__));
    return;
  }
  # Store primary levels
  my $primaryCovenantLevel = $self->covenantLevel;
  my $primarySpecialty1    = $self->specialty1;
  my $primarySpecialty2    = $self->specialty2;
  my $primarySpecialty3    = $self->specialty3;
  my $primarySpecialty4    = $self->specialty4;

  # Switch to secondary levels
  $self->covenantLevel($self->secondaryCovenantLevel);
  $self->specialty1($self->secondarySpecialty1);
  $self->specialty2($self->secondarySpecialty2);
  $self->specialty3($self->secondarySpecialty3);
  $self->specialty4($self->secondarySpecialty4);

  # Calculate secondary buffs
  if ($precomputed && ref($precomputed) && ref($precomputed) eq 'HASH') {
    $self->buffValues($precomputed);
  }
  else {
    $self->general($self->pair->secondary);
    $self->isPrimary(0);
    $self->SUPER::updateBuffs();
  }

  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }

  # Restore primary levels
  $self->covenantLevel($primaryCovenantLevel);
  $self->specialty1($primarySpecialty1);
  $self->specialty2($primarySpecialty2);
  $self->specialty3($primarySpecialty3);
  $self->specialty4($primarySpecialty4);
}

sub updateBuffs ($self) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->log_error(sprintf(
      '%s requires a Game::EvonyTKR::Model::General::Pair',
      __PACKAGE__));
    return;
  }

  # Store primary levels
  my $primaryCovenantLevel = $self->covenantLevel;
  my $primarySpecialty1    = $self->specialty1;
  my $primarySpecialty2    = $self->specialty2;
  my $primarySpecialty3    = $self->specialty3;
  my $primarySpecialty4    = $self->specialty4;

  # Calculate primary buffs
  $self->general($self->pair->primary);
  $self->isPrimary(1);
  $self->SUPER::updateBuffs();

  $self->log_debug(sprintf('After primary updateBuffs: %s',
    Data::Printer::np($self->buffValues, max_depth => 2)));

  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }

  # Switch to secondary levels
  $self->covenantLevel($self->secondaryCovenantLevel);
  $self->specialty1($self->secondarySpecialty1);
  $self->specialty2($self->secondarySpecialty2);
  $self->specialty3($self->secondarySpecialty3);
  $self->specialty4($self->secondarySpecialty4);

  # Calculate secondary buffs
  $self->general($self->pair->secondary);
  $self->isPrimary(0);
  $self->SUPER::updateBuffs();
  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }

  # Restore primary levels
  $self->covenantLevel($primaryCovenantLevel);
  $self->specialty1($primarySpecialty1);
  $self->specialty2($primarySpecialty2);
  $self->specialty3($primarySpecialty3);
  $self->specialty4($primarySpecialty4);
}

sub updatePrimaryDebuffs ($self, $precomputed = undef) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->log_error(sprintf(
      '%s requires a Game::EvonyTKR::Model::General::Pair',
      __PACKAGE__));
    return;
  }
  if ($precomputed && ref($precomputed) && ref($precomputed) eq 'HASH') {
    $self->debuffValues($precomputed);
  }
  else {
    $self->general($self->pair->primary);
    $self->isPrimary(1);
    $self->SUPER::updateDebuffs();
  }
  $self->log_debug(sprintf('After primary updateDebuffs: %s',
    Data::Printer::np($self->debuffValues, max_depth => 2)));

  foreach my $troopType (keys %{ $self->debuffValues }) {
    foreach my $attribute (keys %{ $self->debuffValues->{$troopType} }) {
      $self->pairDebuffValues->{$troopType}->{$attribute} +=
        $self->debuffValues->{$troopType}->{$attribute};
    }
  }
}

sub updateSecondaryDebuffs ($self, $precomputed = undef) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->log_error(sprintf(
      '%s requires a Game::EvonyTKR::Model::General::Pair',
      __PACKAGE__));
    return;
  }
  # Store primary levels
  my $primaryCovenantLevel = $self->covenantLevel;
  my $primarySpecialty1    = $self->specialty1;
  my $primarySpecialty2    = $self->specialty2;
  my $primarySpecialty3    = $self->specialty3;
  my $primarySpecialty4    = $self->specialty4;

  # Switch to secondary levels
  $self->covenantLevel($self->secondaryCovenantLevel);
  $self->specialty1($self->secondarySpecialty1);
  $self->specialty2($self->secondarySpecialty2);
  $self->specialty3($self->secondarySpecialty3);
  $self->specialty4($self->secondarySpecialty4);

  # Calculate secondary buffs
  if ($precomputed && ref($precomputed) && ref($precomputed) eq 'HASH') {
    $self->debuffValues($precomputed);
  }
  else {
    $self->general($self->pair->secondary);
    $self->isPrimary(0);
    $self->SUPER::updateDebuffs();
  }
  foreach my $troopType (keys %{ $self->debuffValues }) {
    foreach my $attribute (keys %{ $self->debuffValues->{$troopType} }) {
      $self->pairDebuffValues->{$troopType}->{$attribute} +=
        $self->debuffValues->{$troopType}->{$attribute};
    }
  }

  # Restore primary levels
  $self->covenantLevel($primaryCovenantLevel);
  $self->specialty1($primarySpecialty1);
  $self->specialty2($primarySpecialty2);
  $self->specialty3($primarySpecialty3);
  $self->specialty4($primarySpecialty4);
}

sub updateDebuffs ($self) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->log_error(sprintf(
      '%s requires a Game::EvonyTKR::Model::General::Pair',
      __PACKAGE__));
    return;
  }

  # Store primary levels
  my $primaryCovenantLevel = $self->covenantLevel;
  my $primarySpecialty1    = $self->specialty1;
  my $primarySpecialty2    = $self->specialty2;
  my $primarySpecialty3    = $self->specialty3;
  my $primarySpecialty4    = $self->specialty4;

  # Calculate primary debuffs
  $self->general($self->pair->primary);
  $self->isPrimary(1);
  $self->SUPER::updateDebuffs();
  foreach my $troopType (keys %{ $self->debuffValues }) {
    foreach my $attribute (keys %{ $self->debuffValues->{$troopType} }) {
      $self->pairDebuffValues->{$troopType}->{$attribute} +=
        $self->debuffValues->{$troopType}->{$attribute};
    }
  }

  # Switch to secondary levels
  $self->covenantLevel($self->secondaryCovenantLevel);
  $self->specialty1($self->secondarySpecialty1);
  $self->specialty2($self->secondarySpecialty2);
  $self->specialty3($self->secondarySpecialty3);
  $self->specialty4($self->secondarySpecialty4);

  # Calculate secondary debuffs
  $self->general($self->pair->secondary);
  $self->isPrimary(0);
  $self->SUPER::updateDebuffs();
  foreach my $troopType (keys %{ $self->debuffValues }) {
    foreach my $attribute (keys %{ $self->debuffValues->{$troopType} }) {
      $self->pairDebuffValues->{$troopType}->{$attribute} +=
        $self->debuffValues->{$troopType}->{$attribute};
    }
  }

  # Restore primary levels
  $self->covenantLevel($primaryCovenantLevel);
  $self->specialty1($primarySpecialty1);
  $self->specialty2($primarySpecialty2);
  $self->specialty3($primarySpecialty3);
  $self->specialty4($primarySpecialty4);
}

# Override getGenericBookValue to check compatibility with both generals in pair
sub _getGenericBookValue_impl ($self, $attribute, $troopType) {
  my $total = 0;
  state $books_helper;
  $books_helper //= do {
    my $helper = eval {
      Game::EvonyTKR::Model::Base->new->with_roles(
        'Game::EvonyTKR::Role::Constants::BuffConstants',
        'Game::EvonyTKR::Role::Constants::GeneralConstants',
        'Game::EvonyTKR::Role::Constants::Books',
        'Game::EvonyTKR::Role::Books',
      );
    };
    if ($@) {
      $self->log_error("Cannot create books helper: $@");
      return $total;
    }
    $helper;
  };

  # Determine which general is current and which is other
  my $current_general = $self->general;
  my $other_general =
    $self->isPrimary ? $self->pair->secondary : $self->pair->primary;

  # Cache key for book compatibility (instance-level cache)
  my $compat_cache = $self->_private->{book_compat_cache} //= {};

  # Helper to check compatibility with caching
  my $check_compat = sub {
    my ($general, $book, $same_side) = @_;
    my $cache_key = join(':', $general->name, $book->name, $same_side);
    return $compat_cache->{$cache_key} //=
      $self->bc->is_general_and_book_compatible($general, $book,
      { same_side => $same_side });
  };

  # Special case for March Size - it's universal, not troop-specific
  if ($attribute eq 'March Size') {

    my $MS = $books_helper->get_generic_book('March Size', $self->bestLevel);
    if ( $MS
      && $check_compat->($current_general, $MS, 1)
      && $check_compat->($other_general,   $MS, 0)) {
      $total += $MS->buffs->[0]->value->number;
    }
    return $total;
  }

  # Overall isn't a real troop type - use general's primary type
  if ($troopType eq 'Overall') {
    $troopType =
      ref($current_general->type)
      ? $current_general->type->[0]
      : $current_general->type;
    $troopType =~ s/_specialist$//;
    $troopType =~ s/_/ /g;
    $troopType = ucfirst($troopType) . ' Troops';
  }

  # Convert troop type to target type key
  my $tt         = $troopType =~ s/ Troops$//r;
  my $targetType = lc($tt) . '_specialist';
  $targetType =~ s/siege machines/siege/;
  $targetType =~ s/ /_/g;

  # Determine which book list to use
  my $key = $self->activationType eq 'PvM' ? 'PvM' : 'default';

  # Get the best books for this troop type
  my $book_priorities = $books_helper->BestSkillBooks->{$targetType}->{$key}
    // $books_helper->BestSkillBooks->{$troopType}->{$key} // {};

  $self->log_debug(sprintf(
'Pair getGenericBookValue: attr=%s, troopType=%s, targetType=%s, key=%s, found %d books',
    $attribute, $troopType, $targetType,
    $key,       scalar keys %$book_priorities
  ));

  # Sort books by priority and check until we find 3 compatible ones
  my @sorted_books = sort { $book_priorities->{$a} <=> $book_priorities->{$b} }
    keys %$book_priorities;

  my $found = 0;
  for my $book_name (@sorted_books) {
    last if $found >= 3;    # Pairs get 6 total but 3 per general

    # Extract base name (remove "Level X" prefix)
    my $base_name = $book_name =~ s/^Level \d+ //r;
    my $book = $books_helper->get_generic_book($base_name, $self->bestLevel);

    next unless $book;

    # Check if this book provides the attribute we're looking for
    my $provides_attr = 0;
    for my $buff (@{ $book->buffs }) {
      if ($buff->attribute eq $attribute
        && ($buff->targetedType // '') eq $troopType) {
        $provides_attr = 1;
        last;
      }
    }

    next unless $provides_attr;

    # Check compatibility with current general (same side) - cached
    my $compat_current = $check_compat->($current_general, $book, 1);

# Check compatibility with other general (different side - no partial conflicts) - cached
    my $compat_other = $check_compat->($other_general, $book, 0);

    $self->log_debug(sprintf(
'Book %s for %s (%s): provides_attr=%d, compat_current=%d, compat_other=%d',
      $book->name,    $attribute,      $current_general->name,
      $provides_attr, $compat_current, $compat_other
    ));

# Book must be compatible with current general AND not conflict with other general
    if ($compat_current && $compat_other) {
      for my $buff (@{ $book->buffs }) {
        if ($buff->attribute eq $attribute
          && ($buff->targetedType // '') eq $troopType) {
          $total += $buff->value->number;
          $self->log_debug(sprintf(
            'Adding %d from %s for %s, total now %d',
            $buff->value->number,   $book->name,
            $current_general->name, $total
          ));
        }
      }
      $found++;
    }
  }

  return $total;
}

1;
__END__

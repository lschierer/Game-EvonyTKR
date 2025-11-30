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

sub updateBuffs ($self) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->logger->error(
      sprintf(
        '%s requires a Game::EvonyTKR::Model::General::Pair', __PACKAGE__
      )
    );
    return;
  }

  # Calculate primary buffs
  $self->general($self->pair->primary);
  $self->SUPER::updateBuffs();
  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }

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
}

sub updateDebuffs ($self) {
  unless ($self->pair
    && ref($self->pair)
    && blessed($self->pair)
    && $self->pair->isa('Game::EvonyTKR::Model::General::Pair')) {
    $self->logger->error(
      sprintf(
        '%s requires a Game::EvonyTKR::Model::General::Pair', __PACKAGE__
      )
    );
    return;
  }

  # Calculate primary debuffs
  $self->general($self->pair->primary);
  $self->SUPER::updateDebuffs();
  foreach my $troopType (keys %{ $self->debuffValues }) {
    foreach my $attribute (keys %{ $self->debuffValues->{$troopType} }) {
      $self->pairDebuffValues->{$troopType}->{$attribute} +=
        $self->debuffValues->{$troopType}->{$attribute};
    }
  }

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
}

# Override getGenericBookValue to check compatibility with both generals in pair
sub _getGenericBookValue_impl ($self, $attribute, $troopType) {
  my $total = 0;

  # Determine which general is current and which is other
  my $current_general = $self->general;
  my $other_general =
    $self->isPrimary ? $self->pair->secondary : $self->pair->primary;

  # Special case for March Size - it's universal, not troop-specific
  if ($attribute eq 'March Size') {
    state $books_helper;
    $books_helper //= do {
      my $helper = eval {
        Game::EvonyTKR::Model::Base->new->with_roles(
          'Game::EvonyTKR::Role::Persistence',);
      };
      if ($@) {
        $self->logger->error("Cannot create books helper: $@");
        return $total;
      }
      $helper;
    };

    my $MS = $books_helper->get_generic_book('March Size', $self->bestLevel);
    if (
      $MS
      && $self->bc->is_general_and_book_compatible($current_general, $MS,
        { same_side => 1 })
      && $self->bc->is_general_and_book_compatible(
        $other_general, $MS, { same_side => 0 }
      )
    ) {
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

  # Get books_helper with BestSkillBooks constants
  state $books_helper;
  $books_helper //= do {
    my $helper = eval {
      Game::EvonyTKR::Model::Base->new->with_roles(
        'Game::EvonyTKR::Role::Persistence',
        'Game::EvonyTKR::Role::Constants::Books',
      );
    };
    if ($@) {
      $self->logger->error("Cannot create books helper: $@");
      return $total;
    }
    $helper;
  };

  # Convert troop type to target type key
  my $tt         = $troopType =~ s/ Troops$//r;
  my $targetType = lc($tt) . '_specialist';
  $targetType =~ s/siege machines/siege/;
  $targetType =~ s/ /_/g;

  # Determine which book list to use
  my $key = $self->activationType eq 'PvM' ? 'PvM' : 'default';

  # Get the best books for this troop type
  my $book_priorities = $books_helper->BestSkillBooks->{$targetType}->{$key}
    // {};

  $self->logger->debug(sprintf(
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

    # Check compatibility with current general (same side)
    my $compat_current =
      $self->bc->is_general_and_book_compatible($current_general, $book,
      { same_side => 1 });

# Check compatibility with other general (different side - no partial conflicts)
    my $compat_other =
      $self->bc->is_general_and_book_compatible($other_general, $book,
      { same_side => 0 });

    $self->logger->debug(sprintf(
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
          $self->logger->debug(sprintf(
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

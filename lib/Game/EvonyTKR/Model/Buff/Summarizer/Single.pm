package Game::EvonyTKR::Model::Buff::Summarizer::Single;
use v5.42.0;
use utf8::all;
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Service::Conflicts;
use Mojo::Base 'Game::EvonyTKR::Model::Buff::Summarizer', -signatures;
use List::AllUtils qw(first any all none uniq);
use Carp;
use diagnostics;

# Override getGenericBookValue to check compatibility with both generals in pair
sub _getGenericBookValue_impl ($self, $attribute, $troopType) {
  my $total = 0;

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
      && $self->bc->is_general_and_book_compatible(
        $self->general, $MS, { same_side => 1 }
      )
    ) {
      $total += $MS->buffs->[0]->value->number;
    }
    return $total;
  }

  # Overall isn't a real troop type - use general's primary type
  # TODO: Handle Overall properly in book selection logic
  if ($troopType eq 'Overall') {
    $troopType =
      ref($self->general->type)
      ? $self->general->type->[0]
      : $self->general->type;
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
'getGenericBookValue: attr=%s, troopType=%s, targetType=%s, key=%s, found %d books',
    $attribute, $troopType, $targetType,
    $key,       scalar keys %$book_priorities
  ));

  # Sort books by priority and check until we find 3 compatible ones
  my @sorted_books = sort { $book_priorities->{$a} <=> $book_priorities->{$b} }
    keys %$book_priorities;

  my $found = 0;
  for my $book_name (@sorted_books) {
    last if $found >= 3;    # Stop after finding 3 compatible books

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

    # Check compatibility
    my $compat =
      $self->bc->is_general_and_book_compatible($self->general, $book,
      { same_side => 1 });

    $self->logger->debug(sprintf(
      'Book %s for %s: provides_attr=%d, compat=%d',
      $book->name, $attribute, $provides_attr, $compat
    ));

    if ($compat) {
      for my $buff (@{ $book->buffs }) {
        if ($buff->attribute eq $attribute
          && ($buff->targetedType // '') eq $troopType) {
          $total += $buff->value->number;
          $self->logger->debug(sprintf(
            'Adding %d from %s, total now %d',
            $buff->value->number, $book->name, $total
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

package Game::EvonyTKR::Role::Books;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use List::AllUtils qw(any all none uniq);

# requires that Game::EvonyTKR::Role::Constants::BuffConstants be composed in.
# requires that Game::EvonyTKR::Role::Constants::Books be composed in.
# requires that Game::EvonyTKR::Role::Logging be composed in.

sub load_best_skill_books ($self, $general, $targetType, $activationType,
  $desiredCount = 3) {
  $self->log_info(sprintf(
    'finding best %s skill books for %s',
    $activationType, $general->name
  ));
  my $key = $activationType eq 'PvM' ? 'PvM' : 'default';

  $key = 'default' if ($targetType eq 'wall');

  if ($desiredCount <= 3 && $desiredCount != 6) {
    $desiredCount = 3;
  }

# TODO: partially implemented support for pairs using $desiredCount variable
#       This would work except for the todo items below.
# TODO: Implement book conflict detection
#       (requires an instance of Game::EvonyTKR::Service::Conflicts )
# TODO: Handle partial conflicts
#       A) book works for single general but conflicts as a pair
#       B) ability to display the potential for this even in the single general UI.
#          This one (B) may require a separate method from this for the UI to call.

  my @books;
  my $generic_dir = $self->collection_dir->child('generic books');
  my @sorted_book_names;
  if ( exists $self->BestSkillBooks->{$targetType}
    && exists $self->BestSkillBooks->{$targetType}->{$key}) {
    @sorted_book_names = sort {
      $self->BestSkillBooks->{$targetType}->{$key}->{$a}
        <=> $self->BestSkillBooks->{$targetType}->{$key}->{$b}
    } keys %{ $self->BestSkillBooks->{$targetType}->{$key} };
  }
  elsif (exists $self->BestSkillBooks->{$targetType}) {
    @sorted_book_names = sort {
      $self->BestSkillBooks->{$targetType}->{'default'}->{$a}
        <=> $self->BestSkillBooks->{$targetType}->{'default'}->{$b}
    } keys %{ $self->BestSkillBooks->{$targetType}->{'default'} };
  }
  else {
    $self->log_error(
      sprintf(
        'targetType "%s" is not supported by BestSkillBooks', $targetType
      )
    );
    return [];
  }

  my $level = $self->bestLevel;

  foreach my $book_name (@sorted_book_names) {
    my $base_name = $book_name =~ s/^Level \d+ //r;
    my $book      = $self->get_generic_book($base_name, $level);
    unless ($book && ref($book) && $book->isa('Game::EvonyTKR::Model::Book')) {
      $self->log_error("Cannot find $book_name");
      next;
    }
    $self->log_info(
      sprintf('Picked book "%s" for "%s"', $book_name, $general->name));
    push @books, $book;
    last if (scalar @books >= $desiredCount);    # Single general gets 3 books
  }

  return \@books;
}

sub load_mandatory_skill_books ($self) {
  my @books;
  my $level = $self->bestLevel;
  # Ensure required books are present for buff summarizer
  my @ttlist = values $self->TroopTypeValues->%*;
  foreach my $attr ('Attack', 'Defense', 'HP', 'March Size') {
    foreach my $tt (@ttlist) {
      $tt =~ s/s$//;

      my $book_name;
      if ($attr ne 'March Size') {
        $book_name = sprintf('%s %s', $tt, $attr);
      }
      else {
        $book_name = sprintf('%s', $attr);
      }

      if (scalar(@books) == 0) {
        my $book = $self->get_generic_book($book_name, $level);
        unless ($book
          && ref($book)
          && $book->isa('Game::EvonyTKR::Model::Book')) {
          $self->log_error("Cannot find $book_name");
          next;
        }
        push @books, $book;
      }
      elsif (
        scalar(@books)
        && List::AllUtils::none { defined($_) && $_->name eq $book_name }
        @books
      ) {
        my $book = $self->get_generic_book($book_name, $level);
        unless ($book
          && ref($book)
          && $book->isa('Game::EvonyTKR::Model::Book')) {
          $self->log_error("Cannot find $book_name");
          next;
        }
        push @books, $book;
      }
    }
  }

  return \@books;
}
1;
__END__

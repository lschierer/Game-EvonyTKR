use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Log::Log4perl::Level;
require Mojo::File;
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Books {
  use Mojo::Base -role, -signatures;
  use List::AllUtils qw( any none uniq all );
  use List::UtilsBy;
  use Log::Any;
  use Carp;

  my $logger = Log::Any->get_logger(category => __PACKAGE__);
  our $namespace = 'books__';

  has 'builtin_book_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'builtin_books:');
  };

  has 'generic_book_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'generic_books:');
  };

  sub add_builtin_book ($self, $item) {
    my $key = $item->name =~ s/ /_/gr;
    $key = $self->normalize($key);
    return $self->builtin_book_cache->set($key, $item->to_wire_hash());
  }

  sub add_generic_book ($self, $item) {
    my $nn = $item->name =~ s/ /_/gr;
    $nn = $self->normalize($nn);
    my $key = sprintf('%s_level_%s', $nn, $item->level);
    return $self->generic_book_cache->set($key, $item->to_wire_hash());
  }

  sub get_builtin_book ($self, $name) {
    state $builtin_books = {};

    $logger->debug("get_builtin_book called for: $name");

    if (exists $builtin_books->{ $self->normalize($name) }) {
      $logger->debug("Returning builtin_books $name from local cache");
      return $builtin_books->{ $self->normalize($name) };
    }

    my $key = $name =~ s/ /_/gr;
    $key = $self->normalize($key);
    $logger->debug("Looking for cache key: $key");

    my $wire_data = $self->builtin_book_cache->get($key);
    unless (defined($wire_data)) {
      $logger->warn("No wire_data found for key: $key");
      return;
    }

    $logger->debug("Found wire_data, attempting to build book");
    my $book =
      Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);

    unless (defined($book)) {
      $logger->error("Factory failed to build book from wire_data");
      return;
    }

    $logger->debug("Successfully built book: " . $book->name);
    $builtin_books->{ $self->normalize($name) } = $book;
    return $book;
  }

  sub get_generic_book ($self, $name, $level) {
    state $generic_books = {};

    if (exists $generic_books->{ $self->normalize($name) }) {
      if (exists $generic_books->{ $self->normalize($name) }->{$level}) {
        $logger->debug("Returning generic book $name from local cache");
        return $generic_books->{ $self->normalize($name) }->{$level};
      }
    }

    my $key = $name =~ s/ /_/gr;
    $key = $self->normalize($key);
    $key = sprintf('%s_level_%s', $key, $level);

    my $wire_data = $self->generic_book_cache->get($key);
    return unless defined($wire_data);

    my $book =
      Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);
    $generic_books->{ $self->normalize($name) }->{$level} = $book;
    return $book;
  }

  sub list_generic_books ($self, $app = undef) {
    state $returnlist;
    $returnlist //= do {
      my $mh            = Mojo::Home->new->detect('Game::EvonyTKR');
      my $collectionDir = $mh->child('share/collections/data');
      my $gbdir         = $collectionDir->child('generic books');
      my @suffixlist    = ('.yaml', '.yml');
      my @files = $gbdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
        ->sort->map(sub { return $_->basename(@suffixlist) })->each;
      [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
    };
    return $returnlist;
  }

  sub list_builtin_books ($self,) {
    state $returnlist;
    $returnlist //= do {
      my $mh            = Mojo::Home->new->detect('Game::EvonyTKR');
      my $collectionDir = $mh->child('share/collections/data');
      my $bbdir         = $collectionDir->child('skill books');
      my @suffixlist    = ('.yaml', '.yml');
      my @files = $bbdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
        ->sort->map(sub { return $_->basename(@suffixlist) })->each;
      [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
    };
    return $returnlist;
  }

  sub load_best_skill_books ($self, $general, $targetType, $activationType,
    $desiredCount = 3) {
    $self->logger->info(sprintf(
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
#       (requires an instance of Game::EvonyTKR::Model::General::Conflict::Book )
# TODO: Handle partial conflicts
#       A) book works for single general but conflicts as a pair
#       B) ability to display the potential for this even in the single general UI.
#          This one (B) may require a separate method from this for the UI to call.

    my @books;
    my $generic_dir       = $self->collection_dir->child('generic books');
    my @sorted_book_names = sort {
      $self->BestSkillBooks->{$targetType}->{$key}->{$a}
        <=> $self->BestSkillBooks->{$targetType}->{$key}->{$b}
    } keys %{ $self->BestSkillBooks->{$targetType}->{$key} };

    my $level = $self->bestLevel;

    foreach my $book_name (@sorted_book_names) {
      my $base_name = $book_name =~ s/^Level \d+ //r;
      my $book      = $self->get_generic_book($base_name, $level);
      unless ($book && ref($book) && $book->isa('Game::EvonyTKR::Model::Book'))
      {
        $self->logger->error("Cannot find $book_name");
        next;
      }
      $self->logger->info(
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
    foreach my $attr ('Attack', 'Defense', 'HP', 'March Size') {
      foreach my $tt ('Mounted Troop', 'Ranged Troop', 'Ground Troop',
        'Siege Machine') {

        my $book_name;
        if ($attr ne 'March Size') {
          $book_name = sprintf('Level %s %s %s', $level, $tt, $attr);
        }
        else {
          $book_name = sprintf('Level %s %s', $level, $attr);
        }
        unless (any { $_->name eq $book_name } @books) {
          my $book = $self->get_generic_book($book_name, $level);
          unless ($book) {
            $self->logger->error("Cannot find $book_name");
            next;
          }
          push @books, $book;
        }
      }
    }

    return \@books;
  }
}
1;
__END__

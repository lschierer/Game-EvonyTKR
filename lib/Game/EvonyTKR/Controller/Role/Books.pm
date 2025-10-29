use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Log::Log4perl::Level;
require Mojo::File;
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Books {
  use Mojo::Base -role,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role;
  use List::AllUtils qw(uniq);
  use List::UtilsBy;
  use Carp;

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

    $self->logger->debug("get_builtin_book called for: $name");

    if (exists $builtin_books->{ $self->normalize($name) }) {
      $self->logger->debug("Returning builtin_books $name from local cache");
      return $builtin_books->{ $self->normalize($name) };
    }

    my $key = $name =~ s/ /_/gr;
    $key = $self->normalize($key);
    $self->logger->debug("Looking for cache key: $key");

    my $wire_data = $self->builtin_book_cache->get($key);
    unless (defined($wire_data)) {
      $self->logger->warn("No wire_data found for key: $key");
      return;
    }

    $self->logger->debug("Found wire_data, attempting to build book");
    my $book =
      Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);

    unless (defined($book)) {
      $self->logger->error("Factory failed to build book from wire_data");
      return;
    }

    $self->logger->debug("Successfully built book: " . $book->name);
    $builtin_books->{ $self->normalize($name) } = $book;
    return $book;
  }

  sub get_generic_book ($self, $name, $level) {
    state $generic_books = {};

    if (exists $generic_books->{ $self->normalize($name) }) {
      if (exists $generic_books->{ $self->normalize($name) }->{$level}) {
        $self->logger->debug("Returning generic book $name from local cache");
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

  sub list_generic_books ($self, $app) {
    my $returnlist;
    unless (defined($app)) {
      $self->logger->logcroak('$app must be defined');
    }
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $gbDir      = $collectionDir->child('generic books');
    my @suffixlist = ('.yaml', '.yml');
    $gbDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->sort->map(sub {
      my $ib = $_->basename(@suffixlist);
      $ib = lc($self->normalize($ib));
      if (List::AllUtils::none { $_ eq $ib } $returnlist->@*) {
        push @$returnlist, $ib;
      }
    });

    return $returnlist;
  }

  sub list_builtin_books ($self, $app) {
    my $returnlist;
    unless (defined($app)) {
      $self->logger->logcroak('$app must be defined');
    }
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $gbDir      = $collectionDir->child('skill books');
    my @suffixlist = ('.yaml', '.yml');
    $gbDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->sort->map(sub {
      my $ib = $_->basename(@suffixlist);
      $ib = lc($self->normalize($ib));
      if (List::AllUtils::none { $_ eq $ib } $returnlist->@*) {
        $self->logger->debug(
          sprintf('adding "%s" to the list of builtins', $ib));
        push @$returnlist, $ib;
      }
      else {
        $self->logger->debug(
          sprintf('excluding "%s" from the list of builtins', $ib));
      }
    });

    return $returnlist;
  }
}
1;
__END__

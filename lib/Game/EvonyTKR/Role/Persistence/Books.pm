package Game::EvonyTKR::Role::Persistence::Books;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;

has 'builtin_book_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'builtin_books:');
};

has 'generic_book_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'generic_books:');
};

##############################################################################
# Builtin Books
##############################################################################

sub add_builtin_book ($self, $book) {
  my $name = $book->name;
  $self->persistence->store_builtin_book($name, $book->to_wire_hash());
  my $key = $name =~ s/ /_/gr;
  $key = $self->normalize($key);
  $self->builtin_book_cache->set($key, $book->to_wire_hash());
  return 1;
}

sub get_builtin_book ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $builtin_books = {};

  $self->logger->debug("get_builtin_book called for: $name");

  my $key = $name =~ s/ /_/gr;
  $key = $self->normalize($key);

  if (exists $builtin_books->{$key}) {
    $self->logger->debug("Returning builtin book $name from state cache");
    return $builtin_books->{$key};
  }

  my $wire_data = $self->builtin_book_cache->get($key);

  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_builtin_book($name);

    if (defined($wire_data)) {
      $self->builtin_book_cache->set($key, $wire_data);
    }
  }

  unless (defined($wire_data)) {
    $self->logger->warn("No wire_data found for key: $key");
    return;
  }

  my $book =
    Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);

  unless (defined($book)) {
    $self->logger->error("Factory failed to build book from wire_data");
    return;
  }

  $self->logger->debug("Successfully built book: " . $book->name);
  $builtin_books->{$key} = $book;
  return $book;
}

sub list_builtin_books ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $bbdir         = $collectionDir->child('skill books');
  my @suffixlist    = ('.yaml', '.yml');
  my @files         = $bbdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

##############################################################################
# Generic Books
##############################################################################

sub add_generic_book ($self, $book) {
  my $name  = $book->name;
  my $level = $book->level;
  $self->persistence->store_generic_book($name, $level, $book->to_wire_hash());
  my $nn = $name =~ s/ /_/gr;
  $nn = $self->normalize($nn);
  my $key = sprintf('%s_level_%s', $nn, $level);
  $self->generic_book_cache->set($key, $book->to_wire_hash());
  return 1;
}

sub get_generic_book ($self, $name, $level) {
  require Game::EvonyTKR::Model::Factory;

  state $generic_books = {};

  my $key = $name =~ s/ /_/gr;
  $key = $self->normalize($key);
  $key = sprintf('%s_level_%s', $key, $level);

  if (exists $generic_books->{$key}) {
    $self->logger->debug("Returning generic book $name from state cache");
    return $generic_books->{$key};
  }

  my $wire_data = $self->generic_book_cache->get($key);

  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_generic_book($name, $level);

    if (defined($wire_data)) {
      $self->generic_book_cache->set($key, $wire_data);
    }
  }

  return unless defined($wire_data);

  my $book =
    Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);
  $generic_books->{$key} = $book if defined($book);
  return $book;
}

sub list_generic_books ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $gbdir         = $collectionDir->child('generic books');
  my @suffixlist    = ('.yaml', '.yml');
  my @files         = $gbdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Books - Book persistence operations

=head1 DESCRIPTION

Handles both builtin and generic skill books.

=cut

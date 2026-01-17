package Game::EvonyTKR::Role::Persistence::Books;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
with 'Game::EvonyTKR::Role::Persistence::Core';

##############################################################################
# Builtin Books
##############################################################################
my $logger;

sub add_builtin_book ($self, $book) {
  my $key = lc($self->normalize($book->name));
  return $self->persistence->store_builtin_book($key, $book->to_wire_hash());
}

sub get_builtin_book ($self, $name) {
  require Game::EvonyTKR::Model::Factory;
  $logger //= WebFramework::Role::Logger::get_logger(__PACKAGE__);

  state $builtin_books = {};

  $logger->debug("get_builtin_book called for: $name");

  my $key = lc($self->normalize($name));

  if (exists $builtin_books->{$key}) {
    $logger->debug("Returning builtin book $name from state cache");
    return $builtin_books->{$key};
  }

  # Load directly from SQLite
  my $wire_data = $self->persistence->get_builtin_book($key);

  unless (defined($wire_data)) {
    $logger->warn("No built in book wire_data found for key: $key");
    return;
  }

  my $book =
    Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);

  unless (defined($book)) {
    $logger->error("Factory failed to build book from wire_data");
    return;
  }

  $logger->debug("Successfully built book: " . $book->name);
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
  $logger //= WebFramework::Role::Logger::get_logger(__PACKAGE__);
  my $key = lc($self->normalize($book->name));
  $key = sprintf('level %s %s', $book->level, $key);
  $logger->debug(sprintf(
    'persistence role add_generic_book called for name "%s" level "%s"',
    $book->name, $book->level,
  ));
  return $self->persistence->store_generic_book($key, $book->to_wire_hash());
}

sub get_generic_book ($self, $name, $level) {
  $logger //= WebFramework::Role::Logger::get_logger(__PACKAGE__);
  require Game::EvonyTKR::Model::Factory;
  $logger->debug(sprintf(
    'persistence role get_generic_book called for name "%s" level "%s"',
    $name, $level
  ));

  my $key = lc($self->normalize($name));
  $key = sprintf('level %s %s', $level, $key);

  # Load directly from SQLite
  my $wire_data = $self->persistence->get_generic_book($key);

  return unless defined($wire_data);

  my $book =
    Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);

  return $book;
}

sub list_generic_books ($self, $level) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $gbdir         = $collectionDir->child('generic books');
  my @suffixlist    = ('.yaml', '.yml');
  my @files         = $gbdir->list->grep(
    sub { $_->basename =~ /^Level\s+${level}\s+.+\.ya?ml$/i && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  my @intermediate = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;

  my @final;
  foreach my $ib (@intermediate) {
    $ib =~ s/^Level\s+\d+\s+//i;
    push @final, $ib;
  }
  return \@final;
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Books - Book persistence operations

=head1 DESCRIPTION

Handles both builtin and generic skill books.

=cut

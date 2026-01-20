package Game::EvonyTKR::Loader::Books;
use v5.42.0;
use utf8::all;

use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';

use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;
require Game::EvonyTKR::Model::Book;

has data_dir => (
  is       => 'ro',
  required => 1,
);

# Separate storage for skill books and generic books
has skill_books => (
  is      => 'rw',
  default => sub { {} },
);

has generic_books => (
  is      => 'rw',
  default => sub { {} },
);

has _failed_files => (
  is      => 'rw',
  default => sub { [] },
);

sub load_all {
  my ($self) = @_;

  my $base_dir = path($self->data_dir);
  unless ($base_dir->exists && $base_dir->is_dir) {
    $self->logger->error("Books data directory not found: $base_dir");
    return 0;
  }

  my $skill_dir   = $base_dir->child('skill books');
  my $generic_dir = $base_dir->child('generic books');

  my $loaded = 0;
  $self->_failed_files([]);

  # Load skill books
  if ($skill_dir->exists && $skill_dir->is_dir) {
    $loaded += $self->_load_directory($skill_dir, 'skill');
  }
  else {
    $self->logger->warn("Skill books directory not found: $skill_dir");
  }

  # Load generic books
  if ($generic_dir->exists && $generic_dir->is_dir) {
    $loaded += $self->_load_directory($generic_dir, 'generic');
  }
  else {
    $self->logger->warn("Generic books directory not found: $generic_dir");
  }

  # Report summary of failures prominently
  my @failed = @{ $self->_failed_files };
  if (@failed) {
    $self->logger->error("=" x 60);
    $self->logger->error("!!! BOOKS LOADER: " . scalar(@failed) . " FILE(S) FAILED TO LOAD !!!");
    for my $failure (@failed) {
      $self->logger->error("  - $failure->{file}");
      $self->logger->error("    Error: $failure->{error}");
    }
    $self->logger->error("=" x 60);
    warn sprintf("BOOKS LOADER: %d file(s) failed to load! Check logs for details.\n", scalar(@failed));
  }

  $self->logger->info("Loaded $loaded total books");
  return $loaded;
}

sub _load_directory {
  my ($self, $dir, $type) = @_;

  my @yaml_files = $dir->children(qr/\.ya?ml$/);
  $self->logger->info(
    sprintf("Found %d $type book files to load", scalar @yaml_files));

  my $loaded = 0;
  for my $file (@yaml_files) {
    eval {
      my $data = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($file->slurp_utf8);

      unless ($data->{name}) {
        push @{ $self->_failed_files }, { file => "$file", error => "No 'name' field in YAML" };
        $self->logger->error("!!! YAML LOAD FAILED !!! $file - no name field");
        return;
      }

      my $book = Game::EvonyTKR::Model::Book->from_hash($data);

      # Store in appropriate hash using normalized keys
      if ($type eq 'skill') {
        my $normalized_key = $self->normalize($book->name);
        $self->skill_books->{$normalized_key} = $book;
        $self->logger->debug(
          "Loaded $type book: " . $book->name . " (key: $normalized_key)");
      }
      else {
        # For generic books, key by "name-level"
        my $key = $book->name;
        if ($book->can('level') && defined $book->level) {
          $key = sprintf("%s (Level %d)", $book->name, $book->level);
        }
        my $normalized_key = $self->normalize($key);
        $self->generic_books->{$normalized_key} = $book;
        $self->logger->debug(
          "Loaded $type book: " . $book->name . " (key: $normalized_key)");
      }

      $loaded++;
    };
    if ($@) {
      my $error = $@;
      push @{ $self->_failed_files }, { file => "$file", error => $error };
      $self->logger->error("!!! YAML LOAD FAILED !!! File: $file");
      $self->logger->error("!!! YAML ERROR: $error");
    }
  }

  $self->logger->info("Loaded $loaded $type books");
  return $loaded;
}

sub get_skill_book {
  my ($self, $name) = @_;
  my $normalized_key = $self->normalize($name);
  return $self->skill_books->{$normalized_key};
}

sub get_generic_book {
  my ($self, $name) = @_;
  my $normalized_key = $self->normalize($name);
  return $self->generic_books->{$normalized_key};
}

# Unified method - tries skill books first, then generic
sub get_book {
  my ($self, $name) = @_;

  # Try skill books first (most built-in books are skill books)
  my $book = $self->get_skill_book($name);
  return $book if $book;

  # Try generic books
  return $self->get_generic_book($name);
}

sub list_skill_books {
  my ($self) = @_;
  return [sort keys %{ $self->skill_books }];
}

sub list_generic_books {
  my ($self) = @_;
  return [sort keys %{ $self->generic_books }];
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Loader::Books - Load book data from YAML files

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Books->new(
        data_dir => 'share/collections/data'
    );
    $loader->load_all();
    my $book = $loader->get_skill_book('Augustus');

=head1 DESCRIPTION

Simplified loader for book data. Loads both skill books and generic books
from their respective directories and keeps them in memory.

=cut

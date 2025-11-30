package Test::Package ;
use v5.42.0;
use utf8::all;
use experimental qw(class);
use Test2::V0;
use Path::Tiny;
require YAML::PP;
use List::AllUtils qw( first any none all );

  use Mojo::Base 'Game::EvonyTKR::Model::Base';
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',                         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',               -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',           -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',            -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',        -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',     -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books',                -role;
  use List::AllUtils qw( first );

  sub import_generic_books ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @books;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $gbdir = $collectionDir->child('generic books');

    $self->logger->debug(
      sprintf('found %s generic book files', $gbdir->list->size));

    foreach my $book_name ($self->list_generic_books->@*) {
      $self->logger->debug("importing $book_name");
      my $bn = lc($self->normalize($book_name));

      my ($file) = $gbdir->list->sort->grep(sub {
        my $b = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $b eq $bn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(
          sprintf('failed to find file for "%s"', $book_name));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $book = Game::EvonyTKR::Model::Book->from_hash($hashObject);
      unless ($book
        && ref($book)
        && blessed($book)
        && $book->isa('Game::EvonyTKR::Model::Book')) {
        $self->logger->error(sprintf(
          'failed to create a book with from_hash for "%s"',
          $book_name));
        return 0;
      }
      $self->add_generic_book($book);
      push @books, $book;
    }
    return \@books;
  }

  sub import_builtin_books ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @books;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $bbdir = $collectionDir->child('skill books');

    $self->logger->debug(
      sprintf('found %s builtin book files', $bbdir->list->size));

    foreach my $book_name ($self->list_builtin_books->@*) {
      $self->logger->debug("importing $book_name");
      my $bn = lc($self->normalize($book_name));

      my ($file) = $bbdir->list->sort->grep(sub {
        my $b = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $b eq $bn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(
          sprintf('failed to find file for "%s"', $book_name));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $book = Game::EvonyTKR::Model::Book->from_hash($hashObject);
      unless ($book
        && ref($book)
        && blessed($book)
        && $book->isa('Game::EvonyTKR::Model::Book')) {
        $self->logger->error(sprintf(
          'failed to create a book with from_hash for "%s"',
          $book_name));
        return 0;
      }
      $self->add_builtin_book($book);
      push @books, $book;
    }
    return \@books;
  }

  sub import_ascendingAttributes ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('ascending attributes');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_ascending_attributes->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object =
        Game::EvonyTKR::Model::AscendingAttributes->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::AscendingAttributes')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_ascending_attribute($object);
      push @objects, $object;
    }
    return \@objects;
  }

  sub import_generals ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('generals');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_generals->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object = Game::EvonyTKR::Model::General->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::General')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_general($object);
      push @objects, $object;
    }
    return \@objects;
  }

  sub import_covenants ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('covenants');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_covenants->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object = Game::EvonyTKR::Model::Covenant->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::Covenant')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_covenant($object);
      push @objects, $object;
    }
    return \@objects;
  }

  sub import_specialties ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('specialties');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_specialties->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object = Game::EvonyTKR::Model::Specialty->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::Specialty')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_specialty($object);
      push @objects, $object;
    }
    return \@objects;
  }

1;

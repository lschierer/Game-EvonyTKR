#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test2::V0;
use lib 'lib';

# File::Share requires the main module first
require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;

my $logger = Game::EvonyTKR::Log::Config->logger('Test');
$logger->info('Test script logging configured');

require Game::EvonyTKR::Role::Logger;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Service::Cache;

package Test::Package {
  use Mojo::Base -base,                                     -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',            -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',            -role;
  use Mojo::Base 'Game::EvonyTKR::Model::Role::Book',       -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books', -role;

  state $bookCache;

  sub load_Supreme_Power ($self) {
    my $entry = 'Supreme Power';
    my $book;

    $book      = $self->get_builtin_book($entry);
    my @suffixlist = ('.yaml', '.yml');

    if ( defined($book)
      && ref($book)
      && $book->isa('Game::EvonyTKR::Model::Book')) {
      my $result = sprintf('returning already loaded book "%s"', $book->name);
      $self->logger->info($result);
      return 1;
    }

    my $distDir = Mojo::Home->new->detect('Game::EvonyTKR')->to_string;
    my $collectionDir =
      Mojo::File->new($distDir)
      ->child('share/collections/data/');
    my $bookDir    = $collectionDir->child('skill books');
    my $ne         = lc($self->normalize($entry));
    my ($bookFile) = $bookDir->list->sort->grep(sub {
      my $b = lc($self->normalize($_->basename(@suffixlist)));
      if ($_ =~ m/\.ya?ml$/ && $b eq $ne) {
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless (defined($bookFile) && length($bookFile)) {
      my $errmessage =
        sprintf('failed to find file for entry "%s" in %s', $entry, $bookDir);
      $self->logger->error($errmessage);
      return 0;
    }
    $self->logger->debug(sprintf('found file "%s" for "%s"',
    $bookFile, $entry));

    my $bd  = $bookFile->slurp('UTF-8');
    my $bho = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($bd);
    $book = Game::EvonyTKR::Model::Book->from_hash($bho);

    unless ($book
      && Scalar::Util::blessed($book)
      && $book->isa('Game::EvonyTKR::Model::Book')) {
      my $errmessage = sprintf('failed to load book for file "%s"', $bookFile);
      $self->logger->error($errmessage);
      return 0;
    }
    $self->logger->debug(sprintf('got a book "%s" back from Game::EvonyTKR::Model::Book->from_hash',
    $book->name));

    my $add_result = $self->add_builtin_book($book);
    if (defined($add_result) && $add_result == 1) {
      my $result = sprintf('imported %s', $book->name);
      $self->logger->info($result);
      say sprintf('book_imported => %s', Data::Printer::np($book));
      return 1;
    }
    else {
      my $errmessage = sprintf('add to cache for %s failed: %s',
        $entry, $add_result // 'undef add result');
      $self->logger->error($errmessage);
      return 0;
    }
  }

  sub retrieve_Supreme_Power ($self){
    my $testBookName = 'Supreme Power';

    my $key = $testBookName =~ s/ /_/gr;
    $key = lc($self->normalize($key));

    my $book = $self->get_builtin_book($testBookName);
    warn "Retrieved book: " . (defined($book) ? ref($book) : 'undef');

    unless(defined($book) && blessed($book) && $book->DOES('Game::EvonyTKR::Model::Book')){
      my $all_items = $self->get_all_items();
      warn "Cache keys: " . join(', ', keys %$all_items);
      warn "Looking for key: '$key'";
      my $raw_value = $self->store->get($key);
      warn "Raw cache value: " . (defined($raw_value) ? length($raw_value) . " bytes" : 'undef');

      return 0;
    }
    return 1;
  }

  sub retrieve_Supreme_Power_with_helper ($self) {
    my ($book, $books_helper, $cache_store);
    my $testBookName = 'Supreme Power';
    eval {
      $books_helper = Mojo::Base->new->with_roles(
        'Game::EvonyTKR::Role::Logger',
        'Game::EvonyTKR::Role::Cache',
        'Game::EvonyTKR::Role::Common',
        'Game::EvonyTKR::Controller::Role::Books'
      );
    } or do {
      $self->logger->error(
        sprintf('eval failed; cannot define book helper: "%s"', $@));
      return;
    };

    eval { $cache_store = $books_helper->create_book_cache(); } or do {
      $self->logger->error(
        sprintf('eval failed; cannot create book cache from helper: "%s"', $@));
      return;
    };

    if (defined($book)) {
      $self->logger->debug(sprintf(
        'fetch returned book "%s" with name "%s" for builtInBookName "%s"',
        blessed($book), $book->can('name') ? $book->name : 'no name method',
        $testBookName
      ));
      return 1;
    }
    else {
      $self->logger->error(
        sprintf('failed to fetch book for builtin book "%s" from cache',
          $testBookName)
      );
      return 0;
    }
  }
}

my $test = Test::Package->new();
DOES_ok($test, ['Test::Package'], 'Test Class instantiated successfully');
ok($test->load_Supreme_Power(), 'Supreme Power Loaded');
ok($test->retrieve_Supreme_Power(), 'Supreme Power Retrieved');
ok($test->retrieve_Supreme_Power_with_helper(), 'Supreme Power Retrieved With Helper');
done_testing();

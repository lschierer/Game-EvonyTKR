use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book::BuiltinBook;
require Game::EvonyTKR::Model::Book::Manager;
use namespace::clean;

package Game::EvonyTKR::Plugins::BuiltinBooks {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  my $manager;

  # Specify which collection this controller handles
  sub collection_name {'skill books'}

  # Override loadItem to add any BuiltinBooks-specific processing

  # Register this when the application starts
  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    eval {
      $manager = Game::EvonyTKR::Model::Book::Manager->new();

      my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
      my $collection = $self->collection_name;
      my $SourceDir  = $distDir->child("collections/$collection");
      $manager->importAll($SourceDir);

      $logger->info(
        "Successfully loaded book manager with collection from $SourceDir");
    };
    if ($@) {
      $logger->error("Failed to initialize Book Manager: $@");
      $manager = undef;
    }

    $app->helper(
      get_builtinbook_manager => sub {
        return $manager;
      }
    );

    $app->helper(
      get_builtin_book_text => sub ($c, $book_name) {
        $app->log->debug("get_builtin_book_text for book '$book_name'");
        my $book = $manager->getBook($book_name);
        if ($book) {
          return $book->text();
        }
        else {
          $logger->warn("No book found for '$book_name'");
        }
        return "";
      }
    );
  }

}

1;

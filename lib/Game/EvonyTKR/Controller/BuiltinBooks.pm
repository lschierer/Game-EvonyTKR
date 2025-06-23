use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book::BuiltinBook;
require Game::EvonyTKR::Model::Book::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::BuiltinBooks {
  use Mojo::Base 'Game::EvonyTKR::Controller::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'skill books'}

  sub get_manager($self) {
    return $self->app->get_root_manager->bookManager;
  }

  # Register this when the application starts
  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_builtinbook_manager => sub {
        return $self->app->get_root_manager->bookManager;
      }
    );

    $app->helper(
      get_builtin_book_text => sub ($c, $book_name) {
        $app->log->debug("get_builtin_book_text for book '$book_name'");
        my $book = $c->app->get_root_manager->bookManager->getBook($book_name);
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

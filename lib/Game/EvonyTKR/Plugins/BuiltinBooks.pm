use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use namespace::clean;

package Game::EvonyTKR::Plugins::BuiltinBooks {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'skillbooks'}

  # Override loadItem to add any BuiltinBooks-specific processing

  # Helper method to get text or link to details
  sub get_book_text($self, $book_name) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    # Check if the book exists
    unless ($self->itemExists($book_name)) {
      $logger->error("failed to find book '$book_name' in index");
      return "Unknown book: $book_name";
    }

    # Lazy load the book data
    my $book = $self->loadItem($book_name);

    # If text is available, return it
    if ($book && $book->{text} && length($book->{text}) > 0) {
      return $book->{text};
    }

    # Otherwise, return a link to the details page
    my $url = $self->url_for('/BuiltinBooks/details/' . $book_name);

    return
qq{<a href="$url">$book_name does not have a text description available, see here for details.</a>};
  }

  # Register this when the application starts
  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_builtin_book_text => sub ($c, $book_name) {
        $app->log->debug("get_builtin_book_text for book '$book_name'");
        return $self->get_book_text($book_name);
      }
    );
  }

}

1;

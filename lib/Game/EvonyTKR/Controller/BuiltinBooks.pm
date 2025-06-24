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
  sub collection_name {
    return 'skill books';
  }

  sub get_manager($self) {
    return $self->app->get_root_manager->bookManager;
  }

  my $base = 'Skill Books';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "BuiltinBooks";
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

    $app->add_navigation_item({
      title => 'Details of General Skill Books',
      path  => '/Skill Books/',
      order => 20,
    });

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $r      = $app->routes;
    my $routes = $r->any("$base");

    $routes->any('/details')->to(
      cb => sub ($c) {
        $c->redirect_to('/Skill Books');
      }
    );

    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $manager) {
          $logger->logcroak('No Manager Defined');
        }

        foreach my $book (@{ $manager->bookManager->get_all_books() }) {
          my $name = $book->name;
          $app->add_navigation_item({
            title  => "Details for $name",
            path   => "/Skill Books/details/$name",
            parent => "/Skill Books/details/",
            order  => 20,
          });
        }
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

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book::Builtin;
require Game::EvonyTKR::Model::Book::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::SkillBooks {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';

  my $logger;
  # Specify which collection this controller handles
  sub collection_name {
    return 'skill books';
  }

  sub get_manager($self) {
    return $self->app->get_root_manager->bookManager;
  }

  sub controller_name ($self) {
    return "SkillBooks";
  }

  my $base = '/Reference/Skill Books';

 # in part because parent classes use this to override different values of $base
  sub getBase($self) {
    $base =~ s{/$}{};
    return $base;
  }

  # Register this when the application starts
  sub register($self, $app, $config = {}) {
    $logger = $app->get_logger(__PACKAGE__);
    $logger->INFO("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_builtinbook_manager => sub {
        return $self->app->get_root_manager->bookManager;
      }
    );

    $app->add_navigation_item({
      title => 'Details of General Skill Books',
      path  => $self->getBase(),
      order => 30,
    });

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->DEBUG("got controller_name $controller_name.");

    my $mainRoutes = $app->routes->any($base);
    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    # for backwards compatibility
    $mainRoutes->any('/details')->to(
      cb => sub ($c) {
        $c->redirect_to($self->getBase());
      }
    );

    # register routes that cannot exist until after the manager class has
    # done its thing only after initialization
    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->DEBUG(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $manager) {
          $app->log->logcroak('No Manager Defined');
        }
        my $base = getBase($self);
        foreach my $book (@{ $manager->bookManager->get_all_books() }) {
          my $name = $book->name;

          my $clean_name = $name;
          $clean_name =~ s{^/}{};

          $mainRoutes->get($clean_name => { name => $clean_name })
            ->to(controller => $controller_name, action => 'show')
            ->name("${base}_show");

          $app->add_navigation_item({
            title  => "Details for $name",
            path   => "$base/$name",
            parent => $base,
            order  => 30,
          });
        }
      }
    );

    $app->helper(
      get_builtin_book_text => sub ($c, $book_name) {
        $logger->DEBUG("get_builtin_book_text for book '$book_name'");
        my $book = $c->app->get_root_manager->bookManager->getBook($book_name);
        if ($book) {
          return $book->text();
        }
        else {
          $logger->WARN("No book found for '$book_name'");
        }
        return "";
      }
    );
  }

  sub index($self) {
    my $collection = collection_name();
    $logger->DEBUG("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $logger->DEBUG("SkillBooks index method has base $base");

    my $items = $self->get_root_manager()->bookManager->get_all_books();
    $logger->DEBUG(
      sprintf('Items: %s with %s items.', ref($items), scalar(@$items)));
    $self->stash(
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      # Render with markdown
      $self->stash(template => 'skill books/index');

      return $self->render_markdown_file($markdown_path,
        { template => 'skill books/index' });
    }
    else {
      # Render just the items
      return $self->render(template => 'skill books/index');
    }
  }

  sub show ($self) {
    $logger->DEBUG("start of show method");
    my $name;
    $name = $self->param('name');
    $logger->DEBUG("show detects name $name, showing details.");

    my $book = $self->get_root_manager()->bookManager->getBook($name);

    unless ($book) {
      $logger->ERR("skill book '$name' was not found.");
      $self->reply->not_found;
    }
    $logger->DEBUG("retrieved skill book $book");

    $self->stash(
      item     => $book,
      template => 'skill books/details',
      layout   => 'default',
    );
    return $self->render();
  }

}

1;

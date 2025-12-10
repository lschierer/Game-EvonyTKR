use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book;
use namespace::clean;

package Game::EvonyTKR::Controller::Books {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {
    return 'skill books';
  }

  sub get_manager($self) {
    return $self->app->get_root_manager->bookManager;
  }

  sub controller_name ($self) {
    return "Books";
  }

  my $base = '/Reference/Skill Books';

 # in part because parent classes use this to override different values of $base
  sub getBase($self) {
    $base =~ s{/$}{};
    return $base;
  }

  sub get_all_books ($c, $app) {
    state @all_books;
    state $initialized = 0;

    return \@all_books if $initialized;

    # Load builtin books
    my $builtin_list = $c->list_builtin_books();
    $c->log_debug(sprintf('Loading %d builtin books', scalar @$builtin_list));

    foreach my $book_name ($builtin_list->@*) {
      my $book = $c->get_builtin_book($book_name);
      unless ($book && ref($book) && $book->isa('Game::EvonyTKR::Model::Book'))
      {
        $c->log_error(sprintf('Failed to load builtin book: %s', $book_name));
        next;
      }
      push @all_books, $book;
    }

    # For now, we only display builtin books in the index
    # Generic books are used programmatically by the buff summarizer
    # If you want to list generic books, uncomment the following:
    #
    # my $generic_list = $c->list_generic_books();
    # foreach my $full_name ($generic_list->@*) {
    #   # Generic book names are like "Level 4 Ground Attack"
    #   my @parts = split ' ', $full_name;
    #   next if @parts < 3;
    #   my $level = $parts[1];
    #   my $name = join ' ', @parts[2..$#parts];
    #   my $book = $c->get_generic_book($name, $level);
    #   next unless $book;
    #   push @all_books, $book;
    # }

    $initialized = 1;
    return \@all_books;
  }

  # Register this when the application starts
  sub register($c, $app, $config = {}) {
    $c->log_info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    $app->add_navigation_item({
      title => 'Details of General Skill Books',
      path  => $c->getBase(),
      order => 30,
    });

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);

    my $controller_name =
        $c->can('controller_name')
      ? $c->controller_name()
      : $baseClass;

    $c->log_debug("got controller_name $controller_name.");

    my $mainRoutes = $app->routes->any($base);
    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    # for backwards compatibility
    $mainRoutes->any('/details')->to(
      cb => sub ($c) {
        $c->redirect_to($c->getBase());
      }
    );

    $app->helper(
      get_all_books => sub {
        return $c->get_all_books($app);
      }
    );

    $app->helper(
      get_builtin_book_text => sub ($self, $book_name) {
        $c->log_debug("get_builtin_book_text for book '$book_name'");

        my $book = $c->get_builtin_book($book_name);

        if ($book) {
          return $book->text();
        }
        else {
          $c->log_warn("No book found for '$book_name'");
        }
        return "";
      }
    );

    # Build routes synchronously during app startup
    $c->build_routes($app, $mainRoutes, $controller_name);
  }

  sub build_routes ($c, $app, $mainRoutes, $controller_name) {
    my $books = $c->get_all_books($app);

    $c->log_info(sprintf(
      'Building routes for %d skill books', scalar(@$books)));

    foreach my $book (@$books) {
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

      $c->log_debug(sprintf('Added route and nav item for "%s"', $name));
    }
  }

  sub index($self) {
    my $collection = collection_name();
    $self->log_debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $self->log_debug("Books index method has base $base");

    my $items = $self->get_all_books($self->app);
    $self->log_debug(
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

      return $self->render_markdown_page($markdown_path,
        { template => 'skill books/index' });
    }
    else {
      # Render just the items
      return $self->render(template => 'skill books/index');
    }
  }

  sub show ($self) {
    $self->log_debug("start of show method");
    my $name;
    $name = $self->param('name');
    $self->log_debug("show detects name $name, showing details.");

    my $book = $self->get_builtin_book($name);

    unless ($book) {
      $self->log_error("skill book '$name' was not found.");
      $self->reply->not_found;
    }
    $self->log_debug("retrieved skill book $book");

    $self->stash(
      item     => $book,
      template => 'skill books/details',
      layout   => 'default',
    );
    return $self->render();
  }

}

1;

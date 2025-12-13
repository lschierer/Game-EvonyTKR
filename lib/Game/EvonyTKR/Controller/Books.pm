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

  sub controller_name ($self) {
    return "Books";
  }

  my $base = '/Reference/Skill Books';

 # in part because parent classes use this to override different values of $base
  sub getBase($self) {
    $base =~ s{/$}{};
    return $base;
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

    # Add generic route with placeholder for any book name
    $mainRoutes->get('/:book_name')
      ->to(controller => $controller_name, action => 'show')
      ->name("${base}_show");

    # Build navigation items asynchronously during app startup
    $c->build_nav_items($app, $mainRoutes, $controller_name);
  }

  sub build_nav_items ($c, $app, $mainRoutes, $controller_name, $retry_number = 0) {
    $c->log_debug("attempting to build nav items for books, retry # $retry_number") if $retry_number;
    if($c->are_prereqs_outstanding($app->minion, ['load_all_builtin_books', 'load_all_generic_books', ])){
      Mojo::IOLoop->timer(30 => sub{
        $c->build_nav_items($app, $mainRoutes, $controller_name, $retry_number++);
      });
      return;
    }


    my $books = [];
    foreach my $bn ($c->list_builtin_books->@*) {
      my $book = $c->get_builtin_book($bn);
      unless($book){
        $c->log_error(sprintf('failed to retrieve built in book "%s" after prereq check passed', $bn));
        next;
      }
      push @{ $books }, $book;
    }
    foreach my $book (@$books) {
      my $name = $book->name;

      $app->add_navigation_item({
        title  => "Details for the $name Book",
        path   => "$base/$name",
        parent => $base,
        order  => 30,
      });

      $c->log_debug(sprintf('Added nav item for "%s"', $name));
    }

    foreach my $level (1..4){
      foreach my $bn ($c->list_generic_books($level)->@*) {
        my $book = $c->get_generic_book($bn, $level);
        unless($book){
          $c->log_error(sprintf('failed to retrieve generic in book "%s" after prereq check passed', $bn));
          next;
        }
        $app->add_navigation_item({
          title  => sprintf('Details for the Level %s %s Book', $level, $book->name),
          path   => sprintf('%s/Level %s %s', $base, $level, $book->name),
          parent => $base,
          order  => 30,
        });
      }
    }

    $c->log_info(sprintf(
      'Building navigation for %d skill books', scalar(@$books)));


  }

  sub index($c) {
    my $collection = collection_name();
    $c->log_debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, __PACKAGE__);
    my $baseClass = pop(@parts);
    my $base      = $c->getBase();
    $c->log_debug("Books index method has base $base");

    my $items = [];
    foreach my $bn ($c->list_builtin_books->@*) {
      my $book = $c->get_builtin_book($bn);
      unless($book){
        $c->log_error(sprintf('failed to retrieve built in book "%s" after prereq check passed', $bn));
        next;
      }
      push @{ $items }, $book;
    }

    my $generics = [];
    foreach my $level (1..4){
      my $ll = $c->list_generic_books($level);
      $c->log_debug(sprintf('there are %s generic books at level %s',
      scalar(@$ll), $level));
      foreach my $bn (@$ll) {
        my $book = $c->get_generic_book($bn, $level);
        unless($book){
          $c->log_error(sprintf('failed to retrieve generic in book "%s" after prereq check passed', $bn));
          next;
        }
        push @{ $generics }, $book;
      }
    }
    $c->log_debug(
      sprintf('Items: %s with %s items.', ref($items), scalar(@$items)));
    $c->stash(
      linkBase        => $base,
      items           => $items,
      generics        => $generics,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      # Render with markdown
      $c->stash(template => 'skill books/index');

      return $c->render_markdown_page($markdown_path,
        { template => 'skill books/index' });
    }
    else {
      # Render just the items
      return $c->render(template => 'skill books/index');
    }
  }

  sub show ($self) {
    $self->log_debug("start of show method");
    my $name = $self->param('book_name');
    $self->log_debug("show detects name $name, showing details.");

    my $book = $self->get_builtin_book($name);

    unless ($book) {
      $self->log_debug("skill book '$name' was not found, passing through to other routes.");
      return $self->continue;  # Pass through to allow other routes to match
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

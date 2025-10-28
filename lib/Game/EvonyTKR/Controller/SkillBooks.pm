use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Role::Book::Builtin;
require Game::EvonyTKR::Role::Book::SkillBook;
use namespace::clean;

package Game::EvonyTKR::Controller::SkillBooks {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',            -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',            -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books', -role;
  use Carp;

  my $bookCache;
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

  sub getBuiltInBooks ($c, $app) {
    state %builtinBooks;
    my @bblist = $c->list_generic_books($app);
    foreach my $bbname (@bblist) {
      unless (length($bbname)
        && exists $builtinBooks{ lc($c->normalize($bbname)) }) {
        $bookCache = $c->create_book_cache() unless (defined($bookCache));
        my $bb = $c->get_builtin_book($bbname, $bookCache);
        unless (defined($bb)
          && ref($bb)
          && $bb->isa('Game::EvonyTKR::Model::Book')) {
          $c->logger->error('invalid book retrieved for list entry "%s"',
            $bbname);
          next;
        }
        $builtinBooks{ lc($c->normalize($bbname)) } = $bb;
      }
    }
    return \%builtinBooks;
  }

  sub getGenericBooks ($c, $app) {
    state %genericBooks;
    my @gglist = $c->list_generic_books($app);
    foreach my $ggname (@gglist) {
      my @parts = split ' ', $ggname;
      my $level = $parts[1] unless ($#parts < 1);
      my $name  = join ' ', @parts[2 .. $#parts] unless ($#parts < 2);
      if (not defined($level)) {
        $c->logger->error(sprintf(
          'invalid entry found in list of generic books: "%s"',
          $ggname));
        next;
      }
      if (not defined($name)) {
        $c->logger->error(sprintf(
          'invalid entry found in list of generic books: "%s"',
          $ggname));
        next;
      }
      unless (exists $genericBooks{ lc($c->normalize($name)) }->{$level}) {
        $bookCache = $c->create_book_cache() unless (defined($bookCache));
        my $gg = $c->get_generic_book($name, $level, $bookCache);
        unless (defined($gg)
          && ref($gg)
          && $gg->isa('Game::EvonyTKR::Model::Book')) {
          $c->logger->error(
'invalid book retrieved for list entry "%s" split into name "%s" and level %s',
            $ggname, $name, $level);
          next;
        }
        $genericBooks{ lc($c->normalize($name)) }->{$level} = $gg;
      }
    }
    return \%genericBooks;
  }

  # Register this when the application starts
  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . ref($c));
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

    $c->logger->debug("got controller_name $controller_name.");

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
      get_builtin_books => sub {
        return $c->getBuiltInBooks($app);
      }
    );

    $app->helper(
      get_builtin_book_text => sub ($self, $book_name) {
        $c->logger->debug("get_builtin_book_text for book '$book_name'");

        $book_name = $c->SUPER::getConstants->normalize($book_name);
        my $book = $c->getBuiltInBooks($app)->{$book_name};

        if ($book) {
          return $book->text();
        }
        else {
          $c->logger->warn("No book found for '$book_name'");
        }
        return "";
      }
    );

    $app->helper(
      get_generic_books => sub ($self) {
        return $c->getGenericBooks($app);
      }
    );

    $app->plugins->on(
      all_books_loaded => sub {
        $c->logger->debug(sprintf(
          '%s register method all_books_loaded handler', blessed($c),));
        my @allBooks;
        push @allBooks,
          sort { $a->name cmp $b->name } values $c->getBuiltInBooks($app)->%*;
        foreach my $book (@allBooks) {
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
  }

  sub load_books ($c, $app) {
    my $allBB           = $c->getBuiltInBooks($app);
    my $allGB           = $c->getGenericBooks($app);
    my $expectedTotal   = 0;
    my $allFilesStarted = 0;

    # register the listener first to ensure all events are captured.
    $app->plugins->on(
      skillbook_loaded => sub {
        my @sbNames;
        push @sbNames, sort keys $allBB->%*;
        push @sbNames, sort keys $allGB->%*;
        if (scalar(@sbNames) >= $expectedTotal && $allFilesStarted) {
          $c->logger->info(sprintf('All %s books loaded.', $expectedTotal));
          $app->plugins->emit(all_books_loaded => { all_books_loaded => 1 });
        }
        else {
          $c->logger->debug(sprintf(
            '%s of %s books loaded. all files %s started.',
            scalar(@sbNames), $expectedTotal,
            $allFilesStarted ? 'are' : 'are not yet'
          ));
        }
      }
    );

    Mojo::File->new($app->config('distDir'))
      ->child('collections/data/skill books/')
      ->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each(
      sub ($e, $index) {
        $expectedTotal++;
        my $delay = 5 + rand(5.0);
        Mojo::IOLoop->timer(
          $delay => sub {
            $c->import_single_book($app, $allBB, $e, $index);
          }
        );
      }
      );

    Mojo::File->new($app->config('distDir'))
      ->child('collections/data/generic books/')
      ->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each(
      sub ($e, $index) {
        $expectedTotal++;
        my $delay = 5 + rand(5.0);
        Mojo::IOLoop->timer(
          $delay => sub {
            $c->import_single_book($app, $allGB, $e, $index, 0);
          }
        );
      }
      );

# the $allFilesStarted is to ensure that both each blocks have fully processed
# before the if inside this handler can match. As the two list_tree blocks are
# syncronous (the delayed subs happen out of band, when their timers expire),
# each will iterate all files before reaching this line to set $allFilesStarted to
# true and thus ungate the final signal.
    $allFilesStarted = 1;

  }

  sub import_single_book($c, $app, $collection, $sbFile, $index, $builtin = 1) {
    $c->logger->debug("processing $sbFile");

    my $data       = $sbFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    my $sb = Game::EvonyTKR::Model::Book->from_hash($hashObject);

    unless ($sb) {
      $c->logger->error(sprintf(
        'failed to build %s book %s from %s.',
        $builtin ? 'Builtin' : 'Generic',
        $index, $sbFile
      ));
      return;
    }
    $collection->{ $c->SUPER::getConstants->normalize($sb->name) } = $sb;
    $c->logger->debug(sprintf(
      'imported %s book %s as %s, for %s in collection.',
      $builtin ? 'Builtin' : 'Generic', $index,
      $sb->name,                        scalar(keys $collection->%*),
    ));
    $app->plugins->emit(
      skillbook_loaded => {
        skillbook => $sb,
        name      => $c->SUPER::getConstants->normalize($sb->name),
      }
    );
  }

  sub index($self) {
    my $collection = collection_name();
    $self->logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $self->logger->debug("SkillBooks index method has base $base");

    my $items;
    @$items = values $self->getBuiltInBooks($self->app)->%*;
    $self->logger->debug(
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
    $self->logger->debug("start of show method");
    my $name;
    $name = $self->param('name');
    $self->logger->debug("show detects name $name, showing details.");

    my $book = $self->get_root_manager()->bookManager->getBook($name);

    unless ($book) {
      $self->logger->error("skill book '$name' was not found.");
      $self->reply->not_found;
    }
    $self->logger->debug("retrieved skill book $book");

    $self->stash(
      item     => $book,
      template => 'skill books/details',
      layout   => 'default',
    );
    return $self->render();
  }

}

1;

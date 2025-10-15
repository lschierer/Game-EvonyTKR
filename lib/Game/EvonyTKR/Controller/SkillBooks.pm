use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book::Builtin;
require Game::EvonyTKR::Model::Book::SkillBook;
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

  sub getBuiltInBoooks {
    state %builtinBooks;
    return \%builtinBooks;
  }

  sub getGenericBooks {
    state %genericBooks;
    return \%genericBooks;
  }

  # Register this when the application starts
  sub register($c, $app, $config = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    $c->load_books($app);

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

    $logger->debug("got controller_name $controller_name.");

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
        return $c->getBuiltInBoooks();
      }
    );

    $app->helper(
      get_builtin_book_text => sub ($self, $book_name) {
        $logger->debug("get_builtin_book_text for book '$book_name'");

        $book_name = $c->SUPER::getConstants->normalize($book_name);
        my $book = $c->getBuiltInBoooks()->{$book_name};

        if ($book) {
          return $book->text();
        }
        else {
          $logger->warn("No book found for '$book_name'");
        }
        return "";
      }
    );

    $app->helper(
      get_generic_books => sub ($self) {
        return $c->getGenericBooks();
      }
    );

    $app->plugins->on(
      all_books_loaded => sub {
        $logger->debug(sprintf(
          '%s register method all_books_loaded handler', blessed($c),));
        my @allBooks;
        push @allBooks,
          sort { $a->name cmp $b->name } values $c->getBuiltInBoooks()->%*;
        push @allBooks,
          sort { $a->name cmp $b->name } values $c->getGenericBooks()->%*;
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
    my $allBB           = $c->getBuiltInBoooks();
    my $allGB           = $c->getGenericBooks();
    my $expectedTotal   = 0;
    my $allFilesStarted = 0;

    # register the listener first to ensure all events are captured.
    $app->plugins->on(
      skillbook_loaded => sub {
        my @sbNames;
        push @sbNames, sort keys $allBB->%*;
        push @sbNames, sort keys $allGB->%*;
        if (scalar(@sbNames) >= $expectedTotal && $allFilesStarted) {
          $logger->info(sprintf('All %s books loaded.', $expectedTotal));
          $app->plugins->emit(all_books_loaded => { all_books_loaded => 1 });
        }
        else {
          $logger->debug(sprintf(
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
    $logger->debug("processing $sbFile");

    my $data       = $sbFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    my $sb;
    if ($builtin) {
      $sb = Game::EvonyTKR::Model::Book::Builtin->from_hash($hashObject);
    }
    else {
      $sb = Game::EvonyTKR::Model::Book::SkillBook->from_hash($hashObject);
    }
    unless ($sb) {
      $logger->error(sprintf(
        'failed to build %s book %s from %s.',
        $builtin ? 'Builtin' : 'Generic',
        $index, $sbFile
      ));
      return;
    }
    $collection->{ $c->SUPER::getConstants->normalize($sb->name) } = $sb;
    $logger->debug(sprintf(
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
    $logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $logger->debug("SkillBooks index method has base $base");

    my $items = $self->get_root_manager()->bookManager->get_all_books();
    $logger->debug(
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
    $logger->debug("start of show method");
    my $name;
    $name = $self->param('name');
    $logger->debug("show detects name $name, showing details.");

    my $book = $self->get_root_manager()->bookManager->getBook($name);

    unless ($book) {
      $logger->error("skill book '$name' was not found.");
      $self->reply->not_found;
    }
    $logger->debug("retrieved skill book $book");

    $self->stash(
      item     => $book,
      template => 'skill books/details',
      layout   => 'default',
    );
    return $self->render();
  }

}

1;

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book;
use namespace::clean;

package Game::EvonyTKR::Controller::Books {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books', -role;
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

  sub getBuiltInBooks ($c, $app) {
    state %builtinBooks;
    my $bblist = $c->list_builtin_books();
    $c->logger->debug(sprintf(
      'got a list of %s builtin books: %s',
      scalar @$bblist,
      Data::Printer::np(@$bblist)
    ));
    foreach my $bbname ($bblist->@*) {
      unless (length($bbname)
        && exists $builtinBooks{ lc($c->normalize($bbname)) }) {
        my $bb = $c->get_builtin_book($bbname);
        unless (defined($bb)
          && ref($bb)
          && $bb->isa('Game::EvonyTKR::Model::Book')) {
          $c->logger->error(sprintf(
            'invalid book retrieved for list entry "%s" : ref %s; blessed %s',
            $bbname,
            ref($bb) // 'scalar variable',
            blessed($bb) // 'not blessed'
          ));
          next;
        }
        $builtinBooks{ lc($c->normalize($bbname)) } = $bb;
      }
    }
    return \%builtinBooks;
  }

  sub getGenericBooks ($c, $app) {
    state %genericBooks;
    my @gglist = $c->list_generic_books();
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
        my $gg = $c->get_generic_book($name, $level);
        unless (defined($gg)
          && ref($gg)
          && $gg->isa('Game::EvonyTKR::Model::Book')) {
          $c->logger->error('invalid book retrieved for list entry "%s" '
              . 'split into name "%s" and level %s',
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

    $c->book_route_builder($app, $mainRoutes, $controller_name);
  }

  sub check_book_loading_readiness ($c, $app) {
    state $jid_builtin;
    state $jid_generic;
    state $retries = 0;
    my $max_retries = 10;
    my $delayTime   = 0.01;

    # Find spawner jobs (same logic as route builder)
    if (not defined $jid_generic) {
      $app->minion->jobs({ tasks => ['load_all_generic_books'] })->each(sub {
        my $info = $_;
        if ($info->{state} eq 'finished') {
          $jid_generic = $info->{id};

        }
        elsif ($info->{state} eq 'inactive' || $info->{state} eq 'active') {
          $jid_generic = $info->{id} unless (defined($jid_generic));
          $retries     = 0;
        }
      });
      if ($retries >= $max_retries) {
        $c->logger->error('cannot find a generic book loader!');
      }
      else {
        $retries++;
        $c->logger->debug('cannot find a generic book loader!');
        Mojo::IOLoop->timer(
          $delayTime => sub {
            return $c->check_book_loading_readiness();
          }
        );
      }
    }
    unless (defined($jid_builtin)) {
      $app->minion->jobs({ tasks => ['load_all_builtin_books'] })->each(sub {
        my $info = $_;
        if ($info->{state} eq 'finished') {
          $jid_builtin = $info->{id};
          $retries     = 0;
        }
      });
      if ($retries >= $max_retries) {
        $c->logger->error('cannot find a builtin book loader!');
      }
      else {
        $retries++;
        $c->logger->debug('cannot find a builtin book loader!');
        Mojo::IOLoop->timer(
          $delayTime => sub {
            return $c->check_book_loading_readiness();
          }
        );
      }
    }

    # Check if spawners are finished
    my $builtin_job = $c->minion->job($jid_builtin);
    my $generic_job = $c->minion->job($jid_generic);

    return 0
      unless $builtin_job->info->{state} eq 'finished'
      && $generic_job->info->{state} eq 'finished';

    # Count spawned load_book jobs
    my $spawned_jobs = $c->minion->jobs({ task => 'load_book' })->total;

    # Count expected books
    my $expected =
      @{ $c->list_builtin_books() } + @{ $c->list_generic_books() };

    return $spawned_jobs == $expected;
  }

  sub book_route_builder ($c, $app, $mainRoutes, $controller_name) {
    my $delayTime = 5;
    state ($jid_generic, $jid_builtin);
    state $retries = 0;
    my $max_retries = 10;
    if (not defined $jid_generic) {
      $app->minion->jobs({ tasks => ['load_all_generic_books'] })->each(sub {
        my $info = $_;
        if ($info->{state} eq 'finished') {
          $jid_generic = $info->{id};

        }
        elsif ($info->{state} eq 'inactive' || $info->{state} eq 'active') {
          $jid_generic = $info->{id} unless (defined($jid_generic));
          $retries     = 0;
        }
      });
      if ($retries >= $max_retries) {
        $c->logger->error('cannot find a generic book loader!');
      }
      else {
        $retries++;
        $c->logger->debug('cannot find a generic book loader!');
        Mojo::IOLoop->timer(
          $delayTime => sub {
            return $c->book_route_builder($app, $mainRoutes, $controller_name);
          }
        );
      }
    }

    unless (defined($jid_builtin)) {
      $app->minion->jobs({ tasks => ['load_all_builtin_books'] })->each(sub {
        my $info = $_;
        if ($info->{state} eq 'finished') {
          $jid_builtin = $info->{id};
          $retries     = 0;
        }
      });
      if ($retries >= $max_retries) {
        $c->logger->error('cannot find a builtin book loader!');
      }
      else {
        $retries++;
        $c->logger->debug('cannot find a builtin book loader!');
        Mojo::IOLoop->timer(
          $delayTime => sub {
            return $c->book_route_builder($app, $mainRoutes, $controller_name);
          }
        );
      }
    }

    my $expectedCount = scalar(@{ $c->list_builtin_books });
    my @allBooks;
    push @allBooks,
      sort { lc($a->name) cmp lc($b->name) }
      values $c->getBuiltInBooks($app)->%*;

    $c->logger->info(sprintf(
      '%s book_route_builder expected %s, found %s',
      __PACKAGE__, $expectedCount, scalar(@allBooks)
    ));

    foreach my $book (@allBooks) {
      $c->logger->debug(sprintf('building routes for "%s"', $book->name));
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

    # the loading jobs take longer than I expect it to take to
    # *start* the jobs to spawn loaders. give it more time by
    # allowing more retries. Do not delay longer so that
    # I get incremental progress.
    if ($expectedCount > scalar(@allBooks) && $retries <= ($max_retries * 10)) {
      $c->logger->debug(sprintf(
        'on retry %s, expected %s, found %s',
        $retries, $expectedCount, scalar(@allBooks)
      ));
      $retries++;
      Mojo::IOLoop->timer(
        $delayTime / 2 => sub {
          return $c->book_route_builder($app, $mainRoutes, $controller_name);
        }
      );
    }
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
    $self->logger->debug("Books index method has base $base");

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

      return $self->render_markdown_page($markdown_path,
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

    my $book = $self->get_builtin_book($name);

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

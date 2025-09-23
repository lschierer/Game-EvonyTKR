use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Game::EvonyTKR::Model::Book::BuiltinBook;
require Game::EvonyTKR::Model::Book::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::Covenants {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';

  # Specify which collection this controller handles
  sub collection_name {
    return 'covenants';
  }

  sub get_manager($self) {
    return $self->app->get_root_manager->bookManager;
  }

  sub controller_name ($self) {
    return "Covenants";
  }

  my $base = '/Reference/Covenants';

 # in part because parent classes use this to override different values of $base
  sub getBase($self) {
    $base =~ s{/$}{};
    return $base;
  }

  # Register this when the application starts
  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_covenant_manager => sub {
        return $self->app->get_root_manager->covenantManager;
      }
    );

    $app->helper(
      covenant_category_names => sub ($c, $printable = 0) {
        my @names;
        foreach my $n ($app->get_root_manager->CovenantCategoryValues->@*) {
          if ($n eq 'none') {next}
          if ($printable) {
            push @names, ucfirst($n);
          }
          else {
            push @names, $n;
          }
        }
        return \@names;
      }
    );

    $app->add_navigation_item({
      title => 'Details of General Covenants',
      path  => $self->getBase(),
      order => 50,
    });

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

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
      'generals_loaded' => sub($c, $manager) {
        $logger->debug(
          "generals_loaded sub has controller_name $controller_name.");

        if (not defined $manager) {
          $logger->logcroak('No Manager Defined');
        }

        my $cd = Mojo::File->new($app->config('distDir'))->child('collections/data/covenants/');
        my @files = $cd->list_tree->each;

        $logger->info("Starting async import of " . scalar(@files) . " covenant files");

        # Import covenants async
        $self->_import_covenants_async($app, \@files, $manager, $controller_name, $mainRoutes, sub {
          $logger->info("All covenants loaded, emitting signal");
          $app->plugins->emit(covenants_loaded => $manager);
        });
      }
    );
    $logger->debug("end of register method");
  }

  sub _import_covenants_async($self, $app, $files, $manager, $controller_name, $mainRoutes, $callback) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    my @files_copy = @$files;
    my $total = @files_copy;
    my $processed = 0;

    my $process_next_batch;
    $process_next_batch = sub {
      my $batch_size = 8; # Process 8 files per tick (covenants are simpler)
      my $batch_count = 0;

      while (@files_copy && $batch_count < $batch_size) {
        my $covenantFile = shift @files_copy;
        $batch_count++;
        $processed++;

        eval {
          $logger->info("importing covenant file $covenantFile ($processed/$total)");
          my $data = $covenantFile->slurp('UTF-8');
          my $name = $covenantFile->basename('.yaml');
          my $object = YAML::PP->new(
            schema       => [qw/ + Perl /],
            yaml_version => ['1.2', '1.1'],
          )->load_string($data);

          $logger->trace(
            "$object imported, looks like " . Data::Printer::np($object));

          if (exists $object->{name}) {
            if ($object->{name} !~ /$name/i) {
              $logger->error(sprintf('filename and internal name do not match for file "%s" with name "%s"',
                $covenantFile, $object->{name}));
            }
            $name = $object->{name};
          }

          my $covenant = Game::EvonyTKR::Model::Covenant->from_hash($object, $manager, $logger);
          $manager->covenantManager->add_covenant($covenant);

          # Build routes for this covenant
          $self->_build_covenant_routes($covenant, $name, $app, $controller_name, $mainRoutes);
        };
        if ($@) {
          $logger->error("Error processing $covenantFile: $@");
        }
      }

      if (@files_copy) {
        # Schedule next batch
        Mojo::IOLoop->next_tick($process_next_batch);
      } else {
        # All done
        $callback->();
      }
    };

    # Start processing
    $process_next_batch->();
  }

  sub _build_covenant_routes($self, $covenant, $name, $app, $controller_name, $mainRoutes) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    $logger->debug("building route for " . $covenant->primary->name);

    my $clean_name = $name;
    $clean_name =~ s{^/}{};

    $mainRoutes->get($clean_name => { name => $clean_name })
      ->to(controller => $controller_name, action => 'show')
      ->name("${base}_show");

    $app->add_navigation_item({
      title  => sprintf('Details for %s\'s Covenant', $name),
      path   => "$base/$name",
      parent => $base,
      order  => 50,
    });
  }

  sub index($self) {
    my $logger     = Log::Log4perl->get_logger(__PACKAGE__);
    my $collection = collection_name();
    $logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir =
      Path::Tiny::path(Mojo::File::Share::dist_dir('Game::EvonyTKR'));
    my $markdown_path = $distDir->child("pages/Covenants/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $logger->debug("Covenants index method has base $base");

    my @items = $self->get_root_manager()->covenantManager->get_all_covenants();
    $logger->debug(sprintf('Items: %s items.', scalar(@items)));
    $self->stash(
      linkBase        => $base,
      items           => \@items,
      collection_name => $collection,
      controller_name => $baseClass,
      template        => 'covenants/index',
    );

    if (-f $markdown_path) {
      # Render with markdown
      return $self->render_markdown_file($markdown_path,
        { template => 'covenants/index' });
    }
    else {
      $logger->debug("no markdown index content found at $markdown_path");
      # Render just the items
      return $self->render(template => 'covenants/index');
    }
  }

  sub show ($self) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    $logger->debug("start of show method");
    my $name;
    $name = $self->param('name');
    $logger->debug("show detects name $name, showing details.");

    my $covenant =
      $self->get_root_manager()->covenantManager->getCovenant($name);

    unless ($covenant) {
      $logger->error("covenant for '$name' was not found.");
      $self->reply->not_found;
    }
    $logger->debug("retrieved covenant $covenant");

    $self->stash(
      item     => $covenant,
      template => 'covenants/details',
      layout   => 'default',
    );
    return $self->render();
  }

}

1;


# Add this helper method to the Covenants controller:

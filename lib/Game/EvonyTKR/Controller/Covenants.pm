use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Game::EvonyTKR::Model::Covenant;
use namespace::clean;

package Game::EvonyTKR::Controller::Covenants {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';

  my $logger;

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

  sub get_all_covenants {
    state %convenants;
    return \%convenants;
  }

  # Register this when the application starts
  sub register($c, $app, $config = {}) {
    $logger = $app->log;
    $logger->info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    $app->helper(
      get_all_covenants => sub {
        return $c->get_all_covenants();
      }
    );
    $app->helper(
      getCovenant => sub($self, $general) {
        my $name;
        if (blessed($general)
          && $general->isa('Game::EvonyTKR::Model::General')) {
          $name = $general->name;
        }
        else {
          $name = $general;
        }
        return $c->get_all_covenants()
          ->{ $c->SUPER::getConstants->normalize($name) };
      }
    );

    $app->helper(
      covenant_category_names => sub ($c, $printable = 0) {
        my @names;
        foreach my $n ($c->SUPER::getConstants()->CovenantCategoryValues->@*) {
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
      path  => $c->getBase(),
      order => 50,
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

    $app->plugins->on(
      all_covenants_imported => sub {
        foreach my $covenant (sort { $a->primary->name cmp $b->primary->name }
          values $c->get_all_covenants()->%*) {
          $c->_build_covenant_routes($covenant, $covenant->primary->name,
            $app, $controller_name, $mainRoutes);
        }
      }
    );

    # register routes that cannot exist until after the manager class has
    # done its thing only after initialization
    $app->plugins->on(
      'generals_loaded' => sub {
        $c->load_covenants($app);
      }
    );

    $logger->debug("end of register method");
  }

  sub import_single_covenant ($c, $app, $fileName, $index) {
    # some filenames have UTF-8 characters.
    # these import oddly unless handled carefully.
    my $covenantFile =
      Mojo::File->new(Encode::decode_utf8($fileName->to_string));
    $logger->debug("importing covenant file $covenantFile ");
    my $data       = $covenantFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    unless (exists $hashObject->{name} && length($hashObject->{name})) {
      $logger->error(sprintf(
        'Name is required for a Covenant.  ' . 'Cannot Import %s',
        $covenantFile
      ));
      return;
    }
    my $primary = $app->get_general($hashObject->{name});
    unless ($primary) {
      $logger->error("cannot find primary for covenant $hashObject->{name}");
      return;
    }
    my $covenant =
      Game::EvonyTKR::Model::Covenant->from_hash($hashObject, $primary);
    unless ($covenant) {
      $logger->error(sprintf('failed to build covenant from %s.', $covenantFile));
      return;
    }
    my $allc = $app->get_all_covenants();
    $allc->{ $c->SUPER::getConstants()->normalize($covenant->primary->name) } =
      $covenant;
    $app->plugins->emit(covenant_imported => { covenant => $covenant });
  }

  sub load_covenants ($c, $app) {
    my $expectedTotal = 0;

    $app->plugins->on(
      covenant_imported => sub {
        my $covenant = $_->{covenant};

        my $allc  = $app->get_all_covenants();
        my $count = scalar(keys $allc->%*);
        if ($count >= $expectedTotal) {
          $logger->info(sprintf('Finished importing %s covenants', $count));
          $app->plugins->emit(
            all_covenants_imported => { covenants_imported => $count });
        }
      }
    );

    Mojo::File->new($app->config('distDir'))
      ->child('collections/data/covenants/')
      ->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each(
      sub ($e, $index) {
        my $delay = rand(4.0);
        Mojo::IOLoop->timer(
          $delay => sub {
            $expectedTotal++;
            $c->import_single_covenant($app, $e, $index);
          }
        );
      }
      );
    $logger->info(
      sprintf('Async import of %s covenant files started', $expectedTotal));
  }

  sub _build_covenant_routes($c, $covenant, $name, $app, $controller_name,
    $mainRoutes) {

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

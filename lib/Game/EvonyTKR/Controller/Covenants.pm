use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
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
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $manager) {
          $logger->logcroak('No Manager Defined');
        }
        my $base = getBase($self);
        foreach my $covenant ($manager->covenantManager->get_all_covenants()) {
          if (blessed($covenant) ne 'Game::EvonyTKR::Model::Covenant') {
            $logger->error(
              sprintf(
              'got a blessed: "%s" ref: "%s" instead of a  Game::EvonyTKR::Model::Covenant'
              ),
              blessed($covenant),
              Scalar::Util::reftype($covenant)
            );
            next;
          }
          elsif (!defined $covenant->primary) {
            $logger->error("Covenant with undefined primary!! "
                . Data::Printer::np($covenant));
            next;
          }
          else {
            $logger->debug("building route for " . $covenant->primary->name);
          }
          my $name = $covenant->primary->name;
          if (!defined $name) {
            $logger->error("primary name is undefined in covenant: "
                . ref $covenant);
            next;
          }

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
      }
    );
    $logger->debug("end of register method");
  }

  sub index($self) {
    my $logger     = Log::Log4perl->get_logger(__PACKAGE__);
    my $collection = collection_name();
    $logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Path::Tiny::path(Mojo::File::Share::dist_dir('Game::EvonyTKR'));
    my $markdown_path = $distDir->child("pages/Covenants/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $logger->debug("Covenants index method has base $base");

    my @items = $self->get_root_manager()->covenantManager->get_all_covenants();
    $logger->debug(
      sprintf('Items: %s items.', scalar(@items)));
    $self->stash(
      linkBase        => $base,
      items           => \@items,
      collection_name => $collection,
      controller_name => $baseClass,
      template => 'covenants/index',
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

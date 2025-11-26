use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Specialty;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Specialties {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Specialties', -role;
  use List::AllUtils qw( all any none first);
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'Specialties'}

  sub get_manager($self) {
    return $self->app->get_root_manager->specialtyManager;
  }

  my $base = '/Reference/Specialties';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Specialties";
  }

  sub get_all_specialties ($c, $app) {
    state %specialties;    # normalized_name -> object
    state $sig;            # signature of expected set we’ve fully hydrated

    return $c->_hydrate_from_list(
      $app,
      sub ($app2) { $c->list_specialties() },      # list provider
      sub ($name) { $c->get_specialty($name) },    # fetch one
      \%specialties,
      \$sig,
    );
  }

  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . __PACKAGE__);
    $c->SUPER::register($app, $config);

    unless (defined($app)) {
      $c->logger->logcroak('$app is not defined in ' . __PACKAGE__);
    }

    $app->add_navigation_item({
      title  => 'Details of General Specialties',
      path   => $base,
      parent => '/Reference',
      order  => 40,
    });

    my @parts     = split(/::/, __PACKAGE__);
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

    Mojo::IOLoop->timer(
      60 => sub {
        $c->build_routes($app, $mainRoutes, $controller_name);
      }
    );

    $app->helper(
      get_all_specialties => sub {
        return values $c->get_all_specialties($app)->%*;
      }
    );

    $app->helper(
      specialty_level_names => sub ($self, $level = '', $printable = 0) {
        $level //= '';    # Ensure defined
        if (length($level) == 0) {
          my $nameList = [];
          foreach
            my $orig_name ($c->SUPER::getConstants->SpecialtyLevelValues->@*) {
            $c->logger->debug(
              "specialty_level_names evaluating specialty level name $orig_name"
            );
            my $name;
            if ($printable) {
              $name = $orig_name =~ s/(\w)(\w*)/\U$1\L$2/r;
            }
            else {
              $name = $orig_name;
            }
            push @$nameList, $name;
          }
          return $nameList;
        }
        else {
          $c->logger->debug(
            "specialty_level_names sees levels"
              . Data::Printer::np(
              $c->SUPER::getConstants->SpecialtyLevelValues->@*
              )
          );
          my $match = first { $level =~ /$_/i }
            $c->SUPER::getConstants->SpecialtyLevelValues->@*;
          $match =~ s/(\w)(\w*)/\U$1\L$2/;
          return $match;
        }
      }
    );

  }

  sub build_routes ($c, $app, $mainRoutes, $controller_name) {
    my $specialties = $c->get_all_specialties($app);
    foreach my $specialty (values $specialties->%*) {
      my $name = $specialty->name;

      my $clean_name = $name;
      $clean_name =~ s{^/}{};

      $mainRoutes->get($clean_name => { name => $clean_name })
        ->to(controller => $controller_name, action => 'show')
        ->name("${base}_show");

      $app->add_navigation_item({
        title  => "Details for $name",
        path   => "$base/$name",
        parent => "$base",
        order  => 40,
      });

      $c->logger->debug(
        sprintf('added route and nav item for name "%s" ', $name)
          . sprintf(
          'cleaned to "%s" with path "%s/%s"',
          $clean_name, $base, $name
          )
      );
    }
  }

  sub sort_levels($self, $levels) {
    # Define the order of levels (if they don't sort alphabetically)
    my %level_order = (
      'Green'  => 1,
      'Blue'   => 2,
      'Purple' => 3,
      'Orange' => 4,
      'Gold'   => 5,
    );

    # Return sorted array
    return [
      sort {
        # Use the defined order if available,
        # otherwise fall back to string comparison
        ($level_order{ $a->{level} } // 999)
          <=> ($level_order{ $b->{level} } // 999)
          || $a->{level} cmp $b->{level}
      } @$levels
    ];
  }

  sub index($c) {
    my $collection = collection_name();
    $c->logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);
    my $base      = $c->getBase();
    $c->logger->debug("Specialties index method has base $base");

    my $items;
    @{$items} = values $c->get_all_specialties($c->app)->%*;
    $c->logger->debug(
      sprintf('Items: %s with %s items.', ref($items), scalar(@$items)));
    $c->stash(
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      # Render with markdown
      $c->stash(template => 'specialties/index');

      return $c->render_markdown_page($markdown_path,
        { template => 'specialties/index' });
    }
    else {
      # Render just the items
      return $c->render(template => 'specialties/index');
    }
  }

  sub show ($c) {
    $c->logger->debug("start of show method");
    my $name = $c->param('name');
    $c->logger->debug("show detects name $name, showing details.");

    my $specialty = $c->get_specialty($name);

    unless ($specialty) {
      $c->logger->error("specialty '$name' was not found.");
      return $c->reply->not_found;
    }
    $c->logger->debug("retrieved specialty $specialty");

    $c->stash(
      item     => $specialty,
      template => 'specialties/details',
      layout   => 'default',
    );
    return $c->render();
  }

}

1;

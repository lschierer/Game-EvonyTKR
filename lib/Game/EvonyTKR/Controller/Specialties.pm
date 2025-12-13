use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Specialty;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Specialties {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
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

  sub register($c, $app, $config = {}) {
    $c->log_info("Registering routes for " . __PACKAGE__);
    $c->SUPER::register($app, $config);

    unless (defined($app)) {
      $c->log_logcroak('$app is not defined in ' . __PACKAGE__);
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

    $c->log_debug("got controller_name $controller_name.");

    my $mainRoutes = $app->routes->any($base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $mainRoutes->get('/:specialty_name')
      ->to(controller => $controller_name, action => 'show')
      ->name("${base}_show");

    # Build routes synchronously during app startup
    $c->build_nav_items($app, $mainRoutes, $controller_name);

    $app->helper(
      specialty_level_names => sub ($self, $level = '', $printable = 0) {
        $level //= '';    # Ensure defined
        if (length($level) == 0) {
          my $nameList = [];
          foreach
            my $orig_name ($c->SUPER::getConstants->SpecialtyLevelValues->@*) {
            $c->log_debug(
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
          $c->log_debug(
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

  sub build_nav_items ($c, $app, $mainRoutes, $controller_name) {

    if($c->are_prereqs_outstanding($app->minion, ['load_all_specialties', ])){
      Mojo::IOLoop->timer(30 => sub{
        $c->build_nav_items($app, $mainRoutes, $controller_name);
      });
    }

    my $specialties = [];
    foreach my $sn ($c->list_specialties->@*) {
      my $specialty = $c->get_specialty($sn);
      unless ($specialty) {
        $c->log_error(sprintf('failed to get listed specialty "%s"', $sn));
        next;
      }
      push @{$specialties}, $specialty;
    }
    foreach my $specialty (@$specialties) {
      my $name = $specialty->name;

      $app->add_navigation_item({
        title  => "Details for the $name Specialty",
        path   => "$base/$name",
        parent => "$base",
        order  => 40,
      });

      $c->log_debug(
        sprintf('added nav item for name "%s" with path "%s/%s"',
          $name, $base, $name)
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
    $c->log_debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);
    my $base      = $c->getBase();
    $c->log_debug("Specialties index method has base $base");

    my $items;
    my $specialties = [];
    foreach my $sn ($c->list_specialties->@*) {
      my $specialty = $c->get_specialty($sn);
      unless ($specialty) {
        $c->log_error(sprintf('failed to get listed specialty "%s"', $sn));
        next;
      }
      push @{$items}, $specialty;
    }
    $c->log_debug(
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
    $c->log_debug("start of show method");
    my $name = $c->param('specialty_name');
    $c->log_debug("show detects name $name, showing details.");

    my $specialty = $c->get_specialty($name);

    unless ($specialty) {
      $c->log_debug("specialty '$name' was not found, passing through to other routes.");
      return $c->continue;  # Pass through to allow other routes to match
    }
    $c->log_debug("retrieved specialty $specialty");

    $c->stash(
      item     => $specialty,
      template => 'specialties/details',
      layout   => 'default',
    );
    return $c->render();
  }

}

1;

use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Speciality::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::Specialities {
  use Mojo::Base 'Game::EvonyTKR::Controller::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'specialities'}

  sub get_manager($self) {
    return $self->app->get_root_manager->specialityManager;
  }

  my $base = '/Specialities/';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Specialities";
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->add_navigation_item({
      title  => 'Details of General Specialities',
      path   => '/Specialities/',
      order  => 20,
    });

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $r      = $app->routes;
    my $routes = $r->any("$base");

    $routes->any('/details')
      ->to(cb => sub ($c) {
        $c->redirect_to('/Specialities')
      });

      $app->plugins->on(
        'evonytkrtips_initialized' => sub($self, $manager) {
          $logger->debug(
            "evonytkrtips_initialized sub has controller_name $controller_name.");

            if (not defined $manager) {
              $logger->logcroak('No Manager Defined');
            }

          foreach my $speciality (@{ $manager->specialityManager->get_all_specialities() }) {
            my $name = $speciality->name;
            $app->add_navigation_item({
              title   => "Details for $name",
              path    => "/Specialities/details/$name",
              parent  => "/Specialities/details/",
              order => 20,
            });
          }
        }
      );

      my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
      my $collection = $self->collection_name;
      my $SourceDir  = $distDir->child("collections/$collection");

      $logger->info(
        "Successfully loaded Speciality manager with collection from $SourceDir"
      );


    $app->helper(
      get_builtinbook_manager => sub {
        my $self = shift;
        return $self->app->get_root_manager->specialityManager;
      }
    );
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
  # Use the defined order if available, otherwise fall back to string comparison
        ($level_order{ $a->{level} } // 999)
          <=> ($level_order{ $b->{level} } // 999)
          || $a->{level} cmp $b->{level}
      } @$levels
    ];
  }

}

1;

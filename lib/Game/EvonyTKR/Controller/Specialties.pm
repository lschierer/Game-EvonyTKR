use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Specialty::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::Specialties {
  use Mojo::Base 'Game::EvonyTKR::Controller::CollectionBase';
  use List::AllUtils qw( all any none first);

  # Specify which collection this controller handles
  sub collection_name {'Specialties'}

  sub get_manager($self) {
    return $self->app->get_root_manager->specialtyManager;
  }

  my $base = '/Reference/Specialties/';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Specialties";
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->add_navigation_item({
      title   => 'Details of General Specialties',
      path    => $base,
      parent  => '/Reference',
      order   => 20,
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

    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $manager) {
          $logger->logcroak('No Manager Defined');
        }

        foreach my $specialty (
          @{ $manager->specialtyManager->get_all_specialties() }) {
          my $name = $specialty->name;

          my $clean_name = $name;
          $clean_name =~ s{^/}{};
          $routes->get($clean_name => {name => $clean_name })
            ->to(controller => $controller_name, action => 'show')
            ->name("${base}_show");

          $app->add_navigation_item({
            title  => "Details for $name",
            path   => "$base/$name",
            parent => "$base/",
            order  => 20,
          });
        }
      }
    );

    my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $collection = $self->collection_name;
    my $SourceDir  = $distDir->child("collections/$collection");

    $logger->info(
      "Successfully loaded Specialty manager with collection from $SourceDir");

    $app->helper(
      get_specialty_manager => sub {
        my $self = shift;
        return $self->app->get_root_manager->specialtyManager;
      }
    );

    $app->helper(
      specialty_level_names => sub ($c, $level = '', $printable = 0) {
        $level //= '';    # Ensure defined
        my $logger = Log::Log4perl->get_logger(__PACKAGE__);
        if (length($level) == 0) {
          my $nameList = [];
          foreach
            my $orig_name ($c->app->get_root_manager->specialtyLevels->@*) {
            $logger->debug(
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
          $logger->debug(
            "specialty_level_names sees levels"
              . Data::Printer::np(
              $c->app->get_root_manager->specialtyLevels->@*
              )
          );
          my $match = first { $level =~ /$_/i }
            $c->app->get_root_manager->specialtyLevels->@*;
          $match =~ s/(\w)(\w*)/\U$1\L$2/;
          return $match;
        }
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

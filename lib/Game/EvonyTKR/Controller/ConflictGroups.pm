use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use namespace::autoclean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use List::AllUtils qw( all any none );
  use Carp;

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';

  sub getBase($self) {
    return $base;
  }

  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . __PACKAGE__);
    $c->SUPER::register($app, $config);

    my $routes          = $app->routes->any($base);
    my $controller_name = $c->controller_name();

    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->add_navigation_item({
      title  => 'General Conflict Groups',
      path   => $base,
      parent => '/Reference/',
      order  => 60,
    });

  }

  has prereqs => sub {
    return [qw(
      load_all_generals
      load_all_builtin_books
      load_all_generic_books
      load_all_covenants
      load_all_specialties
      load_all_ascending_attributes
      load_ml_conflicts
    )];
  };

  sub index ($c) {
    return if $c->check_prereqs_or_wait($c->prereqs);
    $c->logger->debug("Rendering conflict groups index");

    my $detector = $c->get_conflict_detector();

    $c->logger->debug(sprintf('there are %s generals in the by_general index',
      scalar keys $detector->by_general->%*));
    my $groups = $detector->groups_by_conflict_type;
    my $pairs  = $detector->by_general;
    $c->logger->debug('conflict groups controller index handler sees '
        . Data::Printer::np($pairs));

    $c->stash(
      groups   => $groups,
      pairs    => $pairs,
      linkBase => $base,
    );

    return $c->render(template => '/general conflict groups/index');
  }
}
1;
__END__

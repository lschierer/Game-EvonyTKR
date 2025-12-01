use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Service::Cache;
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

    Mojo::IOLoop->timer(
      0.01 => sub {
        $c->logger->debug(
          __PACKAGE__ . ' calling schedule_cached_conflicts_merge');
        $c->schedule_cached_conflicts_merge($app);
      }
    );
  }

  sub schedule_cached_conflicts_merge($c, $app, $delay = 0) {
    my $is_complete = $c->conflict_cache->get('conflict_building_complete');

    $delay++;
    $delay = $delay % 60;
    $delay = $delay ? $delay : 0.01;

    if ($is_complete) {
      $c->logger->info('Conflict building complete, loading final data');
      # calling get_conflict_detector will trigger a refresh
      Mojo::IOLoop->timer(
        0.001 => sub {
          $c->get_conflict_detector();
        }
      );
      return;
    }

    # Not complete yet, retry in 30 seconds
    $c->logger->debug(
      sprintf('Conflict building not complete yet, will retry in %s seconds',
        $delay)
    );
    Mojo::IOLoop->timer(
      $delay => sub { $c->schedule_cached_conflicts_merge($app, $delay) });
  }

  sub index ($c) {
    $c->logger->debug("Rendering conflict groups index");

    my $detector = $c->get_conflict_detector();
    $c->logger->debug(sprintf('there are %s generals in the by_general index',
      scalar keys $detector->by_general->%*));
    my $groups = $detector->groups_by_conflict_type;
    my $pairs  = $detector->by_general;

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

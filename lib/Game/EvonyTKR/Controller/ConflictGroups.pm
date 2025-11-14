use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Service::Cache;
use namespace::autoclean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',            -role;
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

  sub do_merge_cached_conflicts ($c, $app, $cd, $cc, $merged) {
    $c->logger->debug(sprintf(
      'do_merge_cached_conflicts called with merged: %s',
      Data::Printer::np($merged)));
    if (my $by_general = $merged->{by_general}) {
      while (my ($general, $conflicts) = each %$by_general) {
        $cd->by_general->{$general} =
          { %{ $cd->by_general->{$general} // {} }, %$conflicts };
      }
    }

    if (my $groups = $merged->{groups_by_conflict_type}) {
      while (my ($type, $new_groups) = each %$groups) {
        my $existing = $cd->groups_by_conflict_type->{$type} //= [];
        my %seen     = map { $_ => 1 } @$existing;
        push @$existing, grep { !$seen{$_}++ } @$new_groups;
      }
    }
  }

  sub schedule_cached_conflicts_merge($c, $app) {
    # Check if conflict building is complete
    my $conflict_cache = $c->conflict_cache();
    my $is_complete = $conflict_cache->get('conflict_building_complete');
    
    if ($is_complete) {
      $c->logger->info('Conflict building complete, loading final data');
      # Load final conflict data
      my $cached_conflicts = $conflict_cache->get('merged_conflicts');
      if ($cached_conflicts) {
        $c->get_conflict_detector(); # This will auto-update from cache
      }
      return;
    }
    
    # Not complete yet, retry in 30 seconds
    $c->logger->debug('Conflict building not complete yet, will retry in 30s');
    Mojo::IOLoop->timer(
      30 => sub { $c->schedule_cached_conflicts_merge($app) }
    );
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

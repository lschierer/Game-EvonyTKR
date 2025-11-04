use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Service::Cache;
use namespace::autoclean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use List::AllUtils qw( all any none );
  use Carp;

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';

  sub getBase($self) {
    return $base;
  }

  has 'pair_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs:');
  };

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts:');
  };

  sub get_conflict_detector {
    state $cd = Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
    );
    return $cd;
  }

  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    my $routes          = $app->routes->any($base);
    my $controller_name = $c->controller_name();

    $app->helper(
      get_conflict_detector => sub {
        return $c->get_conflict_detector();
      }
    );

    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->add_navigation_item({
      title  => 'General Conflict Groups',
      path   => $base,
      parent => '/Reference/',
      order  => 60,
    });

    $c->merge_cached_conflicts($app);
  }

  sub merge_cached_conflicts($c, $app){
    my $cd = $c->get_conflict_detector();
    my $cc = $c->pair_cache();
    my $merged = $cc->get('merged_conflicts') // {};
    $c->logger->debug(sprintf('merged_conflicts is %s',
    Data::Printer::np($merged)));
    my $by_general = {};
    if(exists $merged->{by_general}){
      $by_general = $merged->{by_general};
    }
    foreach my $general (keys $by_general->%* ) {
      $cd->by_general->{$general} //= {};
      foreach
        my $other_general (keys $by_general->{$general}->%* ) {
        $cd->by_general->{$general}->{$other_general} = 1;
      }
    }

    my $groups_by_conflict_type = {};
    if(exists $merged->{groups_by_conflict_type}){
      $groups_by_conflict_type = $merged->{groups_by_conflict_type} // {};
    }

    foreach
      my $conflict_type (keys $groups_by_conflict_type->%* ) {
      $cd->groups_by_conflict_type->{$conflict_type} //= [];
      push @{ $cd->groups_by_conflict_type->{$conflict_type} },
        @{ $groups_by_conflict_type->{$conflict_type} // [] };
    }
    my $finished = $cc->get('conflicts_complete');
    unless($finished){
      Mojo::IOLoop->timer(5 => sub {
        $c->merge_cached_conflicts($app);
      });
    }
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

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Conflict;
use namespace::clean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use List::AllUtils qw( all any none );
  use Carp;

  my $logger;

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';

  sub getBase($self) {
    return $base;
  }

  my $rootManager;

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
    $logger = $app->get_logger(__PACKAGE__);
    $logger->INFO("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    my $routes          = $app->routes->any($base);
    my $controller_name = $c->controller_name();

    $app->helper(get_conflict_detector => sub {
      return $c->get_conflict_detector();
    });

    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->add_navigation_item({
      title  => 'General Conflict Groups',
      path   => $base,
      parent => '/Reference/',
      order  => 60,
    });

    $app->plugins->on(
      conflicts_complete => sub ($self, $data) {
        $logger->INFO('Conflict Update detected');
        $logger->DEBUG(
          sprintf('data from conflicts_complete signal is %s',
            Data::Printer::np($data))
        );
        my $cd = $c->get_conflict_detector();
        if ($cd) {
          $cd->preseed(
            ($data->{by_general} // {}),
            ($data->{groups_by_conflict_type} // {})
          );
        }
      }
    );

  }

  sub index ($c) {
    $logger->DEBUG("Rendering conflict groups index");

    my $detector = $c->get_conflict_detector();
    $logger->DEBUG(sprintf('there are %s generals in the by_general index',
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

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

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';

  sub getBase($self) {
    return $base;
  }

  sub get_conflict_detector ($self) {
    return $self->app->get_root_manager->conflictDetector;
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    my $routes          = $app->routes->any($base);
    my $controller_name = $self->controller_name();

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
      general_loaded => sub {
        my ($plugin, $data) = @_;
        my $manager = $app->get_root_manager();
        my $gm      = $data->{manager};
        my $general = $data->{general};
        my $delay = rand(4.0);
        $logger->info(sprintf(
        'ConflictGroups Controller saw general_loaded event for %s, waiting %s',
        $general->name, $delay
        ));
        Mojo::IOLoop->timer(
          $delay => sub {
          $logger->debug(
            sprintf('conflict detection triggered for %s', $general->name));
          $manager->conflictDetector->process_single_general($general, $gm);
          $app->plugins->emit(
            conflicts_computed => { manager => $manager, general => $general });
        });
      }
    );
  }

  sub index ($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug("Rendering conflict groups index");

    my $detector = $self->get_conflict_detector();
    my $groups   = $detector->groups_by_conflict_type;
    my $pairs    = $detector->by_general;

    $self->stash(
      groups   => $groups,
      pairs    => $pairs,
      linkBase => $base,
    );

    return $self->render(template => '/general conflict groups/index');
  }
}
1;
__END__

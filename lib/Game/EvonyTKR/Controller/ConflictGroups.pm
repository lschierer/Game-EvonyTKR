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

  sub get_conflict_detector ($self) {
    return $rootManager->conflictDetector;
  }

  state @conflict_queue = ();

  sub register($c, $app, $config = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);
    $rootManager = $app->get_root_manager();

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

    $app->plugins->on(
      general_loaded => sub {
        my ($plugin, $data) = @_;
        my $general = $data->{general};
        unless (defined($general)) {
          $logger->error('general is undefined in general_loaded callback');
          return;
        }
        push @conflict_queue, $general;
      }
    );

    $app->plugins->on(
      generals_loaded => sub {
        foreach my $general (@conflict_queue) {
          my $delay = 10 + rand(10.0);
          Mojo::IOLoop->timer(
            $delay => sub {
              $c->load_one_general($general, $app);
            }
          );
        }
      }
    );

  }

  sub load_one_general($c, $general, $app) {
    my $gm               = $app->get_root_manager()->generalManager;
    my $conflictDetector = $c->get_conflict_detector();

    $logger->info(sprintf('load_one_general called for %s', $general->name));
    $conflictDetector->process_single_general($general, $gm);
    $app->plugins->emit(conflicts_computed => { general => $general });
  }

  sub index ($self) {
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

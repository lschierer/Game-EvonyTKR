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

    my $defined_tasks = $app->minion->tasks;
    unless (exists $defined_tasks->{detect_conflicts_for_general}) {
      $logger->error('detect_conflicts_for_general task did not define!!!');
    }

    $app->plugins->on(
      conflicts_complete => sub {
        $logger->info('Conflict Update detected');
        my ($plugin, $data) = @_;
        my $conflicts = $data->{conflicts} // {};
        $logger->debug(
          sprintf('data from conflicts_complete signal is %s',
            Data::Printer::np($conflicts))
        );
        my $cd = $app->get_root_manager()->conflictDetector;
        if ($cd) {
          if (exists $conflicts->{by_general}) {
            foreach my $gn (keys $conflicts->{by_general}->%*) {
              $logger->debug("conflicts by general for $gn");
              if (ref($conflicts->{by_general}->{$gn}) eq 'HASH') {
                foreach my $og ($conflicts->{by_general}->{$gn}->%*) {
                  next if (Scalar::Util::looks_like_number($og) && $og == 1);
                  $cd->by_general->{$gn}->{$og} = 1;
                }
              }
              else {
                $logger->warn(sprintf(
                  'conflicts by general for %s is a %s',
                  $gn, ref($conflicts->{by_general}->{$gn})
                ));
              }
            }
          }

          if (exists $conflicts->{groups_by_conflict_type}) {
            foreach my $key1 (keys $conflicts->{groups_by_conflict_type}->%*) {
              $logger->debug(sprintf(
                'conflicts for group ref: %s, contains: "%s"',
                ref($key1), $key1
              ));
              my $group = $conflicts->{groups_by_conflict_type}->{$key1};
              if (ref($group) eq 'ARRAY') {
                my @all = @{$group};
                if (exists $cd->groups_by_conflict_type->{$key1}) {
                  push @all, @{ $cd->groups_by_conflict_type->{$key1} };
                  @all = List::AllUtils::uniq @all;
                }
                $cd->groups_by_conflict_type->{$key1} = \@all;
              }
              else {
                $logger->warn(sprintf(
                  'conflict by type %s is a %s',
                  $group, ref($conflicts->{groups_by_conflict_type}->{$group})
                ));
              }

            }
            @{ $cd->groups_by_conflict_type->{group} } =
              List::AllUtils::uniq @{ $cd->groups_by_conflict_type->{group} };
          }
        }
      }
    );

  }

  sub index ($c) {
    $logger->debug("Rendering conflict groups index");

    my $detector = $c->get_conflict_detector();
    $logger->debug(sprintf('there are %s generals in the by_general index',
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

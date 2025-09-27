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

    my @conflictJobs;

    $app->plugins->on(
      generals_loaded => sub {
        unless (ref(\@conflict_queue) eq 'ARRAY') {
          $logger->error('conflict queue is not an array!!!');
          return;
        }
        foreach my $general (@conflict_queue) {
          unless (
            Scalar::Util::blessed($general) eq 'Game::EvonyTKR::Model::General')
          {
            $logger->error('retrieved invalid general from conflict queue: '
                . Data::Printer::np($general));
            return;
          }
          my $delay = 10 + rand(10.0);
          Mojo::IOLoop->timer(
            $delay => sub {
              my $jid = $app->minion->enqueue(
                detect_conflicts_for_general => [{
                  general_name => $general->name
                }]
              );
              push @conflictJobs, $jid;
            }
          );
        }
      }
    );

    my $conflictDetector = $app->get_root_manager->conflictDetector;

    foreach my $jid (@conflictJobs) {
      next unless defined $jid;
      my $job = $c->app->minion->job($jid);
      next unless $job;

      $c->app->minion->result_p($jid)->then(sub {
        my $result = shift;
        if (defined($result) && ref($result) eq 'HASH') {
          $logger->debug("job $jid result is " . Data::Printer::np($result));
          if ($result->{result}->{status} eq 'complete') {
            # Extract conflict cache data from job notes
            my $notes     = $job->info->{notes} || {};
            my $conflicts = $notes->{conflicts};
            if($conflicts) {
              foreach my $general (keys $conflicts->{by_general}->%* ) {
                foreach my $og ( $conflicts->{by_general}->{$general}->@* ){
                  push @{ $conflictDetector->by_general->{$general} }, $og;
                }
              }
              foreach my $group (keys $conflicts->{groups_by_conflict_type}->%* ) {
                foreach my $g ( $conflicts->{groups_by_conflict_type}->@* ) {
                  push @{ $conflictDetector->{groups_by_conflict_type} }, $g;
                }
              }
            }
          }
        }
        return 1;
      })->catch(sub {
        my $err = shift;
        $logger->error(
          "Job $jid failed: " . Data::Printer::np($err, multiline => 0));
        return undef;
      });
    }
  }

  sub index ($c) {
    $logger->debug("Rendering conflict groups index");

    my $detector = $c->get_conflict_detector();
    $logger->debug(sprintf('there are %s generals in the by_general index',
    scalar keys $detector->by_general->%* ));
    my $groups   = $detector->groups_by_conflict_type;
    my $pairs    = $detector->by_general;

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

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

    my @conflictJobs;
    my @completedJobs;

    $app->plugins->on(
      general_loaded => sub {
        my ($plugin, $data) = @_;
        my $general = $data->{general};
        unless (defined($general)) {
          $logger->error('general is undefined in general_loaded callback');
          return;
        }
        my $jid = $app->minion->enqueue(
          detect_conflicts_for_general => [{
            general_name => $general->name
          }]
        );
        push @conflictJobs, $jid;
      }
    );

    $app->plugins->on(
      generals_loaded => sub {
        unless (ref(\@conflict_queue) eq 'ARRAY') {
          $logger->error('conflict queue is not an array!!!');
          return;
        }

        my $conflictDetector = $c->get_conflict_detector();
        $logger->debug(sprintf('there are %s jobs registered in @conflictJobs',
        scalar @conflictJobs));

        foreach my $jid (@conflictJobs) {

          unless (defined $jid) {
            $logger->error('undefined jid in conflictJobs!!');
            next;
          }
          $logger->debug("inspecting jid $jid");
          my $job = $app->minion->job($jid);
          unless($job){
            $logger->error("undefined job for $jid");
            next;
          }
          my $delay = 10 + rand(10.0);
          Mojo::IOLoop->timer(
            $delay => sub {
              $app->minion->result_p($jid)->then(sub {
                my $result = shift;
                if (defined($result) && ref($result) eq 'HASH') {
                  $logger->debug("job $jid result is " . Data::Printer::np($result));
                  my $notes = $result->{notes} || {};
                  my $finalResult = $result->{result};
                  if ($finalResult =~ /Conflicts detected for/) {
                    # Extract conflict cache data from job notes

                    my $conflicts = $notes->{conflicts};
                    if($conflicts) {
                      foreach my $general (keys $conflicts->{by_general}->%* ) {
                        $logger->debug("conflicts in by_general for $general");
                        foreach my $og ( $conflicts->{by_general}->{$general}->%* ){
                          $logger->debug("conflicts in by_general for $general with $og");
                          $conflictDetector->by_general->{$general}->{$og} = 1;
                        }
                      }
                      foreach my $group (keys $conflicts->{groups_by_conflict_type}->%* ) {
                        $logger->debug("conflicts in groups_by_conflict_type for $group " . Data::Printer::np($conflicts->{groups_by_conflict_type}->{$group} ));
                        my @all;
                        if(exists $conflictDetector->groups_by_conflict_type->{$group} && defined $conflictDetector->groups_by_conflict_type->{$group}) {
                          push @all, @{ $conflictDetector->groups_by_conflict_type->{$group} };
                        }
                        push @all, @{ $conflicts->{groups_by_conflict_type}->{$group} };
                        @{ $conflictDetector->groups_by_conflict_type->{$group} } =
                        List::AllUtils::uniq @all;
                      }
                    }
                  } else {
                    $logger->error("final result is unexpected: $finalResult");
                  }
                }
                return 1;
              })->catch(sub {
                my $err = shift;
                $logger->error(
                  "Job $jid failed: " . Data::Printer::np($err, multiline => 0));
                return undef;
              });
              push @completedJobs, $jid;
            });
        }

        if(scalar(@completedJobs) == scalar(@conflictJobs)) {
           $app->plugins->emit('conflicts_complete');
        } else {
          $logger->error(sprintf(
            'Job completion mismatch: %d completed vs %d total jobs',
            scalar @completedJobs, scalar @conflictJobs
          ));
          if ($app->mode eq 'development') {
            croak 'Job completion count mismatch in development mode';
          }
        }
      });
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

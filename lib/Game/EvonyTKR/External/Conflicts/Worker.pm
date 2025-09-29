use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require Log::Log4perl;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;

package Game::EvonyTKR::External::Conflicts::Worker {
  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $app->minion->add_task(
      detect_conflicts_for_general => sub ($job, $args) {
        my $worker = ConflictWorkerLogic->new();
        my $result = $worker->process_general($args);
        $job->note(conflicts => $result);
        $job->finish("Conflicts detected for " . $args->{general_name});
      }
    );

    $app->minion->add_task(
      monitor_conflict_jobs => sub  {
        my $job = shift;
        my $app = $job->app;
        $logger->debug('calling monitor_conflict_jobs');
        __PACKAGE__->monitor_conflict_jobs($job, $app);
      }
    );

    state $ConflictProcessingStarted = 0;
    $app->plugins->on(
      generals_loaded => sub {
        if (!$ConflictProcessingStarted) {
          my $jid = $app->minion->enqueue(monitor_conflict_jobs => [],{ priority  => 30 });
          my $loop;
          $loop = Mojo::IOLoop->recurring(
            15 => sub {
              my $job = $app->minion->job($jid);
              if ($job) {
                my $state = $job->info->{state};
                if ($state eq 'failed') {
                  $logger->error("Conflict Processing Monitor $jid failed.");
                  return;
                }
                elsif ($state eq 'finished') {
                  my $notes = $job->info->{notes};

                  $app->plugins->emit('conflicts_complete' => {conflicts => $notes });
                }
              }
            }
          );
        }
      }
    );

    $app->plugins->on(
      general_loaded => sub {
        my ($plugin, $data) = @_;
        eval {
          my $general = $data->{general};
          unless (defined($general)) {
            $logger->error('general is undefined in general_loaded callback');
            return;
          }
          my $jid = $app->minion->enqueue(
            detect_conflicts_for_general => [{
              general_name => $general->name
            }],
          );
        };
        if ($@) {
          $logger->error(
            "Error in ConflictGroups' general_loaded callback: $@");
          return undef;
        }
      }
    );
  }

  sub monitor_conflict_jobs ($self, $job, $app) {
    $logger->debug('monitor_conflict_jobs started');

    my $by_general              = {};
    my $groups_by_conflict_type = {};
    my $processedJobs = {};

    my $loop;
    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        $logger = Log::Log4perl->get_logger(__PACKAGE__);
        eval {
          my $total = $app->minion->jobs({
            tasks => ['detect_conflicts_for_general'],
          })->total;
          $logger->debug("Successfully got total: $total");
        };
        if ($@) {
          $logger->error("Error getting total jobs: $@");
          return;
        }
        eval {
          my $stillWorking;
          eval {
            $stillWorking = $app->minion->jobs({
              tasks  => ['detect_conflicts_for_general'],
              states => ['active', 'inactive'],
            })->total;
            $logger->debug("Successfully got stillWorking: $stillWorking");
          };
          if ($@) {
            $logger->error("Error getting stillWorking count: $@");
            return;
          }

          $logger->debug("About to enter while loop");
          # iterate all tasks of this specific type,
          # of one of two possible states.
          eval {
            $app->minion->jobs({
              tasks  => ['detect_conflicts_for_general'],
              states => ['failed']
            })->each(sub {
              my $info = $_;
              $logger->debug(sprintf(
                'in while loop for job id %s with state %s',
                $info->{id}, $info->{state}
              ));

              my $state = $info->{state};

              if ($state eq 'failed') {
                $logger->debug(sprintf(
                  'task of type detect_conflicts_for_general '
                    . 'failed: %s, %s',
                  $info->{id}, Data::Printer::np($info)
                ));
                return;
              }
            });
          };
          if ($@) {
            $logger->error("Error running minion->jobs->each in "
                . "detect_conflicts_for_general callback: $@");
          }

          $logger->debug("About to enter while loop");
          # iterate all tasks of this specific type,
          # of one of two possible states.
          $app->minion->jobs({
            tasks  => ['detect_conflicts_for_general'],
            states => ['finished']
          })->each(sub {
            my $info = $_;
            $logger->debug(sprintf('checking job %s', $info->{id}));
            $self->handle_finished_job( $info, $processedJobs, $by_general,
              $groups_by_conflict_type);
          });

          $logger->debug('all jobs checked in this iteration of outer loop');
          if ($stillWorking == 0) {
            $logger->info('all Conflicts computed');
            Mojo::IOLoop->stop($loop);
          }
          else {
            $logger->debug("there are $stillWorking tasks remaining");
          }
        };
        if ($@) {
          $logger->error("Error in ConflictGroups generals_loaded "
              . "recurring loop callback: $@");
          return undef;
        }
        $job->note(by_general              => $by_general);
        $job->note(groups_by_conflict_type => $groups_by_conflict_type);
      }
    );

    Mojo::IOLoop->start($loop) unless Mojo::IOLoop->is_running;


  }

  sub handle_finished_job ($something, $info, $processedJobs, $by_general,
    $groups_by_conflict_type) {
    $logger->debug(sprintf(
      'in while loop for job id %s with state %s',
      $info->{id}, $info->{state}
    ));

    my $state = $info->{state};

    if ($state eq 'finished') {
      return if $processedJobs->{ $info->{id} };
      $processedJobs->{ $info->{id} } = 1;
      Mojo::IOLoop->timer(
        15 => sub {
          my $result = $info->{result};
          $logger->debug(sprintf(
            'result for job id %s is %s', $info->{id}, $result));
          my $notes = $info->{notes};
          $something->process_results($result, $notes, $by_general, $groups_by_conflict_type);

        }
      );
    }
    else {
      $logger->error(sprintf('unexpected state for job %s', $info->{id}));
    }
  }

  sub process_results ($job, $result, $notes, $by_general, $groups_by_conflict_type) {
    if ($result =~ /Conflicts detected for/) {
      my $conflicts = $notes->{conflicts};
      if ($conflicts) {
        foreach my $general (keys $conflicts->{by_general}->%*) {
          $logger->debug("conflicts in by_general for $general");
          foreach my $og ($conflicts->{by_general}->{$general}->%*) {
            $logger->debug(sprintf(
              'by_general reports conflict between %s <-> %s',
              $general, $og
            ));
            $by_general->{$general}->{$og} = 1;
          }
        }
        foreach
          my $group (keys $conflicts->{groups_by_conflict_type}->%*) {
          $logger->debug(
            sprintf(
              'conflicts in ' . 'groups_by_conflict_type' . ' for %s: ',
              $group
              )
              . Data::Printer::np(
              $conflicts->{groups_by_conflict_type}->{$group}
              )
          );
          my @all;
          if (exists $groups_by_conflict_type->{$group}
            && defined $groups_by_conflict_type->{$group}) {
            push @all, @{ $groups_by_conflict_type->{$group} };
          }
          push @all, @{ $conflicts->{groups_by_conflict_type}->{$group} };
          @{ $groups_by_conflict_type->{$group} } =
            List::AllUtils::uniq @all;
        }
      }
    }
    else {
      $logger->error("final result is unexpected: $result");
    }
  }

  class ConflictWorkerLogic : isa(Game::EvonyTKR::Shared::Constants) {
    use Log::Log4perl qw(:levels);
    use Unicode::Normalize;
    use Unicode::CaseFold qw(fc);
    use Encode            qw(is_utf8 decode_utf8 encode_utf8);
    use Carp;

    method process_general($args) {
      my $general_name = $args->{general_name};

      # Load all generals (like buff worker does)
      my $generalManager = Game::EvonyTKR::Model::General::Manager->new();
      my $bookManager    = Game::EvonyTKR::Model::Book::Manager->new();
      my $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
      my $collectionDir = $dist_dir->child("collections/data");
      $generalManager->importAll($collectionDir->child("generals"));
      $bookManager->importAll($collectionDir->child('skill books'));
      $bookManager->importAll($collectionDir->child('generic books'));
      # Create conflict detector

      foreach my $general (values $generalManager->get_all_generals()->%*) {
        unless ($general) {
          $self->logger->error(
            'ConflictWorkerLogic: undefined general in general manager!!');
          next;
        }
        $general->populateBuiltInBook($bookManager);
      }

      # Process conflicts for this general
      my $general = $generalManager->getGeneral($general_name);
      unless ($general) {
        $self->logger->error(sprintf(
        'ConflictWorkerLogic: general "%s" is not available '.
        'in the general manager! Available Generals are: %s',
          $general_name,
          join ', ',
          sort map { $_->name } values $generalManager->get_all_generals()->%*
        ));
        return;
      }
      $general->populateBuiltInBook($bookManager);

      my $conflictDetector =
        Game::EvonyTKR::Model::General::Conflict::Book->new(
        build_index      => 1,
        asst_has_dragon  => 1,
        asst_has_spirit  => 1,
        allow_wall_buffs => 1,
        );

      $conflictDetector->process_single_general($general, $generalManager);

      # Extract and return results
      return {
        by_general              => $conflictDetector->by_general,
        groups_by_conflict_type => $conflictDetector->groups_by_conflict_type,
        processed_general       => $general_name,
      };
    }
  }
}
1;
__END__

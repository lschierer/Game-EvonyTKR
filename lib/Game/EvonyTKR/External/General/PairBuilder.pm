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

package Game::EvonyTKR::External::General::PairBuilder {
  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $app->minion->add_task(
      build_pairs_for_primary => sub ($job, $args) {
        my $worker = PairBuilderWorkerLogic->new();
        $worker->execute($args);
        my $result = $worker->pairs_by_type;
        $job->note(pairs_by_type => $result);
        $job->finish("Pairs Created for " . $args->{general_name});
      }
    );

    state $PairMonitorStarted = 0;
    $app->plugins->on(
      conflicts_complete => sub {
        return if ($PairMonitorStarted);
        $PairMonitorStarted = 1;
        my ($plugin, $data) = @_;
        my $conflicts = $data->{conflicts} // {};
        my @generals;
        @generals = keys $conflicts->{by_general}->%* unless (!$conflicts);
        $logger->debug(sprintf(
          'there are %s generals from the conficts by_general keys.',
          scalar @generals));
        my $jid = $app->minion->enqueue(
          monitor_pair_builder => [{
            generals => \@generals
          }],
          { priority => 30 }
        );
        my $loop;
        $loop = Mojo::IOLoop->recurring(
          15 => sub {
            my $job = $app->minion->job($jid);
            if ($job) {
              my $state = $job->info->{state};
              if ($state eq 'failed') {
                $logger->error("Pair Building Monitor $jid failed.");
                return;
              }
              elsif ($state eq 'finished') {
                my $notes         = $job->info->{notes};
                my $pairs_by_type = $notes->{pairs_by_type};
                $app->plugins->emit(pairs_complete => $pairs_by_type);
                Mojo::IOLoop->remove($loop);
              }
              else {
                my $notes         = $job->info->{notes};
                my $pairs_by_type = $notes->{pairs_by_type};
                $app->plugins->emit(pairs_in_progress => $pairs_by_type);
              }
            }
          }
        );
      }
    );

    state $PairBuildingStarted = 0;
    $app->minion->add_task(
      monitor_pair_builder => sub {
        my $job  = shift;
        my $args = shift;
        $logger->debug(sprintf(
          'monitor_pair_builder task sees %s generals in args',
          scalar @{ $args->{generals} }));
        state $processedJobs = {};
        my $app           = $job->app;
        my $pairs_by_type = {};

        if (!$PairMonitorStarted) {
          foreach my $general ($args->{generals}->@*) {
            my $lock = $app->minion->lock('build_pairs_for_primary' . $general, 360);
            next unless $lock;
            my $jid = $app->minion->enqueue(
              build_pairs_for_primary => [{ general_name => $general }]);
          }
        }

        my $loop;
        # Assume at least one is working until proved otherwise
        my $stillWorking = 1;
        $loop = Mojo::IOLoop->recurring(
          10 => sub {
            eval {
              my $total = $app->minion->jobs({
                tasks => ['build_pairs_for_primary'],
              })->total;
              $logger->debug("Successfully got total: $total");
            };
            if ($@) {
              $logger->error("Error getting total jobs: $@");
              return;
            }

            eval {
              $stillWorking = $app->minion->jobs({
                tasks  => ['build_pairs_for_primary'],
                states => ['active', 'inactive'],
              })->total;
              $logger->debug("Successfully got stillWorking: $stillWorking");
            };
            if ($@) {
              $logger->error("Error getting stillWorking count: $@");
              return;
            }

            $logger->debug("About to enter each subroute looking for failed");
            $app->minion->jobs({
              tasks  => ['build_pairs_for_primary'],
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
                  'task of type build_pairs_for_primary ' . 'failed: %s, %s',
                  $info->{id}, Data::Printer::np($info)
                ));
                return;
              }
            });

            $app->minion->jobs({
              tasks  => ['build_pairs_for_primary'],
              states => ['finished']
            })->each(sub {
              my $info = $_;
              $logger->debug(sprintf(
                'in while loop for job id %s with state %s',
                $info->{id}, $info->{state}
              ));

              my $state = $info->{state};

              if ($state eq 'finished') {
                unless ($processedJobs->{ $info->{id} }) {
                  $processedJobs->{ $info->{id} } = 1;
                  Mojo::IOLoop->timer(
                    5 => sub {
                      __PACKAGE__->handle_result($info, $pairs_by_type);
                    }
                  );
                }
              }
              $logger->debug(
                'All jobs checked for this iteration of the outer loop');
              unless ($stillWorking > 0) {
                $logger->info('All Pairs Built.');
                Mojo::IOLoop->remove($loop);
              }
              else {
                $logger->debug(sprintf(
                  'there are %s PairBuilderWorkerLogic jobs remaining',
                  $stillWorking));
              }
              $job->note(pairs_by_type => $pairs_by_type);
            });
          }
        );
        $job->note(pairs_by_type => $pairs_by_type);
        Mojo::IOLoop->start()
          unless (Mojo::IOLoop->is_running() && $stillWorking > 0);
      }
    );

  }

  sub handle_result ($something, $info, $pairs_by_type) {
    my $result = $info->{result};
    $logger->debug(sprintf('result for job id %s is %s', $info->{id}, $result));
    my $notes = $info->{notes};

    if ($result =~ /Pairs Created for /) {
      my $newPairs = $notes->{pairs_by_type};
      if ($newPairs) {

        foreach my $type (keys %{$newPairs}) {
          my @all;
          if (exists $pairs_by_type->{$type}
            && defined $pairs_by_type->{$type}) {
            push @all, @{ $pairs_by_type->{$type} };
          }
          foreach my $np ($newPairs->{$type}->@*) {
            if (
              List::AllUtils::none {
                $_->{primary} eq $np->{primary}
                  && $_->{secondary} eq $np->{secondary}
              }
              @all
            ) {
              push @all, $np;
            }
          }
          $pairs_by_type->{$type} = \@all;
        }
      }
    }
    else {
      $logger->error("final result is unexpected: $result");
    }
  }

  class PairBuilderWorkerLogic : isa(Game::EvonyTKR::Shared::Constants) {
    use Log::Log4perl qw(:levels);
    use Unicode::Normalize;
    use Unicode::CaseFold qw(fc);
    use Encode            qw(is_utf8 decode_utf8 encode_utf8);
    use Carp;

    ADJUST {
      $self->get_logger('Game::EvonyTKR::External::General::PairBuilder');
    }

    field $pairs_by_type : reader = {};

    field $generalManager = Game::EvonyTKR::Model::General::Manager->new();
    field $bookManager    = Game::EvonyTKR::Model::Book::Manager->new();
    field $conflictDetector =
      Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
      );

    method execute($args) {

      my $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
      my $collectionDir = $dist_dir->child("collections/data");
      $generalManager->importAll($collectionDir->child("generals"));
      $bookManager->importAll($collectionDir->child('skill books'));
      $bookManager->importAll($collectionDir->child('generic books'));
      # Create conflict detector

      foreach my $general (values $generalManager->get_all_generals()->%*) {
        unless ($general) {
          $self->logger->error(
            'PairBuilderWorkerLogic: undefined general in general manager!!');
          next;
        }
        $general->populateBuiltInBook($bookManager);
      }

      my $conflicts = $args->{conflicts};
      if ($conflicts) {
        foreach my $general ($conflicts->{by_general}->%*) {
          foreach my $og ($conflicts->{by_general}->{$general}->%*) {
            $conflictDetector->by_general->{$general}->{$og} = 1;
          }
        }
        foreach my $group (keys $conflicts->{groups_by_conflict_type}->%*) {
          my @all;
          if (exists $conflictDetector->groups_by_conflict_type->{$group}
            && defined $conflictDetector->groups_by_conflict_type->{$group}) {
            push @all,
              @{ $conflictDetector->groups_by_conflict_type->{$group} };
          }
          push @all, @{ $conflicts->{groups_by_conflict_type}->{$group} };
          @{ $conflictDetector->groups_by_conflict_type->{$group} } =
            List::AllUtils::uniq @all;
        }
      }

      my $generalName = $args->{general_name};
      if ($generalName) {
        my $general = $generalManager->getGeneral($generalName);
        if ($general) {
          $self->build_pairs($general);
        }
      }
    }

    method build_pairs($primary) {
      my $generals = $generalManager->get_all_generals();
      my %initial_counts;
      foreach my $type (keys %{$pairs_by_type}) {
        my $tc = scalar @{ $pairs_by_type->{$type} } // 0;
        $initial_counts{$type} = $tc;
      }

      foreach
        my $secondary (sort { $a->name cmp $b->name } values %{$generals}) {
        next if $primary->name eq $secondary->name;
        $self->logger->debug(sprintf(
          'testing if %s and %s conflict.',
          $primary->name, $secondary->name
        ));
        next
          unless $conflictDetector->are_generals_compatible($primary,
          $secondary);

        $self->logger->debug(sprintf(
          'no conflict, testing %s and %s for common type.',
          $primary->name, $secondary->name
        ));
        my $primary_types     = $primary->type   // [];
        my $secondary_types   = $secondary->type // [];
        my %primary_types_map = map { $_ => 1 } @$primary_types;
        my @common = grep { $primary_types_map{$_} } @$secondary_types;
        @common = sort @common;
        next unless (scalar(@common) > 0);

        my $pair = {
          primary   => $primary->name,
          secondary => $secondary->name,
        };

        for my $t (@common) {
          $self->logger->debug(sprintf('%s <-> %s as %s',
            $pair->{primary}, $pair->{secondary}, $t));
          push @{ $pairs_by_type->{$t} }, $pair;
        }
      }

      my $total_added = 0;
      foreach my $type (keys %{$pairs_by_type}) {
        my $tc    = scalar @{ $pairs_by_type->{$type} } // 0;
        my $delta = $tc - ($initial_counts{$type} // 0);
        $total_added += $delta;
        $self->logger->debug(sprintf(
          'general %s has %s pairs for type %s',
          $primary->name, $delta, $type
        ));
      }
      $self->logger->info(
        sprintf('there are %s pairs for %s', $total_added, $primary->name));

    }
  }
}
1;
__END__

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
        # no more than 5 total pair builders at a time
        return $job->retry({delay => 5 }) unless my $guard1 = $job->app->minion->guard("pair_builders", 30, { limit => 5});
        my $gn = $args->{general_name} // '';

        # can't build pairs for an unknown general
        unless (length($gn)){
          $logger->error("General name is empty");
          return $job->finish('General name is empty');
        }

        # no more than one for this specific general
        return $job->finish(sprintf('pairs already in progress for %s', $gn)) unless my $guard2 = $app->minion->guard("build_pairs_for_$gn",10);

        # actually do the work
        my $worker = PairBuilderWorkerLogic->new();
        $worker->execute($args);
        my $result = $worker->pairs_by_type;
        $job->note(pairs_by_type => $result);
        $job->finish("Pairs Created for $gn");
      }
    );

    $app->minion->add_task(
      build_all_pairs => sub ($job, $args) {
        return $job->finish('pairs already being built') unless my $guard = $app->minion->guard('build_all_pairs', 60);
        my $app = $job->app;

        $logger->info("Building All Pairs");
        my $conflicts = $args->{conflicts} // {};
        my @generals;
        @generals = keys $conflicts->{by_general}->%* unless (!$conflicts);
        $logger->debug(sprintf(
          'there are %s generals from the conficts by_general keys.',
          scalar @generals));
        my @pairBuilders;
        foreach my $general (@generals){
          my $jid = $app->minion->enqueue(build_pairs_for_primary => [{ general_name => $general }], {
            priority  => 10,
            attempts  => 5,
            delay     => (0.1 + rand(0.5)),
          });
          push @pairBuilders, $jid;
        }
        $job->note(pairs_builders => \@pairBuilders);
        $job->finish('pair builders launched');
      });

    $app->minion->add_task(monitor_pair_building => sub ($job, $args) {
      my $pairs_by_type = {};

      my $builder_jid = $args->{builder_jid};
      unless ($builder_jid) {
        $logger->error("no builder provided to monitor!");
        $job->finish('no builder to monitor!');
      }
      my $builder = $job->app->minion->job($builder_jid);
      unless ($builder) {
        $logger->error("No builder found for $builder_jid");
        $job->finish("No builder found for $builder_jid");
      }
      my $conflicts = $args->{conflicts};
      unless ($conflicts) {
        $logger->error("No conflicts provided for monitor_pair_building");
        $job->finish('No conflicts provided for monitor_pair_building');
      }

      my $loop;
      $loop = Mojo::IOLoop->recurring( 15 => sub {

        if($builder->info->{state} eq 'failed'){
          unless ($builder->info->{retries} >= 5 ){
            $logger->error(sprintf('build spawner has failed %s times.', $builder->info->{retries}));
            Mojo::IOLoop->remove($loop);
            $job->finish(sprintf('build spawner has failed %s times.', $builder->info->{retries}));
          }
          $builder->retry({delay => 300 });
          return;
        } elsif ($builder->info->{state} eq 'finished') {
          my $builders = $builder->info->{notes}{pairs_builders};
          unless(scalar(@$builders)){
            $logger->error('no builders found from pair builder job!!');
            Mojo::IOLoop->remove($loop);
            $job->finish('no builders found from pair builder job!!');
          }
          my $isFailed = 0;
          my $inProgress = 0;
          while (my $info = $job->app->minion->jobs({ids => [ $builders->@* ] })->next ) {
            if($info->{state} eq 'failed' || $isFailed) {
              if(!$isFailed){
                $logger->error(sprintf('failed job %s detected. cancelling jobs.', $info->{id}));
              } else {
                $logger->debug(sprintf('cleaning up job %s after failure detected', $info->{id}));
              }
              $isFailed = 1;
              $job->app->minion->job($info->{id})->remove;
            } elsif ( $info->{state} eq 'finished') {
              __PACKAGE__->handle_result($info, $pairs_by_type);
              $job->note(pairs_in_progress => $pairs_by_type);
            } else {
              $inProgress++;
            }
          }
          if($inProgress > 0){
            $logger->info("there are $inProgress pair builders remaining");
            return;
          } else {
            $logger->info("All Pair Builders Complete");
            $job->note(pairs_complete => $pairs_by_type);
            $Mojo::IOLoop->remove($loop);
            return $job->finish(pairs_complete => $pairs_by_type);
          }
        }
      });
      Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
    });

    $app->plugins->on(conflicts_complete => sub {
    my ($plugin, $data) = @_;
    my $conflicts = $data->{conflicts} // {};
    my $finalBatch = $data->{final} // 0;
    return unless($finalBatch);

      unless( my $guard = $app->minion->guard('monitor_pair_building', 300)) {
        $logger->warn('monitor_pair_building already present');
        return;
      }

      my $builder_jid = $app->minion->enqueue(build_all_pairs => [{
        conflicts => $conflicts
      }]);

      my $monitor_jid = $app->minion->enqueue(monitor_pair_building => [{
        conflicts   => $conflicts,
        builder_jid => $builder_jid,
      }]);

      my $loop;
      $loop = Mojo::IOLoop->recurring( 5 => sub {
        my $job = $app->minion->job($monitor_jid);
        unless($job) {
          $logger->error("no job found for $monitor_jid");
          Mojo::IOLoop->remove($loop);
          return;
        }
        if ($job->info->{state} eq 'failed') {
          $logger->error("monitor job $monitor_jid failed!!");
          Mojo::IOLoop->remove($loop);
          return;
        }

        if ($job->info->{state} eq 'finished'){
          $logger->info("monitor_pair_building finished successfully");
          my $pairs_by_type = $job->info->{notes}{pairs_complete};
          $app->plugins->emit(pairs_complete => $pairs_by_type);
          Mojo::IOLoop->remove($loop);
          return;
        }

        my $pairs_by_type = $job->info->{notes}{pairs_in_progress};
        $app->plugins->emit(pairs_in_progress => $pairs_by_type);
      });
      Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
    });

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

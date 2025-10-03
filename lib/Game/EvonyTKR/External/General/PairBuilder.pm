use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require YAML::PP;
require Log::Log4perl;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;

class Game::EvonyTKR::External::General::PairBuilder : isa(Game::EvonyTKR::Shared::Constants) {
  use Unicode::Normalize;
  use Unicode::CaseFold qw(fc);
  use Encode            qw(is_utf8 decode_utf8 encode_utf8);
  use Carp;

  field $app            : param;
  field $job            : param;
  field $general_names  : param = [];
  field $conflicts      : param = {};

  field $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
  field $generals   = {};
  field $builderJobs = [];

  ADJUST {
    my $collectionDir = $dist_dir->child('collections/data');
    my $generalsDir = $collectionDir->child('generals');
    my $bookDir = $collectionDir->child('skill books');
    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );
    foreach my $gn ($general_names->@*){
      my ($file) = grep {
        my $nf = $self->normalize_name($_->basename('.yaml'));
        my $nn = $self->normalize_name($gn);
        $nf eq $nn;
      } $generalsDir->children;
      unless(defined($file) && $file->is_file()) {
        $self->worker_croak("no yaml file found for $gn");
        next;
      }
      my $data   = $file->slurp_utf8;
      my $gho = $ypp->load_string($data);
      my $general = Game::EvonyTKR::Model::General->from_hash($gho, $self->logger);

      my ($bookFile) = grep {
        my $nf = $self->normalize_name($_->basename('.yaml'));
        my $nn = $self->normalize_name($general->builtInBookName);
        $nf eq $nn;
      } $bookDir->children;
      unless(defined($bookFile) && $bookFile->is_file()){
        $self->worker_croak(sprintf('no yaml file found for "%s"', $general->builtInBookName));
        next;
      }
      my $bd = $bookFile->slurp_utf8;
      my $bho = $ypp->load_string($bd);
      my $book = Game::EvonyTKR::Model::Book::Builtin->from_hash($bho, $self->logger);
      unless(Scalar::Util::blessed($book) eq
        'Game::EvonyTKR::Model::Book::Builtin') {
        $self->worker_croak('failed to import book ' . $general->builtInBookName);
        next;
      }
      $general->set_builtInBook($book);

      $generals->{$general->name} = $general;
    }
  }

  ADJUST {
    $app->minion->add_task(build_pairs_for_primary => sub ($job, $args){
      my $general_name = $args->{general_name};
      unless(length($general_name)){
        $self->logger->error('general_name not provided to build_pairs_for_primary');
        return $job->finish('general_name not provided to build_pairs_for_primary');
      }
      return $job->finish(sprintf('build_pairs_for_primary for %s already launched', $general_name))
        unless my $bppGuard = $app->minion->guard("build_pairs_for_primary_${general_name}", 360);
      $self->build_pairs_for_primary($general_name);
    });
  }

  method execute {
    if(scalar(keys $generals->%*) > 0){
      $self->build_all_pairs();
    }

  }

  method build_all_pairs {
    foreach my $general (sort {$a->name cmp $b->name } values $generals->%*){
      my $jid = $app->minion->enqueue(build_pairs_for_primary => [{
        general_name => $general->name,
      }], {
        priority  => -1,
        attempts  => 5,
        expire    => 7200,
      });
      push @$builderJobs, $jid;
      $self->logger->debug(sprintf('pair builder kicked off for general "%s" with jid %s', $general->name, $jid));
    }
  }

  method normalize_name ($name) {
    my $dn = is_utf8($name) ? $name : decode_utf8($name);
    my $nn = fc(NFKD($dn));
    $nn =~ s/[’''‛`´]/'/g;
    $nn =~ s/[""‟]/"/g;      # Quotes
    return $nn;
  }
}
1;
__END__

  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $app->minion->add_task(
      build_pairs_for_primary => sub ($job, $args) {
        # no more than 5 total pair builders at a time
        return $job->retry({ delay => 5 })
          unless my $guard1 =
          $job->app->minion->guard("pair_builders", 30, { limit => 5 });
        my $gn = $args->{general_name} // '';

        # can't build pairs for an unknown general
        unless (length($gn)) {
          $logger->error("General name is empty");
          return $job->finish('General name is empty');
        }

        # no more than one for this specific general
        return $job->finish(sprintf('pairs already in progress for %s', $gn))
          unless my $guard2 = $app->minion->guard("build_pairs_for_$gn", 10);

        my $conflicts = $args->{conflicts} // {};

        # actually do the work
        my $worker = PairBuilderLogic->new(
          app         => $job->app,
          conflicts   => $conflicts,
          job         => $job,
        );
        $worker->execute($args);
        my $result = $worker->pairs_by_type;
        $job->note(pairs_by_type => $result);
        $job->finish("Pairs Created for $gn");
      }
    );

    $app->minion->add_task(monitor_and_build => sub ($job, $args) {
      return $job->finish('only one monitor_and_build job allowed')
        unless my $mbGuard = $app->minion->guard('monitor_and_build', 3600);

        my $conflicts = $args->{conflicts};
        my $pairBuilderLogic = PairBuilderLogic->new(
          app       => $app,
          conflicts => $conflicts,
          job       => $job,
        );
        return $pairBuilderLogic->monitor_and_build();
    });

    state $buildingPairs = 0;
    $app->plugins->on(
      conflicts_complete => sub {
        my ($plugin, $data) = @_;
        my $conflicts  = $data->{conflicts} // {};

        my $jid;
        if($buildingPairs) {
          $logger->warn('monitor_pair_building already present');
          return;
        }
        $buildingPairs = 1;
        $jid = $app->minion->enqueue(monitor_and_build => [{
          conflicts => $conflicts,
        }], {
          priority  => 90,
          attempts  => 5,
          delay     => 30,
        });
        my $loop;
        $loop = Mojo::IOLoop->recurring(10 => sub {
          my $job = $app->minion->job($jid);
          if($job) {
            my $notes = $job->info->{notes};
            if($notes) {
              if(exists $notes->{pairs_by_type} && ref($notes->{pairs_by_type}) eq 'HASH'){
                $app->plugins->emit(pairs_by_type => $notes->{pairs_by_type});
              }
            }
            if($job->info->{state} eq 'finished'){
              Mojo::IOLoop->remove($loop);
              return;
            }
          } else {
            $logger->warn("no job available for jid $jid");
            Mojo::IOLoop->remove($loop);
            return;
          }
        });
    });
  }

  $app->minion->add_task('prune_pairs' => sub ($job, $args){
    my $conflicts = $args->{conflicts};
    my $pairs = $args->{pairs_by_type};
    my $pairBuilderLogic = PairBuilderLogic->new(
      app             => $app,
      conflicts       => $conflicts,
      pairs_by_type   => $pairs,
      job             => $job,
    );
    return $pairBuilderLogic->prune_pairs();
  });


  class PairBuilderLogic  {
    use Log::Log4perl qw(:levels);
    use Unicode::Normalize;
    use Unicode::CaseFold qw(fc);
    use Encode            qw(is_utf8 decode_utf8 encode_utf8);
    use Carp;

    ADJUST {
      $self->get_logger('Game::EvonyTKR::External::General::PairBuilder');
    }

    field $app : param;
    field $job : param;
    field $conflicts : param = {};


    field $pairs_by_type : param : reader = {};

    field $generalManager = Game::EvonyTKR::Model::General::Manager->new();
    field $bookManager    = Game::EvonyTKR::Model::Book::Manager->new();
    field $conflictDetector =
      Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
      );

    field $generals = [];
    field $builderTasks = {};
    field $finalConflictSetSeen : reader : writer = 0;

    ADJUST {
      $self->setup_generals();
    }

    method monitor_and_build  {
      $self->spawn_pair_builders();
      $self->monitor_pair_building();
      $self->prune_task();
    }

    method setup_generals {
      # isolate the use of the generalManager here
      # because long term I want to get rid of it.
      my $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
      my $collectionDir = $dist_dir->child("collections/data");
      $generalManager->importAll($collectionDir->child("generals"));
      $bookManager->importAll($collectionDir->child('skill books'));
      $bookManager->importAll($collectionDir->child('generic books'));

      foreach my $general (sort {$a->name cmp $b->name} values $generalManager->get_all_generals()->%*) {
        unless ($general) {
          $self->logger->error(
            'PairBuilderWorkerLogic: undefined general in general manager!!');
          next;
        }
        $general->populateBuiltInBook($bookManager);
        push @$generals, $general;
      }
    }

    method spawn_pair_builders {
      $self->logger->info(sprintf('Creating Pair Builder Tasks for %s generals', scalar(@$generals)));
      foreach my $general (sort {$a->name cmp $b->name } $generals->@* ){
        my $jid = $app->minion->enqueue(build_pairs_for_primary => [{
          general_name      => $general->name,
          conflicts         => $conflicts,
        }] => {
          priority  => 1,
          attempts  => 10,
          delay     => 30,
          expire    => 300,
        });
        if($jid){
          $builderTasks->{$general->name} = {
            jid     => $jid,
            state   => $app->minion->job($jid)->info->{state},
          };
        }
      }
    }

    method prune_task {
      my $jid = $app->minion->enqueue(prune_pairs => [{
        conflicts     => $conflicts,
        pairs_by_type => $pairs_by_type,
      }]);
      my $loop;
      my $result;
      return->
      Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
      return $result;
    }

    method prune_pairs {
      foreach my $type (keys $pairs_by_type->%*){
        $self->logger->debug(sprintf('There are %s pairs of type %s and %s conflicts ',
        scalar($pairs_by_type->{$type}->@* ), $type, scalar(keys $conflicts->{by_general}->%*) ));
      }
      if(scalar(keys $pairs_by_type->%*) == 0){
        $self->logger->warn('No pairs to prune.');
        return;
      }
      foreach my $general_name (sort keys $conflicts->{by_general}->%*) {
        my $general;
        foreach my $type (keys $pairs_by_type->%*) {
          my $generals = $pairs_by_type->{$type};
          $general = List::AllUtils::first { $_->name eq $general_name } $general->@*;
          last if $general;
        }
        next unless($general);
        # a given general may be in multiple types.
        foreach my $type (keys $pairs_by_type->%*) {

          @{ $pairs_by_type->{$type}} = grep {
            my $pair = $_;
            not (
              $pair->primary->name eq $general->name and
              List::AllUtils::any { $_ eq $pair->secondary->name } $conflicts->{by_general}->{$general->name}->@*
            );
          } @{ $pairs_by_type};
        }
      }
      $job->note(pairs_by_type => $pairs_by_type);
      $job->finish("Pairs Filtered");
    }

    method monitor_pair_building {
      my $loop;
      $loop = Mojo::IOLoop->recurring(5 => sub {
        my $jobs;
        $jobs = $app->minion->jobs({
          states  => ['failed'],
          tasks   => ['build_pairs_for_primary'],
        });
        while (my $info = $jobs->next) {
          unless ($info->{retries} >= 5 ){
            next;
          }
          $self->logger->error(sprintf('build_pairs_for_primary job %s failed: %s',
          $info->{id}, $info->{result}));
          $app->minion->job($info->{id})->remove;
        }
        $jobs = undef;
        $jobs = $app->minion->jobs({
          states  => ['finished'],
          tasks   => ['build_pairs_for_primary'],
        });
        my $sendUpdate = 0;
        $sendUpdate = 1 if ($jobs->total > 0);
        while (my $info = $jobs->next) {
          if($info->{result} =~ /Pairs Created for /){
            my $newPairs = $info->{notes}{pairs_by_type};
            foreach my $type (keys $newPairs->%*){
              $self->logger->debug("adding pairs for $type");
              my @all;
              if(exists $pairs_by_type->{$type} && ref($pairs_by_type->{$type}) eq 'ARRAY'){
                push @all, $pairs_by_type->{$type}->@*;
              }
              push @all, $newPairs->{$type}->@*;
              $pairs_by_type->{$type} = [ List::AllUtils::uniqby { $_->primary->name . '/' . $_->secondary->name } @all ];
            }
          } else {
            $self->logger->error(sprintf('unexpected result for job %s: %s',
            $info->{id}, Data::Printer::np($info)));
          }
        }
        if($sendUpdate){
          $sendUpdate = 0;
          $job->note(pairs_by_type => $pairs_by_type);
        }
        $jobs = undef;
        $jobs = $app->minion->jobs({
          states  => ['active', 'inactive'],
          tasks   => ['build_pairs_for_primary'],
        });
        if($jobs->total == 0){
          $self->logger->info("Pair building Complete");
          Mojo::IOLoop->remove($loop);
          $job->note(pairs_by_type => $pairs_by_type);
          return $job->finish("Pair building Complete");
        }
      });
      Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
    }

    method execute($args) {

      my $general_name = $args->{general_name};
      if ($general_name) {
        $self->debug(sprintf('building pairs for %s', $general_name));
        my $general = $generalManager->getGeneral($general_name);
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

use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Shared::Logger;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::External::General::PairBuilder;

package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Minion::Job', -signatures;
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub log_config ($self) {
    $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
  }

  state $OnlyOnePrebuild = 0;

  my $pairBuilder;

  sub register ($self, $app, $conf = {}) {
    $logger = $self->log_config();
    $logger->DEBUG(sprintf('register function for "%s"', __PACKAGE__));

    $app->minion->add_task(external_prebuild => __PACKAGE__);

    $pairBuilder = Game::EvonyTKR::External::General::PairBuilder->new(
      app     => $app,
    );

    my $pbTasks = $pairBuilder->get_tasks();
    foreach my $task_name (keys $pbTasks->%*) {
      my $task = $pbTasks->{$task_name};
      $app->minion->add_task(
        $task_name => sub($job, @args) {
          $logger->DEBUG("Running task $task_name");
          $task->($job, @args);
          $job->finish();
        }
      );
      my $tasks = $app->minion->tasks();
      my @tns   = keys %$tasks;
      $logger->DEBUG(sprintf('registered tasks include %s', join ', ', @tns));
    }

    $app->plugins->on(worker_started => sub{
    if (!$OnlyOnePrebuild) {
        $OnlyOnePrebuild = 1;

        if (my $guard = $app->minion->guard('external_prebuild', 0)) {
          my $prebuildJid = $self->startPrebuild($app);
          $self->monitorPrebuild($app, $prebuildJid);
        }
      }
    });
  }

  sub monitorPrebuild ($plugin, $app, $prebuildJid) {
    my $loop;
    my $retryCount = 0;
    my $maxRetries = 5;
    $loop = Mojo::IOLoop->recurring( 10 => sub {
      my $job = $app->minion->job($prebuildJid);
      unless($job){
        $logger->ERR("prebuildJid $prebuildJid is not associated with a valid job.");
        $retryCount++;
        if($retryCount >= $maxRetries){
          Mojo::IOLoop->remove($loop);
        }
        return;
      }
      my $notes = $job->info->{notes} // {};
      my $pairs_by_type = $app->get_general_pairs();
      foreach my $key (keys $notes->%*){
        if($key eq 'pairs_by_type'){
          $pairs_by_type = $notes->{$key};
          $logger->DEBUG('detected pairs_by_type update: ' . Data::Printer::np($pairs_by_type, multiline => 0));
          $app->plugins->emit(pairs_by_type => $pairs_by_type);
        }
      }
      if($job->info->{state} eq 'finished'){
        $app->plugins->emit(pairs_complete => $pairs_by_type);
        Mojo::IOLoop->remove($loop);
      }
    });
    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }

  sub startPrebuild ($self, $app) {
    # Skip if this is admin interface
    if ($app->can('req') && $app->req && $app->req->url->path =~ m{^/minion}) {
      $OnlyOnePrebuild = 0;
      return;
    }
    my $prebuildJid;
    if (not defined $prebuildJid) {
      $prebuildJid = $app->minion->enqueue(
        external_prebuild => [] => {
          priority => 100,
          attempts => 5,
          expire   => 7200,
          unique   => 'external_prebuild',
        }
      );
    }
    return $prebuildJid;
  }

  sub run ($self, @args) {
    $logger->DEBUG(sprintf('%s run method starting',            __PACKAGE__));
    $logger->DEBUG(sprintf('%s run method escaped guard check', __PACKAGE__));

    #wait to ensure the workers have the new tasks
    sleep 15;

    my $jobsState = {};
    my $jid;
    $jid = $self->app->minion->enqueue(build_all_pairs => [{ }] => {
      priority  => 50,
      attempts  => 5,
      expire    => 720,
    });
    $jobsState->{$jid} = $self->app->minion->job($jid)->info;

    $jid = $self->app->minion->enqueue(monitor_pair_builders => [{}] => {
      priority  => 90,
      attempts  => 5,
      delay     => 15,
      expire    => 7200,
    });
    $jobsState->{$jid} = $self->app->minion->job($jid)->info;


    my $loop;
    $loop = Mojo::IOLoop->recurring(10 => sub {
      my @expectedComplete = keys $jobsState->%*;
      my @jids = sort keys $jobsState->%*;
      foreach my $index (0 .. $#jids ) {
        my $jid = $jids[$index];
        my $job = $self->app->minion->job($jid);
        unless($job){
          $logger->ERR("No Job available for previously recorded jid $jid");
          $self->note(ERROR => "No Job available for previously recorded jid $jid");
          Mojo::IOLoop->remove($loop);
          $self->finish('Invalid tracked JID');
        }
        $jobsState->{$jid} = $job->info;
        if($job->info->{state} eq 'failed') {
          $logger->ERR(sprintf('failed job %s for task %s, result %s', $jid, $job->info->{task}, $job->info->{result}));
          $self->note(ERROR => sprintf('failed job %s: %s', $job->info->{task}, $job->info->{result}));
          Mojo::IOLoop->remove($loop);
          $self->finish('failed tracked task');
        }
        if($job->info->{state} eq 'finished'){
          $self->note($job->info->{task} => $job->info->{result});
          if($job->info->{task} eq 'build_all_pairs' && $job->info->{result} ne 'all builder jobs started'){
            $jobsState->{$jid}->{state} = 'unexpected';
            next;
          }
          if($job->info->{task} eq 'monitor_pair_builders') {
            $self->note(pairs_by_type => $job->info->{notes}->{pairs_by_type} // {});
            if($job->info->{result} ne 'all pair builders complete')
            {
              $jobsState->{$jid}->{state} = 'pending';
              next;
            }
          }
        }
        if($job->info->{task} eq 'monitor_pair_builders') {
          $self->note(pairs_by_type => $job->info->{notes}->{pairs_by_type} // {});
        }
      }

      my @complete = grep { $_->{state} eq 'finished' } values $jobsState->%*;
      if(scalar(@expectedComplete) == scalar(@complete)){
        $logger->INFO('All Monitored Jobs Complete');
        $self->note(complete => 'All Monitored Jobs Complete');
        Mojo::IOLoop->remove($loop);
        $self->finish('All Monitored Jobs Complete');
      } else {
        $logger->DEBUG(sprintf('found %s complete, expected %s', scalar(@complete), scalar(@expectedComplete) ));
      }
    });
    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}
1;
__END__

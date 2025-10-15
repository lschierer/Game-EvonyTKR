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
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::External::General::Pair::Workflow;



package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  state $OnlyOnePrebuild = 0;

  sub register ($self, $app, $conf = {}) {
    $self->SUPER::register($app, $conf);
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug(sprintf('register function for "%s"', __PACKAGE__));

    # Register main prebuild orchestration task
    $app->minion->add_task(external_prebuild => __PACKAGE__);
    $app->plugin('Game::EvonyTKR::External::General::Pair::Workflow');

    # Register pair workflow tasks
    #$app->plugin('Game::EvonyTKR::External::General::Pair::Workflow');
    my $tasks = $app->minion->tasks();
    my @tns   = keys %$tasks;
    $logger->debug(sprintf('registered tasks include %s', join ', ', @tns));

    my $pair_workflow_loaded = 1;
    $app->plugins->on(
      pair_workflow_loaded => sub {
        $pair_workflow_loaded = 1;
      }
    );

    my $mojo_worker_started = 0;
    $app->plugins->on(
      mojo_worker_started => sub {
        $mojo_worker_started = 1;
        my $loop;
        $loop = Mojo::IOLoop->recurring(
          5 => sub {
            if ($pair_workflow_loaded) {
              if (!$OnlyOnePrebuild) {
                $OnlyOnePrebuild = 1;

                if (my $guard = $app->minion->guard('external_prebuild', 0)) {
                  my $prebuildJid = $self->startPrebuild($app);
                  $self->monitorPrebuild($app, $prebuildJid);
                }
              } else {
                $logger->debug('OnlyOnePrebuild prevented restart');
              }
             Mojo::IOLoop->remove($loop);
            } else {
              $logger->debug(sprintf('mojo_worker_started is %s; pair_workflow_loaded is %s.',
              $mojo_worker_started ? 'true' : 'false', $pair_workflow_loaded ? 'true' : 'false'));
            }
          });
        Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
      }
    );

  }

  sub startPrebuild ($self, $app) {
    my $jid = $app->minion->enqueue(
      'external_prebuild' => [{}] => {
        priority => 100,
        attempts => 3,
        expire   => 7200,
      }
    );
    $logger->info("Started prebuild orchestrator job $jid");
    return $jid;
  }

  sub monitorPrebuild ($plugin, $app, $prebuildJid) {
    my $loop;
    my $retryCount = 0;
    my $maxRetries = 5;

    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        my $job = $app->minion->job($prebuildJid);
        unless ($job) {
          $logger->error(
            "prebuildJid $prebuildJid is not associated with a valid job.");
          $retryCount++;
          if ($retryCount >= $maxRetries) {
            Mojo::IOLoop->remove($loop);
          }
          return;
        }

        my $info  = $job->info;
        my $notes = $info->{notes} // {};

        # Check for pairs completion and emit to Mojolicious
        if (my $pairs_by_type = $notes->{pairs_by_type}) {
          $logger->debug('detected pairs_by_type update');
          $app->plugins->emit(pairs_by_type => $pairs_by_type);
        }

        # Check for conflicts completion and emit to Mojolicious
        if (my $conflicts = $notes->{conflicts}) {
          $logger->debug('detected conflicts update');
          $app->plugins->emit(conflicts_complete => $conflicts);
        }

        # Check if prebuild is complete
        if ($info->{state} eq 'finished') {
          $logger->info('Prebuild orchestration complete');
          $app->plugins->emit(
            prebuild_complete => {
              pairs     => $notes->{pairs_by_type},
              conflicts => $notes->{conflicts}
            }
          );
          Mojo::IOLoop->remove($loop);
        }
        elsif ($info->{state} eq 'failed') {
          $logger->error(
            "Prebuild failed: " . ($info->{result} // 'unknown error'));
          Mojo::IOLoop->remove($loop);
        }
      }
    );
  }

  # Main prebuild orchestration job
  sub run ($self, @args) {
    $self->SUPER::run(@args);
    $logger->debug('Prebuild orchestration starting');

     #Start pair building workflow
    my $pair_workflow_jid = $self->app->minion->enqueue(
      'build_all_pairs' => [{}] => {
        priority => 50,
        attempts => 5,
        expire   => 3600,
      }
    );

    # Start monitoring
    my $monitor_jid = $self->app->minion->enqueue(
      'monitor_pair_builders' => [{}] => {
        priority => 90,
        attempts => 5,
        delay    => 15,
        expire   => 7200,
      }
    );

    # Monitor completion
    my $loop;
    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        # Check if pair workflow completed
        my $completed_pairs = $self->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['finished']
        })->total;

        my $active_pairs = $self->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['active', 'inactive']
        })->total;

        # Check monitor job for results
        my $monitor_job = $self->app->minion->job($monitor_jid);
        if (

            ($monitor_job && $monitor_job->info->{state} eq 'finished') ||
            ($monitor_job && $monitor_job->info->{state} eq 'inactive' && $monitor_job->info->{retries} > 0)
          ) {
          # Collect incremental results
          my $pairs_by_type = $monitor_job->info->{notes}->{pairs_by_type}
            // {};
          my $conflicts = $monitor_job->info->{notes}->{conflicts} // {};

          $self->note(pairs_by_type => $pairs_by_type);
          $self->note(conflicts     => $conflicts);

          # if result is final
          if (exists($monitor_job->info->{result}) && length($monitor_job->info->{result}) && $monitor_job->info->{result} eq 'all pair builders complete') {
            Mojo::IOLoop->remove($loop);
            $self->finish('Prebuild orchestration complete');
          }
        }
      }
    );

    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}

1;

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

    $pairBuilder =
      Game::EvonyTKR::External::General::PairBuilder->new(app => $app,);

    foreach my $task_name (keys $pairBuilder->tasks->%*) {
      my $task = $pairBuilder->tasks->{$task_name};
      $app->minion->add_task(
        $task_name => sub($job, @args) {
          $logger->DEBUG("Running task $task_name");
          $task->($job, @args);
          $job->finish();
        }
      );
    }

    my $tasks = $app->minion->tasks();
    my @tns   = keys %$tasks;
    $logger->DEBUG(sprintf('registered tasks include %s', join ', ', @tns));

    $app->plugins->on(
      mojo_worker_started => sub {
        if (!$OnlyOnePrebuild) {
          $OnlyOnePrebuild = 1;

          if (my $guard = $app->minion->guard('external_prebuild', 0)) {
            my $prebuildJid = $self->startPrebuild($app);
            $self->monitorPrebuild($app, $prebuildJid);
          }
        }
      }
    );
  }

  sub monitorPrebuild ($plugin, $app, $prebuildJid) {
    my $loop;
    my $retryCount = 0;
    my $maxRetries = 5;
    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        my $job = $app->minion->job($prebuildJid);
        unless ($job) {
          $logger->ERR(
            "prebuildJid $prebuildJid is not associated with a valid job.");
          $retryCount++;
          if ($retryCount >= $maxRetries) {
            Mojo::IOLoop->remove($loop);
          }
          return;
        }
        my $notes         = $job->info->{notes} // {};
        my $pairs_by_type = $app->get_general_pairs();
        foreach my $key (keys $notes->%*) {
          if ($key eq 'pairs_by_type') {
            $pairs_by_type = $notes->{$key};
            $logger->DEBUG('detected pairs_by_type update: '
                . Data::Printer::np($pairs_by_type, multiline => 0));
            $app->plugins->emit(pairs_by_type => $pairs_by_type);
          }
        }
        if ($job->info->{state} eq 'finished') {
          $app->plugins->emit(pairs_complete => $pairs_by_type);
          Mojo::IOLoop->remove($loop);
        }
      }
    );
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

    my $monitorJid = $self->app->minion->enqueue(
      monitor_pair_builders => [{}] => {
        priority => 90,
        attempts => 5,
        delay    => 15,
        expire   => 7200,
      }
    );

    my $loop;
    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        # Check for any successful completions
        my $completed_pairs = $self->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['finished']
        })->total;

        # Check for any still running
        my $active_pairs = $self->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['active', 'inactive']
        })->total;

        # Check for any failures (optional - decide if you want to handle this)
        my $failed_pairs = $self->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['failed']
        })->total;

        # none have been kicked off, do so.
        my $enqueueBuilder = 0;
        if ($completed_pairs == 0 && $active_pairs == 0 && $failed_pairs == 0) {
          $enqueueBuilder = 1;
        }
        elsif ($completed_pairs == 0 && $active_pairs == 0) {
          # one failed, check how many times its retried
          my $stillRetrying = 0;
          $self->app->minion->jobs({
            tasks  => ['build_all_pairs'],
            states => ['failed']
          })->each(sub {
            my $info = $_;
            if ($info->{retried} <= $info->{attempts}) {
              $stillRetrying = 1;
            }
          });
          if ($stillRetrying == 0) {
            $enqueueBuilder = 1;
          }
        }
        if ($enqueueBuilder) {
          $logger->INFO('enqueing new build_all_pairs');
          $self->app->minion->enqueue(
            build_all_pairs => [{}] => {
              priority => 50,
              attempts => 5,
              expire   => 720,
            }
          );
        }
        my $monitorJob = $self->app->minion->job($monitorJid);
        unless ($monitorJob) {
          $logger->ERR("No job associated with monitorJid $monitorJid");
          Mojo::IOLoop->remove($loop);
          $self->finish("No job associated with monitorJid $monitorJid");
        }
        if ($monitorJob->info->{state} eq 'failed') {
          $logger->ERR("Monitor job failed: $monitorJob->info->{result}");
          Mojo::IOLoop->remove($loop);
          $self->finish("Monitor job failed: $monitorJob->info->{result}");
        }
        if ($monitorJob->info->{state} eq 'finished') {
          $self->note(pairs_by_type =>
              ($monitorJob->info->{notes}->{pairs_by_type} // {}));
          if ($monitorJob->info->{result} eq 'all pair builders complete') {
            if ($completed_pairs == 0 && $active_pairs) {
              $logger->INFO('All Monitored Jobs Complete');
              $self->note(complete => 'All Monitored Jobs Complete');
              Mojo::IOLoop->remove($loop);
              $self->finish('All Monitored Jobs Complete');
            }
          }
        }
      }
    );

    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}
1;
__END__

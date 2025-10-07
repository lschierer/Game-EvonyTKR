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
          $self->startPrebuild($app);
        }
      }
    });
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

  }

  sub run ($self, @args) {
    $logger->DEBUG(sprintf('%s run method starting',            __PACKAGE__));
    $logger->DEBUG(sprintf('%s run method escaped guard check', __PACKAGE__));

    #wait to ensure the workers have the new tasks
    sleep 15;
    my $mpb = $self->app->minion->enqueue(monitor_pair_builders => [{}] => {
      priority  => 90,
      attempts  => 5,
      delay     => 15,
      expire    => 7200,
    });
    my $bpj = $self->app->minion->enqueue(build_all_pairs => [{ }] => {
      priority  => 50,
      attempts  => 5,
      expire    => 720,
    });
    my $loop;
    my $failure = 0;
    my $monitored = [$mpb, $bpj];

    $loop = Mojo::IOLoop->recurring(10 => sub {
      my $jobs = $self->app->minion->jobs({ids => $monitored });
      if($jobs->total == 0){
        my @tn = keys $self->info->{notes}->%*;
        my $incomplete =  1;
        if(scalar @tn >= 2){
          $incomplete = 0;
          foreach my $note (values $self->info->{notes}->%* ){
            if($note =~ /still pending/){
              $incomplete = 1;
            }
          }
        }
        if($incomplete != 0){
          Mojo::IOLoop->remove($loop);
          $self->finish('All Monitored Steps Complete.');
        }
      }
      my @stillrunning;
      while(my $info = $jobs->next ) {
        if($info->{state} eq 'failed'){
          $failure = $info->{result};
          Mojo::IOLoop->remove($loop);
          last;
        }
        if($info->{state} eq 'finished'){
          $logger->DEBUG(sprintf('detected completion of task %s', $info->{task}));
          $self->note($info->{task} => sprintf('%s completed', $info->{id}));
          next;
        }
        $self->note($info->{task} => sprintf('%s still pending', $info->{id}));
      }
    });
    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}
1;
__END__

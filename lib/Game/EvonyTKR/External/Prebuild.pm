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
require Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders;
require Game::EvonyTKR::External::General::Pair::CreatePairs;
require Game::EvonyTKR::External::General::Pair::MonitorCreatePairs2;
require Game::EvonyTKR::External::MonitorLoaders;
require Game::EvonyTKR::External::General::Loader;
require Game::EvonyTKR::External::General::LoadAll;
require Game::EvonyTKR::External::Book::Loader;
require Game::EvonyTKR::External::Book::LoadAllBuiltins;
require Game::EvonyTKR::External::Book::LoadAllGenerics;
require Game::EvonyTKR::External::Specialty::Loader;
require Game::EvonyTKR::External::Specialty::LoadAllSpecialties;

package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Home;
  use Mojo::File;
  use POSIX 'strftime';
  use Time::HiRes 'time';
  use experimental qw(class);
  use Carp;

  state $OnlyOnePrebuild = 0;

  state $generalCache;
  state $prereqs = {};
  state $monitors = {};

  sub register ($plugin, $app, $conf = {}) {
    if (not defined $plugin) {
      say '$plugin ont defined in register for ' . __PACKAGE__ . $$;
      return;
    }
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__ . $$;
      say $errmessage;
      return;
    }
    $plugin->SUPER::register($app, $conf);

    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $plugin->logger->error($errmessage);
      say $errmessage;
      return;
    }
    $plugin->logger->debug(
      sprintf('register function for "%s" %s', __PACKAGE__, $$));

    # Register main prebuild orchestration task
    $app->minion->add_task(external_prebuild => __PACKAGE__);
    my $plugins = [
      'Game::EvonyTKR::External::Book::LoadAllBuiltins',
      'Game::EvonyTKR::External::Book::LoadAllGenerics',
      'Game::EvonyTKR::External::Book::Loader',
      'Game::EvonyTKR::External::General::LoadAll',
      'Game::EvonyTKR::External::General::Loader',
      'Game::EvonyTKR::External::General::Pair::CreatePairs',
      'Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders',
      'Game::EvonyTKR::External::General::Pair::MonitorCreatePairs2',
      'Game::EvonyTKR::External::MonitorLoaders',
      'Game::EvonyTKR::External::Specialty::LoadAllSpecialties',
      'Game::EvonyTKR::External::Specialty::Loader',
    ];

    my @tasks = values $app->minion->tasks->%*;
    foreach my $task (@tasks) {
      $plugin->logger->debug(sprintf('task is %s, %s',
        ref($task) // 'undef ref',
        blessed($task) // 'undef blessed'));
    }
    foreach my $prereq ($plugins->@*) {
      $prereqs->{$prereq} = 0;
      my $signal = $prereq =~ s/::/_/gr;
      $app->plugins->on(
        $signal => sub {
          $plugin->logger->info(sprintf('detected %s ready', $prereq));
          return $plugin->prebuildPrerequisites({ $prereq => 1 });
        }
      );
      eval { $app->plugin($prereq); } or do {
        $plugin->logger->error("Error loading task plugin $prereq: $@");
      };

    }

    $plugin->prebuild_init($app);

    $plugin->logger->info(
      sprintf('%s register function complete for %s', __PACKAGE__, $$));
  }

  sub prebuild_init ($plugin, $app) {
    # wait for prerequisites...
    if (!$plugin->prebuildPrerequisites) {
      Mojo::IOLoop->timer( 5 => sub {
        $plugin->prebuild_init($app);
      });
    }

    if (my $g =
      $app->minion->guard('external_prebuild:bootstrap', 15, { limit => 1 })) {
      # only the guard holder gets here
      my $existing = $app->minion->jobs({
        tasks  => ['external_prebuild'],
        states => [qw(inactive delayed active finished)]
      })->total;

      unless ($existing) {
        my $jid = $app->minion->enqueue(
          'external_prebuild' => [{}] => {
            priority => 100,
            attempts => 3,
            expire   => 7200,
            notes    => { uniq => 'external_prebuild' },
          }
        );
        $plugin->logger->info("Queued external_prebuild $jid");
      }
    }
  }

  sub prebuildPrerequisites ($plugin, $args = {}) {
    foreach my $key (keys $args->%*) {
      $prereqs->{$key} = $args->{$key};
    }

    if (List::AllUtils::none { $_ == 0 } values $prereqs->%*) {
      return 1;
    }
    $plugin->logger->debug(
      sprintf('failed prebuildPrerequisites: %s',
        Data::Printer::np($prereqs, multiline => 0))
    );
    return 0;
  }

  # Main prebuild orchestration job
  sub run ($job, @args) {
    if (not defined($job)) {
      say '$job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    else {
      $job->logger->debug(sprintf(
        'minion in %s is a %s;%s',
        __PACKAGE__, ref($job->minion), blessed($job->minion)
      ));
    }
    $job->logger->debug('Prebuild orchestration starting');

    my $owner         = $$ . '@' . ($ENV{HOSTNAME} // 'localhost');
    my $job_key       = 'prebuild_run';
    my $ttl           = 60;                                 # lock TTL (seconds)
    my $refresh_every = 20;                                 # heartbeat interval
    my $db            = $job->minion->backend->sqlite->db;
    my $pair_monitor_jid;

    unless ($job->app->try_acquire_lock_sqlite($db, $job_key, $owner, $ttl)) {
      $job->note(skipped => 'another prebuild is running');
      return $job->finish('skipped');
    }

    unless ($job->prebuildPrerequisites) {
      $job->logger->debug(sprintf('cannot start prebuild; prereqs: %s',
        Data::Printer::np($prereqs, multiline => 0)));
      return $job->retry({delay => $refresh_every});
    }


    my $timer_id;
    Mojo::IOLoop->timer(1 => sub{
      $job->prebuild_db_lock($timer_id);
    });
    Mojo::IOLoop->timer(1 => sub{
      $job->monitor_prebuild($timer_id);
    });

    my $loaderJobDefs = {
      load_all_generic_books => {
        args     => [],
        attempts => 3,
        delay    => 1,
        expire   => 300,
        priority => 50,
      },
      load_all_builtin_books => {
        args     => [],
        attempts => 3,
        delay    => 1,
        expire   => 300,
        priority => 60,
      },
      load_all_specialties => {
        args     => [],
        attempts => 3,
        delay    => 1,
        expire   => 300,
        priority => 60,
      },
      load_all_generals => {
        args     => ['prebuild load_all_generals'],
        attempts => 3,
        delay    => 5,
        expire   => 7200,
        priority => 10,
      },
      load_all_pair_builders => {
        args     => [],
        attempts => 5,
        delay    => 6,
        expire   => 7200,
        priority => 50,
      },
    };

    $job->logger->info('launching jobs to spawn loaders.');

    my @loaderJids;
    foreach my $jobname (keys $loaderJobDefs->%*) {
      my $args   = $loaderJobDefs->{$jobname}->{args} // [];
      my $params = $loaderJobDefs->{$jobname}         // {};
      delete($params->{args}) if (exists $params->{args});

      my $jid =
        $job->minion->enqueue($jobname => [$args->@*] => { $params->%* });
      if (defined($jid)) {
        $job->note($jobname => $jid);
        $job->logger->debug(sprintf(
          'launched %s with jid %s', $jobname, $jid));
        push @loaderJids, $jid;
      }
      else {
        my $errmessage = sprintf('failed to launch %s', $jobname);
        $job->logger->error($errmessage);
        return $job->fail($errmessage);
      }
    }

    if (scalar(@loaderJids) == keys($loaderJobDefs->%*)) {
      $job->logger->info('all job spawners launched');
    }
    else {
      my $errmessage = sprintf(
        'launched %s spawners, expected %s. ',
        scalar(@loaderJids), keys($loaderJobDefs->%*)
      );
      $job->logger->error($errmessage);
      $job->fail($errmessage);
    }

    while(List::AllUtils::any {$_ ne 'finished'} values $monitors->%*){
      sleep 5;
    }
    $job->finish('Prebuild Complete');
  }

  sub prebuild_db_lock($job, $timer_id){
    $timer_id = Mojo::IOLoop->recurring(
      $refresh_every => sub {
        $job->logger->info('prebuild db lock loop');
        $job->app->refresh_lock_sqlite($db, $job_key, $owner, $ttl) or do {
          $job->logger->error('Lost runtime lock; stopping prebuild.');
          Mojo::IOLoop->remove($timer_id) if $timer_id;
          # safe even if we don’t own it
          $job->app->release_lock_sqlite($db, $job_key, $owner);
          $job->fail('lost lock');
        };
      }
    );
    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }

  sub monitor_prebuild($job, $timer_id) {
    my $monitor_names = [];
    @{$monitor_names} = grep {$_ =~ /monitor/ } keys $job->minion->tasks->%*;

    my $monitorTimer;
    $monitorTimer = Mojo::IOLoop->recurring(5 => sub{

      my @loaderJids;
      $job->minion->jobs({
        tasks   => [$monitor_names->@*],
      })->each(sub {
        my $info = $_;
        push @loaderJids, $info->{id};
      });

      unless(scalar(@loaderJids) > 0){
        Mojo::IOLoop->timer(1 => sub{
          $job->monitor_prebuild($timer_id);
        });
      }

      foreach my $mn ($monitor_names->@*){
        my $monitor_args = [];
        push @{ $monitor_args }, \@loaderJids;

        unless($job->minion->jobs({
          tasks   => [$mn],
          states  => ['active','inactive','finished'],
        })->total > 0){
          my $mj = $job->minion->enqueue($mn => [ $monitor_args->@* ] => {
            attempts => 5,
            delay    => 10,
            expire   => 200,
            priority => 90,
          });
          $monitors->{$mn} = $mj;
        }

        $job->minion->jobs({
          tasks   => [$mn],
          states  => ['active','inactive','finished'],
        })->each(sub{
          my $info = $_;
          $job->logger->info(sprintf('found %s with jid %s', $mn, $info->{id}));
          if($info->{state} eq 'failed'){
            Mojo::IOLoop->remove($monitorTimer);
            Mojo::IOLoop->remove($timer_id);
            my $errmessage = sprintf('monitor %s failed: %s', $info->{id}, $info->{result});
            $job->logger->error($errmessage);
            return $job->fail($errmessage);
          }
          if($info->{state} eq 'finished'){
            $job->logger->info(sprintf('monitor %s finished: %s', $info->{id}, ($info->{result} // 'no result')));
            $monitors->{$mn} = 'finished';
          }
        });
      }

      if(List::AllUtils::all {$_ eq 'finished'} values $monitors->%*){
        $job->logger->info('All Monitors Finished');
        Mojo::IOLoop->remove($monitorTimer);
        Mojo::IOLoop->remove($timer_id);
        return $job->finish('All Monitors Finished');
      }
    });
    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}

1;

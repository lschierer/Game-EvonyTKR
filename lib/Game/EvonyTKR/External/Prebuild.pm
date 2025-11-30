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
require Mojo::Loader;

package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Home;
  use Mojo::File;
  use Mojo::Loader;

  use POSIX 'strftime';
  use Time::HiRes 'time';
  use experimental   qw(class);
  use List::AllUtils qw(any all none uniq);
  use Carp;

  state $OnlyOnePrebuild = 0;

  state $generalCache;
  state $prereqs  = {};
  state $monitors = {};

  my $prereq_plugins = [
    'Game::EvonyTKR::External::AscendingAttributes::Loader',
    'Game::EvonyTKR::External::AscendingAttributes::LoadAll',
    'Game::EvonyTKR::External::Book::LoadAllBuiltins',
    'Game::EvonyTKR::External::Book::LoadAllGenerics',
    'Game::EvonyTKR::External::Book::Loader',
    'Game::EvonyTKR::External::General::LoadAll',
    'Game::EvonyTKR::External::General::Loader',
    'Game::EvonyTKR::External::General::BuildIndexes',
    'Game::EvonyTKR::External::General::Pair::CreatePairs',
    'Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders',
    'Game::EvonyTKR::External::General::Pair::ReduceCoordinator',
    'Game::EvonyTKR::External::General::Pair::ReduceBatch',
    'Game::EvonyTKR::External::MonitorLoaders',
    'Game::EvonyTKR::External::Specialty::LoadAllSpecialties',
    'Game::EvonyTKR::External::Specialty::Loader',
    'Game::EvonyTKR::External::Covenant::LoadAll',
    'Game::EvonyTKR::External::Covenant::Loader',
  ];

  sub task_name {'external_prebuild'}

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
    $app->minion->add_task($plugin->task_name => __PACKAGE__);

    my @tasks = values $app->minion->tasks->%*;
    foreach my $task (@tasks) {
      $plugin->logger->debug(sprintf('task is %s, %s',
        ref($task) // 'undef ref',
        blessed($task) // 'undef blessed'));
    }
    foreach my $prereq ($prereq_plugins->@*) {
      $prereqs->{$prereq} = 0;
      if (my $e = Mojo::Loader::load_class($prereq)) {
        $plugin->logger->logcroak(sprintf(
          'exception loading %s: ', ref($e) ? $e : 'Not Found!'));
        next;
      }
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
      Mojo::IOLoop->timer(
        5 => sub {
          $plugin->prebuild_init($app);
        }
      );
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

    if (none { $_ == 0 } values $prereqs->%*) {
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
      return $job->retry({ delay => $refresh_every });
    }
    my $timer_id;
    Mojo::IOLoop->timer(
      0.01 => sub {
        $job->logger->debug("prebuild obtaining db lock");
        $job->prebuild_db_lock($timer_id);
      }
    );

    my $loaderJobDefs = {
      load_all_ascending_attributes => {
        args     => [],
        attempts => 3,
        delay    => 1,
        priority => 50,
      },
      load_all_generic_books => {
        args     => [],
        attempts => 3,
        delay    => 1,
        priority => 50,
      },
      load_all_builtin_books => {
        args     => [],
        attempts => 3,
        delay    => 1,
        priority => 60,
      },
      load_all_specialties => {
        args     => [],
        attempts => 3,
        delay    => 1,
        priority => 60,
      },
      load_all_generals => {
        args     => ['prebuild load_all_generals'],
        attempts => 3,
        delay    => 5,
        priority => 10,
      },
      load_all_covenants => {
        args     => [],
        attempts => 3,
        delay    => 5,
        priority => 10,
      },
      load_all_pair_builders => {
        args     => [],
        attempts => 5,
        delay    => 6,
        priority => 50,
      },
    };

    $job->logger->info('launching jobs to spawn loaders.');

    my $loaderJids = [];
    foreach my $jobname (sort keys $loaderJobDefs->%*) {
      my $args   = $loaderJobDefs->{$jobname}->{args} // [];
      my $params = $loaderJobDefs->{$jobname}         // {};
      delete($params->{args}) if (exists $params->{args});

      my $jid =
        $job->minion->enqueue($jobname => [$args->@*] => { $params->%* });
      if (defined($jid)) {
        $job->note($jobname => $jid);
        $job->logger->debug(sprintf('launched %s with jid %s', $jobname, $jid));
        push @{$loaderJids}, $jid;
      }
      else {
        my $errmessage = sprintf('failed to launch %s', $jobname);
        $job->logger->error($errmessage);
        Mojo::IOLoop->remove($timer_id) if ($timer_id);
        return $job->fail($errmessage);
      }
    }

    if (scalar(@$loaderJids) == keys($loaderJobDefs->%*)) {
      $job->logger->info('all job spawners launched');
    }
    else {
      my $errmessage = sprintf('launched %s spawners, expected %s. ',
        scalar(@$loaderJids), keys($loaderJobDefs->%*));
      $job->logger->error($errmessage);
      Mojo::IOLoop->remove($timer_id) if ($timer_id);
      return $job->fail($errmessage);
    }

    my $monitor_names = [
      sort grep { $_ =~ /(?:monitor|coordinator)/i }
        keys $job->minion->tasks->%*
    ];

    foreach my $mn ($monitor_names->@*) {

      unless (
        $job->minion->jobs({
          tasks  => [$mn],
          states => ['active', 'inactive', 'finished'],
        })->total > 0
      ) {
        my $mj = $job->minion->enqueue(
          $mn => [] => {
            attempts => 5,
            delay    => 10,
            priority => 90,
            parents  => $loaderJids
          }
        );
        $monitors->{$mn} = $mj;
      }
    }

    Mojo::IOLoop->remove($timer_id) if ($timer_id);
    $job->finish('Prebuild spawning complete');
  }

  sub cleanup ($job) {
    my $prebuild_start = time;

    my @tasks;
    foreach my $prereq ($prereq_plugins->@*) {
      unless ($prereq->can('task_name')) {
        $job->logger->error(
          sprintf('prereq plugin "%s" is missing the task_name method.',
            $prereq)
        );
        next;
      }
      push @tasks, $prereq->task_name();
    }

    $job->minion->jobs({
      tasks  => \@tasks,
      states => ['finished', 'failed'],
    })->each(sub {
      my $j = $_;
      if ($j->info->{finished} && $j->info->{finished} < $prebuild_start) {
        $j->remove;
      }
      elsif ($j->info->{failed}) {
        $j->remove;
      }
    });
  }

  sub prebuild_db_lock($job, $db, $job_key, $owner, $ttl, $timer_id,
    $refresh_every) {
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

}

1;

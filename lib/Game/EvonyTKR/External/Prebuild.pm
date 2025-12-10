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
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
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
    'Game::EvonyTKR::External::Glossary::LoadAll',
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
    return 1 unless $plugin->SUPER::register($app, $conf);

    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $plugin->log_error($errmessage);
      say $errmessage;
      return;
    }
    $plugin->log_debug(
      sprintf('register function for "%s" %s', __PACKAGE__, $$));

    # Register main prebuild orchestration task
    $app->minion->add_task($plugin->task_name => __PACKAGE__);

    my @tasks = values $app->minion->tasks->%*;
    foreach my $task (@tasks) {
      $plugin->log_debug(sprintf('task is %s, %s',
        ref($task) // 'undef ref',
        blessed($task) // 'undef blessed'));
    }
    foreach my $prereq ($prereq_plugins->@*) {
      # By this point, parent _init_minion() has already loaded all External modules
      # So we're just checking if their tasks were successfully registered

      # Get the task name from the prerequisite class
      my $task_name = eval { $prereq->task_name };
      if ($@) {
        my $errmsg = sprintf(
          'Failed to get task_name from %s: %s', $prereq, $@);
        $plugin->log_error($errmsg);
        $prereqs->{$prereq} = 0;
        next;
      }

      # Check if the task is registered in Minion
      # If not, the prerequisite's register() failed
      if ($app->minion->tasks->{$task_name}) {
        $plugin->log_debug(sprintf(
          'prereq %s has task "%s" registered', $prereq, $task_name));
        $prereqs->{$prereq} = 1;
      } else {
        my $errmsg = sprintf(
          'PREREQUISITE FAILED: %s task "%s" was not registered in Minion. ' .
          'This means its register() method failed or did not call add_task()',
          $prereq, $task_name);
        $plugin->log_error($errmsg);
        $prereqs->{$prereq} = 0;
      }

    }

    $plugin->prebuild_init($app);

    $plugin->log_info(
      sprintf('%s register function complete for %s', __PACKAGE__, $$));
    return 1;
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
        $plugin->log_info("Queued external_prebuild $jid");
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
    $plugin->log_debug(
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
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    else {
      $job->log_debug(sprintf(
        'minion in %s is a %s;%s',
        __PACKAGE__, ref($job->minion), blessed($job->minion)
      ));
    }
    $job->log_debug('Prebuild orchestration starting');

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
      $job->log_debug(sprintf('cannot start prebuild; prereqs: %s',
        Data::Printer::np($prereqs, multiline => 0)));
      return $job->retry({ delay => $refresh_every });
    }
    my $timer_id;
    Mojo::IOLoop->timer(
      0.01 => sub {
        $job->log_debug("prebuild obtaining db lock");
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
      load_all_glossary_terms => {
        args     => [],
        attempts => 3,
        delay    => 1,
        priority => 60,
      },
      load_ml_conflicts => {
        args     => [],
        attempts => 3,
        delay    => 5,
        priority => 15,
      },
      load_all_pair_builders => {
        args     => [],
        attempts => 5,
        delay    => 6,
        priority => 50,
      },
    };

    $job->log_info('launching jobs to spawn loaders.');

    my $loaderJids = [];
    foreach my $jobname (sort keys $loaderJobDefs->%*) {
      my $args   = $loaderJobDefs->{$jobname}->{args} // [];
      my $params = $loaderJobDefs->{$jobname}         // {};
      delete($params->{args}) if (exists $params->{args});

      my $jid =
        $job->minion->enqueue($jobname => [$args->@*] => { $params->%* });
      if (defined($jid)) {
        $job->note($jobname => $jid);
        $job->log_debug(sprintf('launched %s with jid %s', $jobname, $jid));
        push @{$loaderJids}, $jid;
      }
      else {
        my $errmessage = sprintf('failed to launch %s', $jobname);
        $job->log_error($errmessage);
        Mojo::IOLoop->remove($timer_id) if ($timer_id);
        return $job->fail($errmessage);
      }
    }

    if (scalar(@$loaderJids) == keys($loaderJobDefs->%*)) {
      $job->log_info('all job spawners launched');
    }
    else {
      my $errmessage = sprintf('launched %s spawners, expected %s. ',
        scalar(@$loaderJids), keys($loaderJobDefs->%*));
      $job->log_error($errmessage);
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
        $job->log_error(
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
        $job->log_info('prebuild db lock loop');
        $job->app->refresh_lock_sqlite($db, $job_key, $owner, $ttl) or do {
          $job->log_error('Lost runtime lock; stopping prebuild.');
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

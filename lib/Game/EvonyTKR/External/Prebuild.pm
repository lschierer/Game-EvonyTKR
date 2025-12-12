use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;

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
  use diagnostics;

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

  sub register ($taskClass, $app, $conf = {}) {
    if (not defined $taskClass) {
      say '$taskClass ont defined in register for ' . __PACKAGE__ . $$;
      return;
    }
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__ . $$;
      say $errmessage;
      return;
    }
    return 1 unless $taskClass->SUPER::register($app, $conf);

    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $taskClass->log_error($errmessage);
      say $errmessage;
      return;
    }
    $taskClass->log_debug(
      sprintf('register function for "%s" %s', __PACKAGE__, $$));

    # Register main prebuild orchestration task
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);

    $taskClass->log_info(
      sprintf('%s register function complete for %s', __PACKAGE__, $$));
    return 1;
  }



  sub prebuildPrerequisites ($job, $args = {}) {

    my @loaded_plugins = sort values $job->minion->tasks->%*;
    $job->log_debug(
      sprintf('there are %s tasks in minion', scalar(@loaded_plugins)));
    if (scalar(keys($args->%*)) == 0) {
      foreach my $prereq_plugin ($prereq_plugins->@*) {
        if (any { $_ eq $prereq_plugin } @loaded_plugins) {
          $job->log_debug(sprintf('prereq %s is registered', $prereq_plugin,));
          $prereqs->{$prereq_plugin} = 1;
        }
        else {
          my $errmessage = sprintf('module "%s" unavailable', $prereq_plugin,);
          print STDERR $errmessage;
          $job->log_error($errmessage);
          $prereqs->{$prereq_plugin} = 0;
          return $job->fail($errmessage);
        }
      }
    }

    foreach my $key (keys $args->%*) {
      $prereqs->{$key} = $args->{$key};
    }

    if (none { $_ == 0 } values $prereqs->%*) {
      return 1;
    }
    $job->log_debug(
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

    # Check prerequisites before proceeding
    unless ($job->prebuildPrerequisites) {
      $job->log_debug(sprintf('Cannot start prebuild; prereqs: %s',
        Data::Printer::np($prereqs, multiline => 0)));
      return $job->retry({ delay => 10 });
    }

    # Check if we should force a rebuild regardless of data state
    my $force_reload = $ENV{FORCE_DATA_RELOAD} || 0;
    if ($force_reload) {
      $job->log_info(
        "FORCE_DATA_RELOAD set - rebuilding data regardless of current state");
    }

    # Get current version from config (git-commit)
    my $current_version =
      eval { $job->app->config->{version}{'git-commit'} } // 'unknown';
    $job->log_info("Current git-commit: $current_version");

    # Check if data is current for this version
    unless ($force_reload) {
      my $data_current = 0;
      eval {
        my $stored_version = $job->persistence->get_data_version;

        if ($stored_version && $stored_version eq $current_version) {
          # Version matches - verify data actually exists
          my $generals_count    = $job->persistence->count_generals      // 0;
          my $specialties_count = $job->persistence->count_specialties   // 0;
          my $books_count       = $job->persistence->count_builtin_books // 0;

          if ($generals_count > 0 && $specialties_count > 0 && $books_count > 0)
          {
            $data_current = 1;
            $job->log_info(sprintf(
"Data current for version %s (generals=%d, specialties=%d, books=%d)",
              $current_version,   $generals_count,
              $specialties_count, $books_count
            ));
          }
          else {
            $job->log_warn(sprintf(
"Version matches but data incomplete: generals=%d, specialties=%d, books=%d",
              $generals_count, $specialties_count, $books_count
            ));
          }
        }
        elsif ($stored_version) {
          $job->log_info(
"Data version mismatch: stored=$stored_version, current=$current_version - will reload"
          );
        }
        else {
          $job->log_info("No stored data version - first run or data cleared");
        }
      };

      if ($@) {
        $job->log_debug("Error checking data version: $@");
        $data_current = 0;
      }

      if ($data_current) {
        return $job->finish("Data current for version $current_version");
      }
    }

    my $owner         = $$ . '@' . ($ENV{HOSTNAME} // 'localhost');
    my $job_key       = 'prebuild_run';
    my $ttl           = 30;    # reduced TTL (seconds)
    my $refresh_every = 10;    # heartbeat interval
    my $pair_monitor_jid;

    # Use Minion's built-in job uniqueness instead of custom SQLite locking

    my $timer_id;
    # Remove custom locking - Minion handles job uniqueness

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

    # Create unique run identifier for this prebuild
    my $run_id = sprintf('%s-%s', $$, $job->id);
    $job->prebuild_run_id($run_id);
    $job->log_info("Prebuild run ID: $run_id");

    # Harvest ALL jobs from previous prebuild runs (assume previous prebuild crashed)
    my $harvested = $job->harvest_tagged_jobs();
    $job->log_info("Harvested $harvested jobs from previous runs");

    my $loaderJids = [];
    my $totalJobs = 0;  # Count both launched and existing jobs
    foreach my $jobname (sort keys $loaderJobDefs->%*) {
      # Simple check: if any active/inactive jobs exist for this task, skip it
      my $existing = $job->minion->jobs({
        tasks  => [$jobname],
        states => [qw(inactive active)]
      })->total;

      if ($existing > 0) {
        $job->log_info("Skipping $jobname - $existing jobs already exist");
        $totalJobs++;  # Count existing jobs
        next;
      }
      $job->log_debug("Prebuild needs to launch $jobname");

      my $args   = $loaderJobDefs->{$jobname}->{args} // [];
      my $params = $loaderJobDefs->{$jobname}         // {};
      delete($params->{args}) if (exists $params->{args});

      my $jid =
        $job->minion->enqueue($jobname => [$args->@*] => {
          $params->%*,
          notes => { prebuild_run_id => $run_id }
        });
      if (defined($jid)) {
        $job->note($jobname => $jid);
        $job->log_debug(sprintf('launched %s with jid %s', $jobname, $jid));
        push @{$loaderJids}, $jid;
        $totalJobs++;  # Count newly launched jobs
      }
      else {
        my $errmessage = sprintf('failed to launch %s', $jobname);
        $job->log_error($errmessage);
        return $job->fail($errmessage);
      }
    }

    if ($totalJobs == keys($loaderJobDefs->%*)) {
      $job->log_info('all job spawners launched or already exist');
    }
    else {
      my $errmessage = sprintf('launched %s spawners, expected %s. ',
        $totalJobs, scalar(keys($loaderJobDefs->%*)));
      $job->log_error($errmessage);
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
            notes    => {
              prebuild_jid => $job->id,
              prebuild_run_id => $job->prebuild_run_id,
            }  # Reference instead of parent
          }
        );
        if (defined $mj) {
          $monitors->{$mn} = $mj;
          $job->log_debug("Launched monitor job $mn with jid $mj");
        } else {
          $job->log_warn("Failed to launch monitor job $mn");
        }
      }
    }

    # Store current version to persistence after successful load
    eval {
      my $current_version = $job->app->config->{version}{'git-commit'}
        // 'unknown';
      $job->persistence->set_data_version($current_version);
      $job->log_info("Stored data version: $current_version");
    };
    if ($@) {
      $job->log_warn("Failed to store data version: $@");
    }

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

}

1;


package Game::EvonyTKR::External::Prebuild;
use v5.40;
use experimental qw(class);
use utf8::all;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
require Data::Printer;
require File::Share;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR;
require MIME::Base64;
require Path::Tiny;
use List::AllUtils qw(any all none uniq);
use Mojo::Home;
use Mojo::Loader;
use POSIX 'strftime';
use Time::HiRes 'time';
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

sub _build_loader_job_defs ($self, $stored_version, $current_version) {
  return {
    load_all_ascending_attributes => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 1,
      priority => 50,
    },
    load_all_generic_books => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 1,
      priority => 50,
    },
    load_all_builtin_books => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 1,
      priority => 60,
    },
    load_all_specialties => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 1,
      priority => 60,
    },
    load_all_generals => {
      args => ['prebuild load_all_generals', $stored_version, $current_version],
      attempts => 3,
      delay    => 5,
      priority => 10,
    },
    load_all_covenants => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 5,
      priority => 10,
    },
    load_all_glossary_terms => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 1,
      priority => 60,
    },
    load_ml_conflicts => {
      args     => [$stored_version, $current_version],
      attempts => 3,
      delay    => 5,
      priority => 15,
    },
    load_all_pair_builders => {
      args     => [$stored_version, $current_version],
      attempts => 5,
      delay    => 6,
      priority => 50,
    },
    reduce_coordinator => {
      args     => [],
      attempts => 5,
      delay    => 16,
      priority => 30,
    },
  };
}

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
  $taskClass->SUPER::register($app, $conf);

  unless (defined($app->minion)) {
    my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
    $taskClass->logger->error($errmessage);
    say $errmessage;
    return;
  }
  $taskClass->logger->debug(
    sprintf('register function for "%s" %s', __PACKAGE__, $$));

  # Register main prebuild orchestration task
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);

  $taskClass->logger->info(
    sprintf('%s register function complete for %s', __PACKAGE__, $$));
  return 1;
}

sub prebuildPrerequisites ($job, $args = {}) {

  my @loaded_plugins = sort values $job->minion->tasks->%*;
  $job->logger->debug(
    sprintf('there are %s tasks in minion', scalar(@loaded_plugins)));
  if (scalar(keys($args->%*)) == 0) {
    foreach my $prereq_plugin ($prereq_plugins->@*) {
      if (any { $_ eq $prereq_plugin } @loaded_plugins) {
        $job->logger->debug(sprintf('prereq %s is registered', $prereq_plugin,));
        $prereqs->{$prereq_plugin} = 1;
      }
      else {
        my $errmessage = sprintf('module "%s" unavailable', $prereq_plugin,);
        print STDERR $errmessage;
        $job->logger->error($errmessage);
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
  $job->logger->debug(
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
  return $job->finish(sprintf(
    'pid %s job id %s is not the only prebuild', $$, $job->info->{id}))
    unless $job->minion->lock('prebuild_guard', 300);
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

  # Check prerequisites before proceeding
  unless ($job->prebuildPrerequisites) {
    $job->logger->debug(sprintf('Cannot start prebuild; prereqs: %s',
      Data::Printer::np($prereqs, multiline => 0)));
    return $job->retry({ delay => $job->standard_delay });
  }

  # Get current version from config (git-commit)
  my $current_version =
    eval { $job->app->config->{version}{'git-commit'} } // 'unknown';
  $job->logger->info("Current git-commit: $current_version");
  my $stored_version = 0;

  # Create unique run identifier for this prebuild
  my $run_id = sprintf('%s-%s', $$, $job->id);
  $job->prebuild_run_id($run_id);
  $job->logger->info("Prebuild run ID: $run_id");

  # harvest old stuff
  $job->cleanup();

  # Get loader job configurations
  my $loaderJobDefs =
    $job->_build_loader_job_defs($stored_version, $current_version);

  $job->logger->debug(
    sprintf('this prebuild includes jobs: %s',
      join(', ', keys %{$loaderJobDefs}),)
  );
  # Check if data is current for this version
  my $data_current = 0;
  eval {
    $stored_version = $job->persistence->get_data_version;

    if ($stored_version && $stored_version eq $current_version) {
      # Version matches - verify data actually exists
      my $generals_count    = $job->persistence->count_generals      // 0;
      my $specialties_count = $job->persistence->count_specialties   // 0;
      my $books_count       = $job->persistence->count_builtin_books // 0;

      if ($generals_count > 0 && $specialties_count > 0 && $books_count > 0) {
        $data_current = 1;
        $job->logger->info(sprintf(
          'Data current for version %s '
            . '(generals=%d, specialties=%d, books=%d)',
          $current_version,   $generals_count,
          $specialties_count, $books_count
        ));
      }
      else {
        $job->logger->warn(sprintf(
          'Version matches but data incomplete: '
            . 'generals=%d, specialties=%d, books=%d',
          $generals_count, $specialties_count, $books_count
        ));
      }
    }
    elsif ($stored_version) {
      $job->logger->info(sprintf(
        'Data version mismatch: stored=,%s current=%s - will reload',
        $stored_version, $stored_version
      ));
    }
    else {
      $job->logger->info("No stored data version - first run or data cleared");
    }
  };

  if ($@) {
    $job->logger->debug("Error checking data version: $@");
    $data_current = 0;
  }

  my $owner         = $$ . '@' . ($ENV{HOSTNAME} // 'localhost');
  my $job_key       = 'prebuild_run';
  my $ttl           = 30;    # reduced TTL (seconds)
  my $refresh_every = 10;    # heartbeat interval
  my $pair_monitor_jid;

  $job->logger->info('launching jobs to spawn loaders.');

  # Mark all work units as incomplete at start
  require Game::EvonyTKR::WorkUnit::Tracker;
  my $tracker =
    Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $job->persistence);

  my @work_units;
  foreach my $prereq ($prereq_plugins->@*) {
    unless ($prereq->can('task_name')) {
      $job->logger->error(
        sprintf('prereq plugin "%s" is missing the task_name method.', $prereq)
      );
      next;
    }
    if ($prereq->task_name =~ /load_all/) {
      $tracker->mark_incomplete($prereq->task_name);
    }
  }

  # Clear other work units that don't match the load_all pattern
  $tracker->mark_incomplete('load_ml_conflicts');

  # Store current run_id in persistence so controllers can find it
  $job->set_metadata(
    'current_prebuild_run_id',
    {
      run_id     => $run_id,
      started_at => time(),
    }
  );

  my $loaderJids = [];
  my $totalJobs  = 0;    # Count both launched and existing jobs
  foreach my $jobname (sort keys $loaderJobDefs->%*) {
    $job->logger->debug("Prebuild needs to launch $jobname");

    my $args   = $loaderJobDefs->{$jobname}->{args} // [];
    my $params = $loaderJobDefs->{$jobname}         // {};
    delete($params->{args}) if (exists $params->{args});

    my $parents = [];
    if ($jobname eq 'reduce_coordinator') {
      $job->minion->jobs({
        tasks => ['load_all_pair_builders'],
      })->each(sub {
        my $info = $_;
        if ($info->{notes}->{prebuild_run_id} eq $job->prebuild_run_id) {
          push @{$parents}, $info->{id};
        }
      });
      $params->{parents} = $parents;
    }

    my $jid = $job->minion->enqueue(
      $jobname => [$args->@*] => {
        $params->%*,
        notes => {
          prebuild_run_id => $run_id,
          prebuild_jid    => $job->id,
          parents         => $parents,
        },
      }
    );
    if (defined($jid)) {
      $job->note($jobname => $jid);
      $job->logger->debug(sprintf('launched %s with jid %s', $jobname, $jid));
      push @{$loaderJids}, $jid;
      $totalJobs++;    # Count newly launched jobs
    }
    else {
      my $errmessage = sprintf('failed to launch %s', $jobname);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
  }

  if ($totalJobs == keys($loaderJobDefs->%*)) {
    $job->logger->info('all job spawners launched or already exist');
  }
  else {
    my $errmessage = sprintf('launched %s spawners, expected %s. ',
      $totalJobs, scalar(keys($loaderJobDefs->%*)));
    $job->logger->error($errmessage);
    return $job->fail($errmessage);
  }

  # Spawn completion jobs for collection types

  my $collection_completions;
  foreach my $prereq ($prereq_plugins->@*) {
    unless ($prereq->can('task_name')) {
      $job->logger->error(
        sprintf('prereq plugin "%s" is missing the task_name method.', $prereq)
      );
      next;
    }
    if ($prereq->task_name =~ /load_all/) {
      # skip special cases.
      next if ($prereq->task_name =~ /(?:pair_builders|glossary_terms|reduce)/);
      my $completion_jid = $job->minion->enqueue(
        'mark_collection_complete' => [$prereq->task_name] => {
          attempts => 10,
          delay    => 30,
          priority => 5,
          expire   => 600,
          notes    => { prebuild_run_id => $run_id }
        }
      );
      $job->logger->debug(sprintf(
        'Spawned completion job for %s: %s',
        $prereq->task_name, $completion_jid
      ));
      $collection_completions->{ $prereq->task_name } = $completion_jid;
    }
  }
  $job->note(collection_completions => $collection_completions);

  # Simple completions (single job marks complete)
  # ml_conflicts and glossary_terms will mark themselves complete

  my $pairs_completion_jid = $job->minion->enqueue(
    'mark_pairs_complete' => [] => {
      attempts => 10,
      delay    => 60,
      priority => 1,
      notes    => { prebuild_run_id => $run_id }
    }
  );
  $job->logger->debug("Spawned pairs completion job: $pairs_completion_jid");

  my $monitor_names = [
    sort grep { $_ =~ /(?:monitor)/i }
      keys $job->minion->tasks->%*
  ];

  foreach my $mn ($monitor_names->@*) {

    my $mj = $job->minion->enqueue(
      $mn => [] => {
        attempts => 5,
        delay    => 10,
        priority => 90,
        notes    => {
          prebuild_jid    => $job->id,
          prebuild_run_id => $job->prebuild_run_id,
        }    # Reference instead of parent
      }
    );
    if (defined $mj) {
      $monitors->{$mn} = $mj;
      $job->logger->debug("Launched monitor job $mn with jid $mj");
    }
    else {
      $job->logger->warn("Failed to launch monitor job $mn");
    }
  }

  # Store current version to persistence after successful load
  eval {
    my $current_version = $job->app->config->{version}{'git-commit'}
      // 'unknown';
    $job->persistence->set_data_version($current_version);
    $job->logger->info("Stored data version: $current_version");
  };
  if ($@) {
    $job->logger->warn("Failed to store data version: $@");
  }

  $job->finish('Prebuild spawning complete');
}

sub cleanup ($job) {
  my $prebuild_start = time;

  # Harvest ALL jobs from previous prebuild runs
  # (assume previous prebuild crashed)
  my $harvested = $job->harvest_tagged_jobs() // 0;
  $job->logger->info("Harvested $harvested jobs from previous runs");

  my @tasks;
  foreach my $prereq ($prereq_plugins->@*) {
    unless ($prereq->can('task_name')) {
      $job->logger->error(
        sprintf('prereq plugin "%s" is missing the task_name method.', $prereq)
      );
      next;
    }
    push @tasks, $prereq->task_name();
  }
  push @tasks, 'compute_general_buff_cache';
  push @tasks, 'monitor_general_buff_cache';

  my $cleaned = 0;
  $job->minion->jobs({
    tasks  => \@tasks,
    states => ['finished', 'failed', 'inactive'],
  })->each(sub {
    my $ji = $_;
    if ($ji->{finished} && $ji->{finished} < $prebuild_start) {
      $job->minion->job($ji->{id})->remove;
      $cleaned++;
    }
    elsif ($ji->{failed}) {
      $job->minion->job($ji->{id})->remove;
      $cleaned++;
    }
    elsif ($ji->{notes}->{prebuild_run_id} ne $job->prebuild_run_id) {
      my $id = $ji->{id};
      if ($id =~ /\d+/) {
        $job->minion->job($id)->remove;
        $cleaned++;
      }

    }
  });
  $job->logger->debug(sprintf('cleaned %s jobs after harvest ran', $cleaned));
}

1;
__END__

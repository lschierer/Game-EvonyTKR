use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::JobBase {
  use Mojo::Base 'Minion::Job',                       -signatures;
  use Mojo::Base 'Mojolicious::Plugin',               -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',     -role;
  use Mojo::Base 'Game::EvonyTKR::Role::JSON',        -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;
  use diagnostics;
  use Carp;

  has prebuild_run_id => '';

  has standard_delay => 30;

  sub task_name {
    my $class = shift;
    $class->log_logcroak(
      sprintf('%s must implement task_name()', ref($class) || $class));
  }

  # Track which classes have been registered to prevent multiple registrations
  # This prevents Hypnotoad worker forks from re-registering tasks
  state %registered_classes;

  # Initialize Log4perl for all job-based classes
  # Returns 1 if registration happened, 0 if already registered
  sub register ($plugin, $app, $conf = {}) {
    # Prevent multiple registrations of the same class
    my $class = ref($plugin) || $plugin;
    return 0 if $registered_classes{$class}++;

    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $plugin->log_error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $plugin->log_error($errmessage);
      say $errmessage;
      return;
    }
    # Test backend connectivity (works for both SQLite and Redis)
    eval { $app->minion->backend->list_jobs(0, 1) };
    if ($@) {
      my $errmessage =
        sprintf('Minion backend connectivity test failed for %s: %s',
        __PACKAGE__, $@);
      $plugin->log_error($errmessage);
      say $errmessage;
      return;
    }

    #force the subclass to implement task_name
    $plugin->task_name();

    return 1;    # Registration completed successfully
  }

  sub run {
    my $job = shift;
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    my $parent_notes = $job->info->{notes} || {};
    $job->prebuild_run_id($parent_notes->{prebuild_run_id} || '');
    Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);

    $job->log_debug(
      sprintf('JobBase configured Logging in run "%s"', $job->prebuild_run_id));
    unless (defined($job->app)) {
      my $errmessage = sprintf('app undefined in job for %s', __PACKAGE__);
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }

    $job->note(prebuild_run_id => $job->prebuild_run_id)
      if (length($job->prebuild_run_id));
    $job->harvest_tagged_jobs();
  }

  sub harvest_tagged_jobs ($job) {
    my $harvested = 0;
    if (!length($job->prebuild_run_id)) {
      $job->log_warn('cannot harvest without a prebuild_run_id');
      return;
    }

    # Only harvest once per prebuild run - use a metadata flag
    my $harvest_key = "harvested_" . $job->prebuild_run_id;
    if ($job->persistence->get_metadata($harvest_key)) {
      $job->log_debug(
        "Harvesting already done for run " . $job->prebuild_run_id);
      return 0;
    }

    # Mark as harvesting to prevent concurrent harvests
    $job->persistence->set_metadata($harvest_key, { started_at => time() });

    # Harvest stale Minion jobs
    my $jobs = $job->minion->jobs({
      states => [qw(inactive active failed)],
      limit  => 50000                           # Large limit to catch all jobs
    });

    while (my $j = $jobs->next) {
      my $notes = $j->{notes} || {};
      unless (exists $notes->{prebuild_run_id}) {
        # This job was from a previous run, remove it
        eval { $job->minion->job($j->{id})->remove };
        $harvested++ unless $@;
        next;
      }
      unless ($notes->{prebuild_run_id} eq $job->prebuild_run_id) {
        # This job was from a previous run, remove it
        eval { $job->minion->job($j->{id})->remove };
        $harvested++ unless $@;
      }
    }
    $job->log_info(sprintf('harvested %s Minion jobs', $harvested));

    # Also harvest stale persistence job_completed records
    my $persistence_harvested =
      eval { $job->harvest_job_completions($job->prebuild_run_id); };
    if ($@) {
      $job->log_warn("Failed to harvest persistence records: $@");
      $persistence_harvested = 0;
    }

    $job->log_info(sprintf(
      'Total harvested: %d Minion jobs, %d persistence records',
      $harvested, $persistence_harvested || 0
    ));

    return $harvested + ($persistence_harvested || 0);
  }
}

1;

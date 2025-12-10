use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::JobBase {
  use Mojo::Base 'Minion::Job',                       -signatures;
  use Mojo::Base 'Mojolicious::Plugin',               -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',     -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;
  use diagnostics;
  use Carp;

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
    $app->minion->backend->sqlite->db->ping;

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
    Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    $job->log_debug("JobBase configured Logging in run.");
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
  }
}

1;

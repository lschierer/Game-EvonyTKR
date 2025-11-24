use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::JobBase {
  use Mojo::Base 'Minion::Job',                  -signatures;
  use Mojo::Base 'Mojolicious::Plugin',          -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Log::Config',  -role;
  use diagnostics;
  use Carp;

  # Initialize Log4perl for all job-based classes
  sub register ($plugin, $app, $conf = {}) {

    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $plugin->logger->error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $plugin->logger->error($errmessage);
      say $errmessage;
      return;
    }
    $app->minion->backend->sqlite->db->ping;

    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run {
    my $job = shift;
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    Game::EvonyTKR::Log::Config->get_logger();
    $job->logger->debug("JobBase configured Logging in run.");
    unless (defined($job->app)) {
      my $errmessage = sprintf('app undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
  }
}

1;

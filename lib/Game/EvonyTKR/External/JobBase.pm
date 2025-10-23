use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Log::Config;

package Game::EvonyTKR::External::JobBase {
  use Mojo::Base 'Minion::Job',                  -signatures;
  use Mojo::Base 'Mojolicious::Plugin',          -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Cache',  -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Carp;

  # Initialize Log4perl for all job-based classes
  sub register ($plugin, $app, $conf = {}) {
    unless (Log::Log4perl->initialized()) {
      Game::EvonyTKR::Log::Config->logger();
      $plugin->logger->debug("JobBase configured Logging in register.");
    }
    else {
      $plugin->logger->debug("JobBase found Logging configured in register.");
    }
  }

  sub run {
    my $plugin = shift;
    Game::EvonyTKR::Log::Config->logger();
    $plugin->logger->debug("JobBase configured Logging in run.");
  }
}

1;

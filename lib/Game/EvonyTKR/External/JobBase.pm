use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::JobBase {
  use Mojo::Base 'Minion::Job', -signatures;
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Game::EvonyTKR::Log::Config;
  use Carp;

  my $logger;

  # Initialize Log4perl for all job-based classes
  sub register ($self, $app, $conf = {}) {
    unless (Log::Log4perl->initialized()) {
      $logger = Game::EvonyTKR::Log::Config->logger();
    }
  }

  sub run {
    unless (Log::Log4perl->initialized()) {
      $logger = Game::EvonyTKR::Log::Config->logger();
    }
  }
}

1;

package Game::EvonyTKR::External::JobBase {
  use Mojo::Base 'Minion::Job', -signatures;
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Game::EvonyTKR::Log::Config;

  # Initialize Log4perl for all job-based classes
  sub register ($self, $app, $conf = {}){

  }

  sub run {
    unless (Log::Log4perl->initialized()) {
      Game::EvonyTKR::Log::Config->logger();
    }
  }
}

1;

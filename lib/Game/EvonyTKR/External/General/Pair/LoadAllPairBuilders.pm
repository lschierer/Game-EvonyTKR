use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(load_all_pair_builders => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));
    $job->logger->info('Starting LoadAllPairBuilders job');

    # Get all generals from cache
    my $generals = $job->get_generals($job->app);

    $job->logger->info(
      sprintf('Found %d generals to process', scalar keys %$generals));

    # Spawn CreatePairs jobs for each general/type combination
    my $job_count = 0;
    foreach my $general_name (keys %$generals) {
      my $general = $generals->{$general_name};

      # Handle scalar vs array types for this general
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

      # Create jobs for each type this general supports
      foreach my $type (@$general_types) {
        my $job_id =
          $job->minion->enqueue('create_pairs' => [$general_name, $type]);
        $job->logger->debug(sprintf(
          'Enqueued create_pairs job %s for general %s, type %s',
          $job_id, $general_name, $type
        ));
        $job_count++;
      }
    }

    $job->logger->info(
      sprintf('LoadAllPairBuilders job completed, spawned %d create_pairs jobs',
        $job_count)
    );
  }
}

1;

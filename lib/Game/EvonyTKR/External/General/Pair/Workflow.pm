use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::External::General::Pair::Builder;

package Game::EvonyTKR::External::General::Pair::Workflow {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf = {}) {
    $self->SUPER::register($app, $conf);
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug('Registering pair workflow tasks');

    # Job spawner - creates individual pair building jobs
    $app->minion->add_task(
      build_all_pairs => sub ($job, $args) {
        $logger->debug('build_all_pairs task starting');

        # Prevent multiple spawners with 2-hour lock
        return $job->finish('only one pair builder kickoff')
          unless my $guard = $job->app->minion->lock('build_all_pairs', 7200);

        return $self->spawn_pair_jobs($job, $args);
      }
    );

    # Individual pair builder job
    $app->minion->add_task(
      build_pairs_for_primary => sub ($job, $args) {
        $logger->debug('build_pairs_for_primary task starting');

        return $self->build_pairs_for_general($job, $args);
      }
    );

    # Monitor and aggregator job
    $app->minion->add_task( monitor_pair_builders => __PACKAGE__ );

    $app->plugins->emit(pair_workflow_loaded => {});
  }

  sub run ($self, $args) {
    $self->SUPER::run($args);
    $logger->debug('monitor_pair_builders task starting');

    # Allow multiple monitors (one per hypnotoad worker)
    return $self->monitor_pair_building_progress($self, $args);
  }

  sub spawn_pair_jobs ($self, $job, $args) {
    $logger->info('Spawning individual pair building jobs');

    my @builderJobs;

    Mojo::File->new($job->app->config('distDir'))
      ->child('collections/data/generals')
      ->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each(
      sub ($e, $index) {
        my $general_name = $e->basename('.yaml');

        # Normalize the general name
        my $builder = Game::EvonyTKR::External::General::Pair::Builder->new(
          app => $job->app,
        );
        $general_name = $builder->normalize($general_name);

        $logger->debug("Enqueueing job for general: $general_name");

        my $child_jid = $job->app->minion->enqueue(
          build_pairs_for_primary => [{ general_name => $general_name }] => {
            priority => 1,
            attempts => 5,
            delay    => 1 + rand(0.5),
            expire   => 3600,
          }
        );

        push @builderJobs, $child_jid;
      }
      );

    $job->note(builderJobs => \@builderJobs);
    $logger->info(sprintf('%d builder jobs started', scalar @builderJobs));

    return $job->finish('all builder jobs started');
  }

  sub build_pairs_for_general ($self, $job, $args) {
    # Concurrency limiting
    my $limit_length = 300;
    unless (
      my $taskLimit = $job->minion->guard(
        'build_pairs_for_primary', $limit_length, { limit => 3 }
      )
    ) {
      $logger->info('Concurrency limit hit for build_pairs_for_primary');
      return $job->retry({ delay => rand($limit_length) });
    }

    my $general_name = $args->{general_name};
    unless (length($general_name)) {
      $logger->error('general_name not provided to build_pairs_for_primary');
      return $job->finish(
        'general_name not provided to build_pairs_for_primary');
    }

    # Prevent duplicate jobs for same general
    return $job->finish(
      "build_pairs_for_primary for $general_name already launched")
      unless my $bppGuard =
      $job->app->minion->guard("build_pairs_for_primary_${general_name}",
      360);

    $logger->info("Building pairs for general: $general_name");
    my $testExternalCommonLog = Log::Log4perl->get_logger('Game::EvonyTKR::External::Common');
    $logger->info(sprintf('in %s, Log::Log4perl %s initialized. External::Common has level %s',
      __PACKAGE__, Log::Log4perl->initialized() ? 'is' : 'is not',
      Log::Log4perl::Level::to_level($testExternalCommonLog->level())
    ));
    # Create builder instance for this specific job
    my $builder = Game::EvonyTKR::External::General::Pair::Builder->new(
      app => $job->app,
    );

    # Execute the pair building logic
    my $result = $builder->build_pairs_for_primary($job, $general_name);

    return $result;
  }

  sub monitor_pair_building_progress ($self, $job, $args) {
    $logger->info('Starting pair building progress monitor');

    # Get existing state from job notes
    my $existing_pairs = $job->info->{notes}->{pairs_by_type} // {};

    # Create builder instance with existing state
    my $builder = Game::EvonyTKR::External::General::Pair::Builder->new(
      app           => $job->app,
      pairs_by_type => $existing_pairs
    );

    # Execute monitoring logic
    return $builder->monitor_pair_builders($job);
  }
}

1;

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;

  sub register ($plugin, $app, $conf = {}) {
    $app->minion->add_task(load_all_pair_builders => __PACKAGE__);
    $app->plugins->emit('Game_EvonyTKR_External_General_Pair_LoadAllPairBuilders');
  }

  sub run ($self, @args) {
    $self->logger->info('Starting LoadAllPairBuilders job');

    # Get all generals from cache
    my $generals = $self->get_generals();
    my @general_names = keys %$generals;

    $self->logger->info(sprintf('Found %d generals to process', scalar @general_names));

    # Get all valid general types
    my @general_types = $self->GeneralKeys();

    # Spawn CreatePairs jobs for each general/type combination
    my $job_count = 0;
    foreach my $general_name (@general_names) {
      foreach my $type (@general_types) {
        my $job_id = $self->minion->enqueue('create_pairs' => [$general_name, $type]);
        $self->logger->debug(sprintf('Enqueued create_pairs job %s for general %s, type %s',
          $job_id, $general_name, $type));
        $job_count++;
      }
    }

    $self->logger->info(sprintf('LoadAllPairBuilders job completed, spawned %d create_pairs jobs', $job_count));
  }
}

1;

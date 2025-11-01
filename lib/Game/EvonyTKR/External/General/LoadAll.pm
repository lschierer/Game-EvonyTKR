use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',      -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::File;

  sub register ($plugin, $app, $conf = {}) {
    $app->minion->add_task(load_all_generals => __PACKAGE__);
    $app->plugins->emit('Game_EvonyTKR_External_General_LoadAll');
  }

  sub run ($job, @args) {
    $job->logger->info('Starting LoadAll generals job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalDir = $collectionDir->child('generals');

    my @files =
      $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;

    $job->logger->info(
      sprintf('Found %d general files to load', scalar @files));

    foreach my $file (@files) {
      my $job_id = $job->minion->enqueue('load_general' => [$job->normalize($file->to_string)]);
      $job->logger->debug(
        sprintf(
          'Enqueued load_general job %s for file %s',
          $job_id, $file->basename
        )
      );
    }
    $job->note(generalCount => scalar(@files));
    $job->logger->info('LoadAll generals job completed');
  }
}

use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

require Game::EvonyTKR::External::Common;

package Game::EvonyTKR::External::General::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug('Registering pair workflow tasks');
    $app->minion->add_task(load_general => __PACKAGE__);
    $app->setup_general_storage($app);
    $app->plugins->emit(general_loader_job_ready => 1);
  }

  sub run ($job, @args) {
    $job->SUPER::run(@args);
    my $params = shift @args;
    my $general_name = $params->{general_name};

    my $index = $params->{index};
    my $worker = Game::EvonyTKR::External::Common->new(
      app     => $job->app,
    );
    my $collectionDir =
      Mojo::File->new($job->app->config('distDir'))->child('collections/data/');
    my $generalsDir   = $collectionDir->child('generals');
    my ($generalFile) = $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->grep(sub {qr/$general_name/i})->head(1)->each;
    unless($generalFile) {
      $logger->error("no file found for general with name $general_name");
      return $job->finish("no file found for general with name $general_name");
    }
    my $general = $worker->load_single_general($generalFile, $index);
    unless($general){
      $logger->error("No general returned by load_single_general for general with name $general_name");
      return $job->finish("No general returned by load_single_general for general with name $general_name");
    }
    my $generals = $job->app->get_shared_data('generals');
    $generals->{$general->normalize($general->name)} = $general;
    $job->app->set_shared_data('generals', $generals);
    $logger->info(sprintf('general loaded: %s',$general->name));
    return $job->finish(sprintf('general loaded: %s',$general->name));
  }
}

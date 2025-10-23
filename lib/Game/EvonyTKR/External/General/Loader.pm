use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

require Game::EvonyTKR::External::Common;

package Game::EvonyTKR::External::General::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',               -role;
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $taskClass->logger->debug('Registering General Loader workflow tasks');
    $app->minion->add_task(load_general => __PACKAGE__);

    $app->plugins->emit(general_loader_job_ready => 1);
  }

  sub run ($job, @args) {
    $job->SUPER::run(@args);
    $job->logger->debug(
      sprintf('::General::Loader log level is %s',
        Log::Log4perl::Level::to_level($job->logger->level()))
    );
    my $params       = shift @args;
    my $general_name = $job->normalize($params->{general_name});
    my $index        = $params->{index};
    my @suffixlist   = ('.yaml', '.yml');
    state $generalCache;
    unless (
      my $lock = $job->app->minion->guard(
        "load_general: $general_name",
        300, { limit => 1 }
      )
    ) {
      $job->finish(sprintf(
        'import for "%s" has already started; %s exiting.',
        $general_name, $index
      ));
    }
    my $worker = Game::EvonyTKR::External::Common->new(app => $job->app,);
    my $collectionDir =
      Mojo::File->new($job->app->config('distDir'))->child('collections/data/');
    my $generalsDir = $collectionDir->child('generals');
    my ($generalFile) = $generalsDir->list->sort->grep(sub {
      my $b = $job->normalize($_->basename(@suffixlist));
      if ($_ =~ m/\.y[a]?ml$/ && $b eq $general_name) {
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless ($generalFile) {
      $job->logger->error("no file found for general with name $general_name");
      return $job->fail("no file found for general with name $general_name");
    }
    my $general;
    $generalCache = $job->create_general_cache() unless defined($generalCache);
    $general = $job->get_general($general_name,$generalCache);
    if(defined($general) &&
      ref($general) eq 'HASH' &&
      blessed($general) && $general->isa('Game::EvonyTKR::Model::General')) {
      my $result = sprintf('returning already loaded general %s', $general_name);
      $job->logger->info($result);
      return $job->finish($result);
    }
    $general = $worker->load_single_general($generalFile, $index);
    unless ($general) {
      my $result = sprintf(
        'No general returned by load_single_general'
          . ' for general with name "%s"',
        $general_name
      );
      $job->logger->error($result);
      return $job->fail($result);
    }

    $generalCache = $job->create_general_cache() unless defined($generalCache);
    my $storeResult = $job->add_general($general->normalize($general->name),
      $general, $generalCache);
    my $result;
    if (defined $storeResult && $storeResult eq '1') {
      $result = sprintf('general cached: %s', $job->normalize($general->name));
      $job->logger->info($result);
    }
    elsif (defined $storeResult && $storeResult eq '0') {
      my $result = sprintf('add_general reports inability to set "%s"',
        $job->normalize($general->name));
      $job->logger->error($result);
      return $job->info->fail($result);
    }
    else {
      my $result = sprintf(
        'add_general reports "%s" error setting "%s"',
        defined $result ? $result : 'unknown',
        $job->normalize($general->name)
      );
      $job->logger->error($result);
      return $job->info->fail($result);
    }
    return $job->finish($result);
  }
}

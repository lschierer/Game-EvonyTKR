use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  require Game::EvonyTKR::External::General::BuildIndexes;
  use Mojo::File;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(load_all_generals => __PACKAGE__);
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

    return
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_ascending_attributes', 'load_all_builtin_books',
        'load_all_specialties',          'load_ascending_attributes',
        'load_book',                     'load_specialty',
      ]
      ));

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
      my $job_id = $job->minion->enqueue(
        'load_general' => [$job->normalize($file->to_string)] => {
          attempts => 3,
          delay    => rand(10),
          expire   => 1000,
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_general job %s for file %s',
        $job_id, $file->basename
      ));
    }
    $job->minion->enqueue(
      build_general_indexes => [] => {
        attempts => 3,
        priority => 30,
      }
    );
    $job->note(generalCount => scalar(@files));
    my $message = 'LoadAll generals job completed';
    $job->logger->info($message);
    return $job->finish($message);
  }
}

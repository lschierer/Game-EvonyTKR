use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Specialty::LoadAllSpecialties {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::File;

  sub task_name {'load_all_specialties'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
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
    $job->logger->info('Starting LoadAllSpecialties job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $specialtyDir = $collectionDir->child('specialties');

    my @files =
      $specialtyDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;

    $job->logger->info(
      sprintf('Found %d specialty files to load', scalar @files));

    foreach my $file (@files) {
      my $job_id = $job->minion->enqueue(
        'load_specialty' => [$file->to_string] => {
          attempts => 3,
          delay    => rand(10),
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_specialty job %s for file %s',
        $job_id, $file->basename
      ));
    }

    $job->logger->info('LoadAllSpecialties job completed');
  }
}

1;

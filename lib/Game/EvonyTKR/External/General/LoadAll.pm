use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',      -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
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

    return if($job->are_prereqs_outstanding());

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
          expire   => 300,
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_general job %s for file %s',
        $job_id, $file->basename
      ));
    }
    $job->note(generalCount => scalar(@files));
    my $message = 'LoadAll generals job completed';
    $job->logger->info($message);
    return $job->finish($message);
  }

  sub are_prereqs_outstanding ($job) {
    my $bookLoaderFinishedCount = $job->app->minion->jobs({
      tasks  => ['load_book'],
      states => ['finished'],
    })->total // 0;
    my $bookLoaderPendingCount = $job->app->minion->jobs({
      tasks  => ['load_book'],
      states => ['active', 'inactive'],
    })->total // 0;

    my $bookLoaderFailedCount = $job->app->minion->jobs({
      tasks  => ['load_book'],
      states => ['failed'],
    })->total // 0;

    my $specialtyLoaderFinishedCount = $job->app->minion->jobs({
      tasks  => ['load_specialty'],
      states => ['finished'],
    })->total // 0;
    my $specialtyLoaderPendingCount = $job->app->minion->jobs({
      tasks  => ['load_specialty'],
      states => ['active', 'inactive'],
    })->total // 0;
    my $specialtyLoaderFailedCount = $job->app->minion->jobs({
      tasks  => ['load_specialty'],
      states => ['failed'],
    })->total // 0;

    $job->logger->debug(sprintf('Job query results: book_pending=%d, specialty_pending=%d',
      $bookLoaderPendingCount, $specialtyLoaderPendingCount));

    if ($bookLoaderFailedCount > 0) {
      return $job->fail(
        'cannot import generals if book import was not successful');
    }
    if ($specialtyLoaderFailedCount > 0) {
      return $job->fail(
        'cannot import generals if specialty import was not successful');
    }

    $job->note(bookLoaderPendingCount      => $bookLoaderPendingCount);
    $job->note(specialtyLoaderPendingCount => $specialtyLoaderPendingCount);

    if ($bookLoaderPendingCount > 0 || $specialtyLoaderPendingCount > 0) {
      # delay a max of 30 seconds
      my $delay = List::Util::min(2 * $bookLoaderPendingCount, 30);
      $job->logger->debug(sprintf(
        'kicking off retry with delay %s due to %s',
        $delay,
        sprintf(
          'pending books: %s; pending specialties: %s',
          $bookLoaderPendingCount, $specialtyLoaderPendingCount
        )
      ));
      return $job->retry({ delay => $delay });
    }

    if ($bookLoaderFinishedCount > 0 && $specialtyLoaderFinishedCount > 0) {
      return 0;
    }

    if ($bookLoaderFinishedCount == 0) {
      return $job->fail(sprintf(
        '%s must be started before %s is launched',
        'book loading', __PACKAGE__
      ));
    }

    if ($specialtyLoaderFinishedCount == 0) {
      return $job->fail(sprintf(
        '%s must be started before %s is launched',
        'specialty loading', __PACKAGE__
      ));
    }

    #fall back value, should not be reached.
    return 1;
  }
}

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

    return if ($job->are_prereqs_outstanding());

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

    my $prereqs = {
      load_book                 => 0,
      load_specialty            => 0,
      load_ascending_attributes => 0,
    };

    foreach my $prereq (keys $prereqs->%*){
      my $prereqFinishedCount = $job->app->minion->jobs({
        tasks  => [$prereq],
        states => ['finished'],
      })->total // 0;
      my $prereqPendingCount = $job->app->minion->jobs({
        tasks  => [$prereq],
        states => ['active', 'inactive'],
      })->total // 0;
      my $prereqFailedCount = $job->app->minion->jobs({
        tasks  => [$prereq],
        states => ['failed'],
      })->total // 0;
      if($prereqFailedCount > 0) {
        my $errmessage = sprintf('cannot import generals if %s import was not successful.', $prereq);
        $job->logger->error($errmessage);
        return $job->fail($errmessage);
      }
      $job->note("${prereq}PendingCount" => $prereqPendingCount);
      if($prereqPendingCount > 0 ) {
        my $delay = List::Util::min(2 * $prereqPendingCount, 30);
        $job->logger->debug(sprintf(
          'kicking off retry with delay %s due to %s',
          $delay,
          sprintf(
            'pending %s: %s',
            $prereq, $prereqPendingCount
          )
        ));
        return $job->retry({ delay => $delay });
      }
      $prereqs->{$prereq} = $prereqFinishedCount;
    }

    if ( List::AllUtils::all {$_ ne "0" } values $prereqs->%* ) {
      return 0;
    }

    foreach my $prereq (keys $prereqs->%*){
      my $prereqFinishedCount = $prereqs->{$prereq};
      if($prereqFinishedCount == 0){
        my $errmessage = sprintf('%s must be finished before %s is launched', $prereq, __PACKAGE__);
        $job->logger->error($errmessage);
        return $job->fail($errmessage);
      }
    }

    #fall back value, should not be reached.
    return 1;
  }
}

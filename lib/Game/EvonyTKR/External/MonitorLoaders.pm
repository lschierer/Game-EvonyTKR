use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::MonitorLoaders {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(monitor_loaders => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run([]);
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
      if $job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_ascending_attributes', 'load_all_builtin_books',
        'load_all_generals',             'load_all_generic_books',
        'load_all_pair_builders',        'load_all_specialties',
        'load_all_covenants',
      ]
      );

    $job->logger->info(sprintf('starting run of %s', __PACKAGE__));
    my $loader_types = [
      'load_general',         'load_book',
      'load_specialty',       'load_all_generic_books',
      'load_all_generals',    'load_all_builtin_books',
      'load_all_specialties', 'load_all_covenants',
      'load_covenant',
    ];

    my $LoaderFinishedCount = $job->app->minion->jobs({
      tasks  => [$loader_types->@*],
      states => ['finished'],
    })->total // 0;
    my $LoaderPendingCount = $job->app->minion->jobs({
      tasks  => [$loader_types->@*],
      states => ['active', 'inactive'],
    })->total // 0;

    my $LoaderFailedCount = $job->app->minion->jobs({
      tasks  => [$loader_types->@*],
      states => ['failed'],
    })->total // 0;

    if ($LoaderFailedCount > 0) {
      $job->note(LoaderFailedCount => $LoaderFailedCount);
      return $job->fail('detected a failed job.');
    }

    $job->note(LoaderPendingCount => $LoaderPendingCount);
    if ($LoaderPendingCount > 0) {
      my $delay = List::Util::min($LoaderPendingCount, 5);
      return $job->retry({ delay => $delay });
    }

    if ($LoaderFinishedCount > 0) {
      return $job->finish('all loaders finished');    # Ready to proceed
    }

  }

}
1;
__END__

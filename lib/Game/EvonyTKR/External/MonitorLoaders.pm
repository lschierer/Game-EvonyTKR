use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::MonitorLoaders {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;

  sub task_name {'monitor_loaders'}

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
    $job->SUPER::run([]);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    $job->log_debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    return
      if $job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_generals',      'load_all_builtin_books',
        'load_all_generic_books', 'load_all_specialties',
        'load_all_covenants',     'load_all_ascending_attributes',
        'load_all_pair_builders',
      ]
      );

    $job->log_info('All loader jobs completed');

    # Mark job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    return $job->finish('All loaders finished');

  }

}
1;
__END__

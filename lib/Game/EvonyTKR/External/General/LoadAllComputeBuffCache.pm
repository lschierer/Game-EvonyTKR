package Game::EvonyTKR::External::General::LoadAllComputeBuffCache;
use v5.42.0;
use utf8::all;

use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
require Game::EvonyTKR::External::General::BuildIndexes;
use Mojo::File;

sub task_name {'load_all_compute_buff_cache'}

sub register ($taskClass, $app, $conf = {}) {
  $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($job, @args) {
  if (not defined($job)) {
    say 'job not defined in run for ' . $job->task_name;
    return;
  }
  $job->SUPER::run(@args);
  unless (defined($job->minion)) {
    my $errmessage = sprintf('minion undefined in job for %s', $job->task_name);
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
      'load_all_covenants',            'load_all_generals',
      'load_all_generic_books',        'load_all_specialties',
      'load_ml_conflicts',
    ]
    ));

  $job->logger->info(sprintf('Starting %s job', $job->task_name));

  my $cache_jobs = [];

  my $generals = {};
  foreach my $general_name ($job->list_generals->@*) {
    my $general = $job->get_general($general_name);
    unless ($general) {
      $job->logger->error(sprintf(
        'failed to get general "%s" from persistence.', $general_name));
      next;
    }
    $general->type->[0] =~ /^(\S+)/;
    my $type = $1;
    push @{ $generals->{$type} }, $general;
  }

  foreach my $type (sort keys $generals->%*) {
    foreach my $general ($generals->{$type}->@*) {
      my $queue;
      my $priority;
      my $delay;

      # Base priority by primary type (10-point spread)
      if ($general->type->[0] =~ /siege/i) {
        $priority = -10;
        $delay    = 0;
        $queue    = 'siege';
      }
      elsif ($general->type->[0] =~ /ground/i) {
        $priority = -20;
        $delay    = 10;
        $queue    = 'ground';
      }
      elsif ($general->type->[0] =~ /ranged/i) {
        $priority = -30;
        $delay    = 20;
        $queue    = 'ranged';
      }
      elsif ($general->type->[0] =~ /mounted/i) {
        $priority = -40;
        $delay    = 30;
        $queue    = 'mounted';
      }
      elsif ($general->type->[0] =~ /mayor/i) {
        $priority = -50;
        $delay    = 40;
        $queue    = 'mayor';
      }
      elsif ($general->type->[0] =~ /wall/i) {
        $priority = -60;
        $delay    = 50;
        $queue    = 'wall';
      }
      else {
        $priority = -50;
        $delay    = 60;
        $queue    = 'default';
      }

      # Dual/triple-type penalty (they take longer, push to end)
      my $type_count = scalar(@{ $general->type });
      if ($type_count > 1) {
        $priority -= 15 * $type_count;
      }
      $priority = -99 if ($priority < -99);

      my $cache_job_id = $job->minion->enqueue(
        'compute_general_buff_cache' => [$general->name] => {
          attempts => 3,
          queue    => $queue,
          delay    => $delay,
          priority => $priority,
          notes    => {
            prebuild_run_id => $job->prebuild_run_id,
            delay           => $delay,
          }
        }
      );
      push @$cache_jobs, $cache_job_id;
      $job->logger->debug(sprintf(
        'Enqueued buff cache job %s for "%s"',
        $cache_job_id, $general->name
      ));
    }
  }

  # Mark this job as completed in persistence (with run_id for isolation)
  # IMPORTANT: This must be OUTSIDE the if (@job_ids) block so jobs that
  # skip all work (because data exists) still mark themselves complete

  $job->mark_task_completed($job->task_name, $job->prebuild_run_id);

  $job->logger->info(
    sprintf("Enqueued %s buff cache computation jobs", scalar(@$cache_jobs)));

  # Enqueue monitor job to track buff cache completion
  $job->minion->enqueue(
    'monitor_general_buff_cache' => [] => {
      attempts => 10,
      priority => 5,             # Low priority, runs after other jobs
      parents  => $cache_jobs,
      lax      => 1,
      notes    => { prebuild_run_id => $job->prebuild_run_id }
    }
  );

  $job->note(generalCount => scalar(@{ $job->list_generals }));
  my $message = sprintf('%s job completed', $job->task_name);
  $job->logger->info($message);
  return $job->finish($message);
}

1;
__END__

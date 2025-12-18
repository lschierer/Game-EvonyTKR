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
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    $job->log_debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    return
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_ascending_attributes',
        'load_all_builtin_books',
        'load_all_covenants',
        'load_all_generals',
        'load_all_generic_books',
        'load_all_specialties',
        'load_ml_conflicts',
      ]
      ));

    $job->log_info(sprintf('Starting %s job', $job->task_name));

    my $cache_jobs = [];

    foreach my $general_name ($job->list_generals->@*) {
      my $general = $job->get_general($general_name);
      unless($general){
        $job->log_error(sprintf('failed to get general "%s" from persistence.',
        $general_name));
        next;
      }

      # prioritize the generals with only one type.
      my $priority = 0 - scalar(@{ $general->type });
      if($general->type->[0] =~ /siege/i){
        $priority--;
      }elsif($general->type->[0] =~ /ground/i){
        $priority = $priority - 2;
      }elsif($general->type->[0] =~ /ranged/i){
        $priority = $priority - 3;
      }elsif($general->type->[0] =~ /mounted/i){
        $priority = $priority - 4;
      }
      my $cache_job_id = $job->minion->enqueue(
        'compute_general_buff_cache' => [ $general_name ] => {
          attempts =>  3,
          priority => $priority,
          notes    => { prebuild_run_id => $job->prebuild_run_id }
        }
      );
      push @$cache_jobs, $cache_job_id;
      $job->log_debug(
        "Enqueued buff cache job $cache_job_id for $general_name");
    }


    # Mark this job as completed in persistence (with run_id for isolation)
    # IMPORTANT: This must be OUTSIDE the if (@job_ids) block so jobs that
    # skip all work (because data exists) still mark themselves complete

    $job->mark_task_completed($job->task_name, $job->prebuild_run_id);

    $job->log_info(sprintf("Enqueued %s buff cache computation jobs", scalar(@$cache_jobs)));

    # Enqueue monitor job to track buff cache completion
    $job->minion->enqueue(
      'monitor_general_buff_cache' => [] => {
        attempts  => 10,
        priority  => 5,     # Low priority, runs after other jobs
        parents   => $cache_jobs,
        lax       => 1,
        notes     => { prebuild_run_id => $job->prebuild_run_id }
      }
    );

    $job->note(generalCount => scalar(@{ $job->list_generals }));
    my $message = sprintf('%s job completed',$job->task_name);
    $job->log_info($message);
    return $job->finish($message);
  }

1;
__END__

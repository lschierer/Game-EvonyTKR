use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::MonitorCreatePairs2 {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts:');
  };

  state $total_conflicts = 0;
  state $merged_by_general = {};
  state $merged_groups_by_conflict_type = {};
  state $total_pairs = [];
  state $processed = {};

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(monitor_create_pairs => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, $loaderJids) {
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

    return if $job->are_prereqs_outstanding($loaderJids);

    # Initialize state from job notes
    my $notes = $job->info->{notes} // {};
    %{ $processed } = map { $_ => 1 } split ', ', $notes->{state}->{processed} //'';
    $total_conflicts = $notes->{total_conflicts} // 0;
    $merged_by_general = $notes->{by_general} // {};
    $merged_groups_by_conflict_type = $notes->{groups_by_conflict_type} // {};
    $total_pairs = $notes->{pairs} // [];

    $job->logger->info(sprintf('starting run of %s', __PACKAGE__));
    my $loader_types = [
      'load_all_pair_builders',
      'create_pairs',
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
      my $errmessage = sprintf('detected %s failed jobs', $LoaderFailedCount);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    my $finished = $job->app->minion->jobs({
      tasks  => [$loader_types->@*],
      states => ['finished'],
    });

    $finished->each(sub{
      my $info = $_;

      $processed->{$info->{id}}++;
      $job->logger->debug(
        sprintf('Processing new job %s with notes', $info->{id}));
      $job->merge_pair_results($info);
      $job->merge_conflict_results($info);
    });
    $job->note(
      by_general              => $merged_by_general,
      groups_by_conflict_type => $merged_groups_by_conflict_type,
      pairs                   => $total_pairs,
      state                   => {
      jobs_finished           => $LoaderFinishedCount,
      jobs_pending            => $LoaderPendingCount,
      jobs_processed          => scalar(keys($processed->%*)),
      processed               => join ', ', keys $processed->%*,
      },
      total_conflicts         => $total_conflicts,
      total_pairs             => scalar(@{$total_pairs}),
    );

    if ($LoaderPendingCount > 0) {
        my $delay = List::Util::min($LoaderPendingCount, 5);
        return $job->retry({ delay => $delay });
    }

    if ($LoaderFinishedCount > 0 ) {
      my $pc = scalar keys $processed->%*;
      if($pc < $LoaderFinishedCount){
        $job->logger->error('reached finish line before all jobs processed.');
        return $job->retry();
      }
      $job->pair_cache->set('pair_building_complete', 1);
      return $job->finish('pair creation finished');  # Ready to proceed
    }
  }

  sub merge_pair_results($job, $job_info){
    my $job_id = $job_info->{id};
    my $notes = $job_info->{notes} // {};
    $job->logger->debug(
      sprintf('Processing pair results for job %s with notes', $job_id));

    if (defined($notes->{pairs})) {
      $total_pairs = [ List::UtilsBy::uniq_by { $job->wire_pair_to_key($_) } ($total_pairs->@*, $notes->{pairs}->@*) ];
      foreach my $pair ($total_pairs->@*) {
        # I do not need to run a ->to_hash() here
        # because I have not yet instantiated any sort of object
        $job->add_wire_pair($pair);
      }
    }
    $job->note(
      total_pairs     => scalar(@{$total_pairs}),
      pairs           => $total_pairs,
    );
  }

  sub merge_conflict_results($job, $job_info) {
    my $job_id = $job_info->{id};
    my $notes = $job_info->{notes} // {};
    $job->logger->debug(
      sprintf('Processing conflict results for job %s with notes', $job_id));

    if ($notes->{by_general}) {
      $job->logger->debug(sprintf('by_general in job is %s',
        Data::Printer::np($notes->{by_general})));
      # Merge by_general data
      foreach my $general (keys %{ $notes->{by_general} }) {
        $merged_by_general->{$general} //= {};
        foreach
          my $other_general (keys %{ $notes->{by_general}->{$general} }) {
          $merged_by_general->{$general}->{$other_general} = 1;
        }
      }
    }

    if ($notes->{groups_by_conflict_type}) {
      $job->logger->debug(sprintf('groups_by_conflict_type in job is %s',
        Data::Printer::np($notes->{groups_by_conflict_type})));
      # Merge groups_by_conflict_type data
      foreach
        my $conflict_type (keys $notes->{groups_by_conflict_type}->%*) {
        $merged_groups_by_conflict_type->{$conflict_type} = [ List::AllUtils::uniq ($merged_groups_by_conflict_type->{$conflict_type}->@*, $notes->{groups_by_conflict_type}->{$conflict_type}->@*)];
      }
    }
    $total_conflicts += $notes->{conflicts_found} // 0;
    $job->note(
      total_conflicts         => $total_conflicts,
      by_general              => $merged_by_general,
      groups_by_conflict_type => $merged_groups_by_conflict_type,
    );

    $job->conflict_cache->set('merged_conflicts', {
      timestamp               => time(),
      total_conflicts         => $total_conflicts,
      by_general              => $merged_by_general,
      groups_by_conflict_type => $merged_groups_by_conflict_type,
    });
  }

  sub are_prereqs_outstanding ($job, $loaderJids) {

    unless(ref($loaderJids) && ref($loaderJids) eq 'ARRAY'){
      my $errmessage = sprintf('%s requires an arrayref as its first parameter', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    foreach my $loaderJid ($loaderJids->@*) {
      my $loader = $job->app->minion->job($loaderJid);
      $job->logger->debug(sprintf('%s checking jid %s', __PACKAGE__, $loaderJid));
      unless($loader){
        my $errmessage = sprintf('prerequisite job %s does not exist',
        $loaderJid);
        $job->logger->error($errmessage);
        return $job->fail($errmessage);
      }
      if($loader->info->{state} eq 'failed'){
        my $errmessage = sprintf('prerequisite job %s failed.', $loaderJid);
        my $error = $job->info->{notes}{error} // [];
        push @{$error}, $errmessage;
        push @{$error}, $loader->info->{result};
        $job->note(error => $error);
        $job->logger->error($errmessage);
        return $job->fail($errmessage);
      }
      if($loader->info->{state} eq 'finished'){
        next;
      }
      return $job->retry({delay => 1});
    }

    return 0;
  }

}
1;
__END__

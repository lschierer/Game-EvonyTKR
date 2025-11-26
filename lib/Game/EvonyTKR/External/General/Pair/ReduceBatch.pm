use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::ReduceBatch {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',       -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;

  sub task_name {'reduce_batch'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  state $total_conflicts                = 0;
  state $total_cache_hits               = 0;
  state $merged_by_general              = {};
  state $merged_groups_by_conflict_type = {};
  state $total_pairs                    = [];

  sub run ($job) {
    my $job_ids = $job->info->{parents};
    return $job->fail('No parent jobs found')
      unless $job_ids && ref($job_ids) eq 'ARRAY';

    $job->SUPER::run([]);
    $job->logger->info(
      sprintf('ReduceBatch processing %d parent jobs', scalar(@$job_ids)));

    my $batch_id = $job->id;

    foreach my $job_id (@$job_ids) {
      my $job_info = $job->minion->job($job_id);
      next unless $job_info;    # Parent jobs are guaranteed finished

      $job->logger->debug("Processing parent job $job_id");
      $job->merge_pair_results($job_info->info);
      $job->merge_conflict_results($job_info->info,);
    }

    # Store batch results in cache
    $job->pair_cache->set(
      "batch_results:$batch_id",
      {
        total_pairs    => scalar(@$total_pairs),
        pairs          => $total_pairs,
        processed_jobs => scalar(@$job_ids),
      }
    );

    $job->conflict_cache->set(
      "batch_results:$batch_id",
      {
        total_conflicts         => $total_conflicts,
        total_cache_hits        => $total_cache_hits,
        by_general              => $merged_by_general,
        groups_by_conflict_type => $merged_groups_by_conflict_type,
        processed_jobs          => scalar(@$job_ids),
      }
    );

    $job->note(
      total_conflicts         => $total_conflicts,
      by_general              => $merged_by_general,
      groups_by_conflict_type => $merged_groups_by_conflict_type,
      total_pairs             => scalar(@$total_pairs),
      pairs                   => $total_pairs,
      processed_jobs          => scalar(@$job_ids),
    );

    $job->finish("Processed " . scalar(@$job_ids) . " jobs");
  }

  sub merge_pair_results($job, $job_info) {
    my $notes = $job_info->{notes} // {};

    if (defined($notes->{pairs})) {
      $total_pairs = [
        List::UtilsBy::uniq_by { $job->wire_pair_to_key($_) }
        ($total_pairs->@*, $notes->{pairs}->@*)
      ];
      $job->logger->debug(sprintf(
        'after merging pair results for %s, ReduceBatch shows %s',
        $job_info->{id}, Data::Printer::np($total_pairs)
      ));
      foreach my $pair ($notes->{pairs}->@*) {
        $job->add_wire_pair($pair);
      }
    }
    else {
      $job->logger->warn(
        'ReduceBatch found a CreatePair job %s that ccreated no pairs.',
        $job_info->{id});
    }
    return $total_pairs;
  }

  sub merge_conflict_results($job, $job_info) {
    my $notes = $job_info->{notes} // {};

    # Aggregate cache hits
    $total_cache_hits += $notes->{conflict_cache_hits} // 0;

    if ($notes->{by_general}) {
      foreach my $general (keys %{ $notes->{by_general} }) {
        $merged_by_general->{$general} //= {};
        foreach my $other_general (keys %{ $notes->{by_general}->{$general} }) {
          $merged_by_general->{$general}->{$other_general} = 1;
        }
      }
    }

    if ($notes->{groups_by_conflict_type}) {
      foreach my $conflict_type (keys $notes->{groups_by_conflict_type}->%*) {
        $merged_groups_by_conflict_type->{$conflict_type} //= [];
        push @{ $merged_groups_by_conflict_type->{$conflict_type} },
          @{ $notes->{groups_by_conflict_type}->{$conflict_type} // [] };
      }
    }

    $total_conflicts += $notes->{conflicts_found} // 0;
  }
}
1;
__END__

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::ReduceCoordinator {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',       -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',                      -role;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(reduce_coordinator => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  state $total_conflicts                = 0;
  state $total_cache_hits               = 0;
  state $merged_by_general              = {};
  state $merged_groups_by_conflict_type = {};
  state $total_pairs                    = [];

  sub run ($job) {
    $job->SUPER::run([]);
    my $processed = $job->info->{notes}{processed} // {};

    # Initialize merged results from job notes
    my $notes = $job->info->{notes} // {};
    $total_conflicts                = $notes->{total_conflicts}         // 0;
    $total_cache_hits               = $notes->{total_cache_hits}        // 0;
    $merged_by_general              = $notes->{by_general}              // {};
    $merged_groups_by_conflict_type = $notes->{groups_by_conflict_type} // {};
    $total_pairs                    = $notes->{pairs}                   // [];

    # Find finished ReduceBatch jobs
    my $finished_batches = $job->minion->jobs({
      tasks  => ['reduce_batch'],
      states => ['finished']
    });

    my $new_batches = 0;
    $finished_batches->each(sub {
      my $batch_info = $_;
      my $batch_id   = $batch_info->{id};

      return if exists $processed->{$batch_id};
      $processed->{$batch_id}++;
      $new_batches++;

      $job->logger->debug("Processing batch results for job $batch_id");
      $job->cache_conflict_results($batch_id);
      $job->cache_pair_results($batch_id);
    });

    # Update job notes with current state
    $job->note(
      processed               => $processed,
      total_conflicts         => $total_conflicts,
      total_cache_hits        => $total_cache_hits,
      by_general              => $merged_by_general,
      groups_by_conflict_type => $merged_groups_by_conflict_type,
      total_pairs             => scalar(@$total_pairs),
      pairs                   => $total_pairs,
      batches_processed       => scalar(keys %$processed),
    );

    # Check if more ReduceBatch jobs are still running
    my $active_batches = $job->minion->jobs({
      tasks  => ['reduce_batch'],
      states => ['active', 'inactive']
    })->total;

    if ($active_batches > 0 || $new_batches > 0) {
      return $job->retry({ delay => 5 });
    }

    my $cache_effectiveness =
      $total_conflicts > 0
      ? sprintf("%.1f%%", ($total_cache_hits / $total_conflicts) * 100)
      : "N/A";

    $job->logger->info(sprintf(
      "Cache effectiveness: %d cache hits out of %d total conflicts (%s)",
      $total_cache_hits, $total_conflicts, $cache_effectiveness
    ));

    my $pc = 0;
    my $cc = 0;
    do {
      # Set completion flags for both Pairs and ConflictGroups controllers
      $pc = $job->pair_cache->set('pair_building_complete', 1);
      $cc = $job->conflict_cache->set('conflict_building_complete', 1);
      $job->logger->debug(sprintf(
        'pair_building_complete is %s; conflict_building_complete is %s.',
        $pc ? 'true' : 'false',
        $cc ? 'true' : 'false'
      ));
    } while (!$pc && !$cc);
    $job->logger->info("Set completion flags in cache");
    $job->finish(
      sprintf(
"Merged results from %d batches - %d conflicts (%d cache hits, %s effectiveness)",
        scalar(keys %$processed), $total_conflicts,
        $total_cache_hits,        $cache_effectiveness
      )
    );
  }

  sub cache_conflict_results($job, $batch_id) {
    # Get batch results from cache
    my $batch_results = $job->conflict_cache->get("batch_results:$batch_id");
    return unless $batch_results;

    # Merge batch results
    $total_conflicts  += $batch_results->{total_conflicts}  // 0;
    $total_cache_hits += $batch_results->{total_cache_hits} // 0;

    if ($batch_results->{by_general}) {
      foreach my $general (keys %{ $batch_results->{by_general} }) {
        $merged_by_general->{$general} //= {};
        %{ $merged_by_general->{$general} } = (
          %{ $merged_by_general->{$general} },
          %{ $batch_results->{by_general}->{$general} }
        );
      }
    }

    if ($batch_results->{groups_by_conflict_type}) {
      foreach my $type (keys %{ $batch_results->{groups_by_conflict_type} }) {
        $merged_groups_by_conflict_type->{$type} //= [];
        $merged_groups_by_conflict_type->{$type} = [
          List::AllUtils::uniq(
            $merged_groups_by_conflict_type->{$type}->@*,
            $batch_results->{groups_by_conflict_type}->{$type}->@*
          )
        ];
      }
    }

    # store incremental results
    $job->conflict_cache->set(
      'merged_conflicts',
      {
        timestamp               => time(),
        total_conflicts         => $total_conflicts,
        by_general              => $merged_by_general,
        groups_by_conflict_type => $merged_groups_by_conflict_type,
      }
    );
  }

  sub cache_pair_results ($job, $batch_id) {
    my $batch_results = $job->pair_cache->get("batch_results:$batch_id");
    if ($batch_results->{pairs}) {
      $total_pairs = [
        List::UtilsBy::uniq_by { $job->wire_pair_to_key($_) }
        ($total_pairs->@*, $batch_results->{pairs}->@*)
      ];
    }

    # only those in $batch_results->{pairs} can be new
    foreach my $pair ($batch_results->{pairs}->@*) {
      $job->add_wire_pair($pair);
    }
  }
}
1;
__END__

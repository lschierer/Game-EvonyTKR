use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::ReduceCoordinator {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',        -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use List::AllUtils qw(uniq none all any);

  sub task_name {'reduce_coordinator'}

  sub register ($taskClass, $app, $conf = {}) {
    return 1 unless $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);

    return 1;
  }

  state $total_conflicts                = 0;
  state $total_cache_hits               = 0;
  state $merged_by_general              = {};
  state $merged_groups_by_conflict_type = {};
  state $total_pairs                    = [];

  sub run ($job) {
    $job->SUPER::run([]);

    # Check if prerequisites are loaded - fail fast if not
    return
      if ($job->are_prereqs_outstanding(
      $job->minion, ['load_all_generals']));

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

      $job->log_debug("Processing batch results for job $batch_id");
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
      return $job->retry({ delay => $job->standard_delay });
    }

    my $cache_effectiveness =
      $total_conflicts > 0
      ? sprintf("%.1f%%", ($total_cache_hits / $total_conflicts) * 100)
      : "N/A";

    $job->log_info(sprintf(
      "Cache effectiveness: %d cache hits out of %d total conflicts (%s)",
      $total_cache_hits, $total_conflicts, $cache_effectiveness
    ));

    # Flush pairs_by_type to persistence before marking complete
    my $pairs_by_type = $job->pairs_by_type();
    foreach my $type (keys %$pairs_by_type) {
      my $pairs = $pairs_by_type->{$type};
      $job->log_info(sprintf(
        'Storing %d pairs for type %s to persistence',
        scalar(@$pairs), $type
      ));
      $job->persistence->store_pairs($type, $pairs);
    }

    # Set completion flags for both Pairs and ConflictGroups controllers
    my $pc_verify = 0;
    my $cc_verify = 0;
    do {
      # Use SQLite metadata for completion tracking
      $job->persistence->set_metadata('pair_building_complete', '1');
      my $pc = 1;    # Always succeeds with SQLite
          # Use SQLite metadata instead of memcache for conflict completion
      $job->persistence->set_metadata('conflict_building_complete', '1');
      my $cc = 1;    # Always succeeds with SQLite

      $job->log_info(sprintf(
"Cache set results: pair_building_complete=%s, conflict_building_complete=%s",
        defined($pc) ? ($pc ? 'success' : 'failed') : 'undef',
        defined($cc) ? ($cc ? 'success' : 'failed') : 'undef'
      ));

      # Verify the values were actually set
      $pc_verify = $job->persistence->get_metadata('pair_building_complete');
      $cc_verify =
        $job->persistence->get_metadata('conflict_building_complete');

      $job->log_info(sprintf(
"Cache verification: pair_building_complete=%s, conflict_building_complete=%s",
        defined($pc_verify) ? $pc_verify : 'undef',
        defined($cc_verify) ? $cc_verify : 'undef'
      ));
    } while (!$pc_verify || !$cc_verify);

    # Mark job as completed in persistence
    my $run_id = $job->info->{notes}->{prebuild_run_id};
    $job->mark_task_completed($job->task_name, $run_id);

    $job->finish(sprintf(
"Merged results from %d batches - %d conflicts (%d cache hits, %s effectiveness)",
      scalar(keys %$processed), $total_conflicts,
      $total_cache_hits,        $cache_effectiveness
    ));
  }

  sub cache_conflict_results($job, $batch_id) {
    # Get batch results from job notes (stored by ReduceBatch)
    my $batch_job = $job->minion->job($batch_id);
    return unless $batch_job;

    my $batch_results = $batch_job->info->{notes};
    return unless $batch_results;

    # Check if batch was skipped due to all cache hits
    if ( $batch_results->{skipped_reason}
      && $batch_results->{skipped_reason} eq 'all_cache_hits') {
      $job->log_debug(
        "Batch $batch_id was skipped (all cache hits), nothing to merge");
      $total_cache_hits += $batch_results->{total_cache_hits} // 0;
      return;
    }

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

        # Store conflicts to persistence
        foreach
          my $other_general (keys %{ $batch_results->{by_general}->{$general} })
        {
          my $conflicts = $merged_by_general->{$general}{$other_general};
          $job->persistence->store_conflict($general, $other_general,
            $conflicts);
        }
      }
    }

    if ($batch_results->{groups_by_conflict_type}) {
      foreach my $type (keys %{ $batch_results->{groups_by_conflict_type} }) {
        $merged_groups_by_conflict_type->{$type} //= [];
        $merged_groups_by_conflict_type->{$type} = [
          uniq(
            $merged_groups_by_conflict_type->{$type}->@*,
            $batch_results->{groups_by_conflict_type}->{$type}->@*
          )
        ];
      }
    }

    # Conflicts are already stored in SQLite above - no need for memcache
  }

  sub cache_pair_results ($job, $batch_id) {
    # Get batch results from job notes (stored by ReduceBatch)
    my $batch_job = $job->minion->job($batch_id);
    return unless $batch_job;

    my $batch_results = $batch_job->info->{notes};
    return unless $batch_results;

    # Check if batch was skipped - no pairs to process
    if ( $batch_results->{skipped_reason}
      && $batch_results->{skipped_reason} eq 'all_cache_hits') {
      $job->log_debug(
        "Batch $batch_id was skipped (all cache hits), no pairs to merge");
      return;
    }

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

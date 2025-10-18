use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require YAML::PP;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::External::Common;

class Game::EvonyTKR::External::General::Pair::Builder :
  isa(Game::EvonyTKR::External::Common) {
  use Unicode::Normalize;
  use Unicode::CaseFold qw(fc);
  use Encode            qw(is_utf8 decode_utf8 encode_utf8);
  use List::UtilsBy     qw(uniq_by);
  use Carp;

  field $conflicts : param = {};
  field $pairs_by_type : param : reader = {};

  # Build pairs for a specific primary general
  method build_pairs_for_primary ($job, $general_name) {
    $self->logger->info("Building pairs for primary general: $general_name");

    # Load all generals for comparison
    $self->load_generals($job->info->{task});

    my $primary = $self->generals->{ $self->normalize($general_name) };
    unless ($primary) {
      $self->logger->error("General '$general_name' not found!");
      $self->logger->debug(sprintf(
        'Available generals: %s',
        join ', ', sort keys $self->generals->%*
      ));
      return $job->finish("General '$general_name' not found!");
    }

    # Track initial pair counts for delta calculation
    my %initial_counts;
    foreach my $type (keys %{$pairs_by_type}) {
      $initial_counts{$type} = scalar @{ $pairs_by_type->{$type} } // 0;
    }

    # Build pairs with all other generals
    my $pairs_added = $self->build_pairs_with_all_generals($primary);

    # Calculate deltas and log results
    my $total_added = 0;
    foreach my $type (keys %{$pairs_by_type}) {
      my $current_count = scalar @{ $pairs_by_type->{$type} } // 0;
      my $delta         = $current_count - ($initial_counts{$type} // 0);
      $total_added += $delta;

      $self->logger->debug(sprintf(
        'General %s added %d pairs for type %s',
        $primary->name, $delta, $type
      ));
    }

    $self->logger->info(sprintf(
      'General %s: added %d total pairs', $primary->name, $total_added
    ));

    # Store results and conflicts in job notes
    $job->note(pairs_by_type => $pairs_by_type);
    $job->note(
      conflicts => {
        by_general              => $self->conflictDetector->by_general,
        groups_by_conflict_type =>
          $self->conflictDetector->groups_by_conflict_type
      }
    );

    # Emit signal for incremental results
    $self->app->plugins->emit(pairs_by_type => $pairs_by_type);

    return $job->finish(
      "Completed pairs for $general_name: $total_added pairs added");
  }

  method build_pairs_with_all_generals ($primary) {
    my $pairs_added = 0;

    foreach
      my $secondary (sort { $a->name cmp $b->name } values %{ $self->generals })
    {
      next if $primary->name eq $secondary->name;

      $self->logger->debug(sprintf(
        'Testing compatibility: %s <-> %s',
        $primary->name, $secondary->name
      ));

      # Check for conflicts
      unless (
        $self->conflictDetector->are_generals_compatible($primary, $secondary))
      {
        $self->logger->debug(sprintf(
          'Conflict detected: %s <-> %s',
          $primary->name, $secondary->name
        ));
        next;
      }

      # Find common troop types
      my $common_types = $self->find_common_troop_types($primary, $secondary);
      next unless @$common_types;

      # Create pairs for each common type
      $pairs_added +=
        $self->create_pairs_for_types($primary, $secondary, $common_types);
    }

    return $pairs_added;
  }

  method find_common_troop_types ($primary, $secondary) {
    my $primary_types     = $primary->type   // [];
    my $secondary_types   = $secondary->type // [];
    my %primary_types_map = map  { $_ => 1 } @$primary_types;
    my @common            = grep { $primary_types_map{$_} } @$secondary_types;

    return [sort @common];
  }

  method create_pairs_for_types ($primary, $secondary, $common_types) {
    my $pairs_created = 0;

    my $pair = {
      primary   => $primary->name,
      secondary => $secondary->name,
    };

    for my $type (@$common_types) {
      $self->logger->debug(sprintf(
        'Creating pair: %s <-> %s (type: %s)',
        $pair->{primary}, $pair->{secondary}, $type
      ));

      push @{ $pairs_by_type->{$type} }, $pair;
      $pairs_created++;
    }

    return $pairs_created;
  }

  # Monitor pair building progress and aggregate results
  method monitor_pair_builders ($monitor_job) {
    my $jobs = $monitor_job->app->minion->jobs({
      tasks => ['build_pairs_for_primary']
    });

    $self->logger->info(sprintf(
      'Monitoring %d pair building jobs (monitor job: %s)',
      $jobs->total, $monitor_job->info->{id}
    ));

    my $something_incomplete = 0;
    my $something_failed     = 0;
    my $completed_jobs       = 0;

    # Process each job
    $jobs->each(sub {
      my $info = $_;
      $self->logger->debug(sprintf(
        'Inspecting job %s (state: %s)', $info->{id}, $info->{state}));

      if ($info->{state} eq 'failed') {
        $self->logger->error(sprintf(
          'Pair builder job %s failed: %s',
          $info->{id}, $info->{result} // 'unknown error'
        ));
        $something_failed++;
        return;
      }

      if ($info->{state} eq 'finished') {
        $completed_jobs++;
        $self->merge_job_results($info);
        return;
      }

      # Job is still active or inactive
      $something_incomplete++;
    });

    # Update job notes with current progress
    $monitor_job->note(pairs_by_type => $pairs_by_type);
    $monitor_job->note(
      conflicts => {
        by_general              => $self->conflictDetector->by_general,
        groups_by_conflict_type =>
          $self->conflictDetector->groups_by_conflict_type
      }
    );

    # Determine next action
    if ($something_incomplete > 0) {
      $self->logger->debug(sprintf(
        'Monitor: %d incomplete, %d completed, %d failed - retrying in 30s',
        $something_incomplete, $completed_jobs, $something_failed
      ));
      return $monitor_job->retry({ delay => 30 });
    }

    if ($completed_jobs > 0) {
      $self->logger->info(sprintf(
        'All pair builders complete: %d succeeded, %d failed',
        $completed_jobs, $something_failed
      ));

      # Emit completion signal with final merged results
      $self->app->plugins->emit(pairs_complete => $pairs_by_type);

      return $monitor_job->finish('all pair builders complete');
    }

    # No jobs found - this shouldn't happen
    $self->logger->warn('No pair building jobs found');
    return $monitor_job->finish('no pair building jobs found');
  }

  method merge_job_results ($job_info) {
    my $job_pairs     = $job_info->{notes}->{pairs_by_type} // {};
    my $job_conflicts = $job_info->{notes}->{conflicts}     // {};

    # Merge pairs by type
    foreach my $type (keys %$job_pairs) {
      my @existing_pairs = @{ $pairs_by_type->{$type} // [] };
      my @new_pairs      = @{ $job_pairs->{$type}     // [] };

      # Combine and deduplicate
      my @all_pairs    = (@existing_pairs, @new_pairs);
      my @unique_pairs = uniq_by {
        $_->{primary} . '/' . $_->{secondary}
      }
      @all_pairs;

      $pairs_by_type->{$type} = \@unique_pairs;
    }

    # Merge conflicts
    if (my $by_general = $job_conflicts->{by_general}) {
      $self->conflictDetector->preseed($by_general,
        $job_conflicts->{groups_by_conflict_type} // {});
    }

    $self->logger->debug(sprintf(
      'Merged results from job %s', $job_info->{id}));
  }
}

1;

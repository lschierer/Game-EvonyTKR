use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Pair::BatchSummarizer {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use List::Util qw(min);

  sub task_name {'batch_summarize_pairs'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    return 1;
  }

  sub run ($job, $pairs, $params) {
    $job->SUPER::run();

    # Wait for buff cache to be available
    # pairs needed for residual conflicts
    #
    return
      if $job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_covenants', 'load_all_pair_builders',
        'load_all_generals',  'load_ml_conflicts',
      ]
      );

    $job->log_info(sprintf("Processing batch of %d pairs", scalar @$pairs));

    # Extract unique generals from pairs
    my %unique_generals;
    foreach my $pair (@$pairs) {
      $job->log_debug(sprintf(
        'pair is a "%s".  It is "%s", and dumps as %s',
        ref($pair)     ? ref($pair) : 'scalar',
        blessed($pair) ? 'blessed'  : 'not blessed',
        Data::Printer::np($pair)
      ));
      $pair = Game::EvonyTKR::Model::General::Pair->from_wire_hash($pair);
      $job->log_debug(sprintf(
        'after constructor, is a "%s".  It is "%s", and dumps as %s',
        ref($pair)     ? ref($pair) : 'scalar',
        blessed($pair) ? 'blessed'  : 'not blessed',
        Data::Printer::np($pair)
      ));
      $unique_generals{ $pair->primary->name }   = 1;
      $unique_generals{ $pair->secondary->name } = 1;
    }

    my @general_names = keys %unique_generals;
    $job->log_debug(
      sprintf("Batch contains %d unique generals", scalar @general_names));

    # Build cache keys for all generals with current params
    my $cached_buffs = {};
    foreach my $general_name (@general_names) {
      my $primary_key = $job->generate_buff_cache_key(
        $general_name,                1,
        $params->{targetType},        $params->{activationType},
        $params->{ascendingLevel},    $params->{primaryCovenantLevel},
        $params->{primarySpecialty1}, $params->{primarySpecialty2},
        $params->{primarySpecialty3}, $params->{primarySpecialty4}
      );
      $cached_buffs->{$general_name}->{primary} =
        $job->get_buff_cache($primary_key);

      my $secondary_key = $job->generate_buff_cache_key(
        $general_name,                  0,
        $params->{targetType},          $params->{activationType},
        'none',                         $params->{secondaryCovenantLevel},
        $params->{secondarySpecialty1}, $params->{secondarySpecialty2},
        $params->{secondarySpecialty3}, $params->{secondarySpecialty4}
      );
      $cached_buffs->{$general_name}->{secondary} =
        $job->get_buff_cache($secondary_key);
    }

    # Enqueue individual pair jobs with or without cached buffs
    my $jobs_enqueued = 0;
    my @spawned_job_ids;
    foreach my $pair (@$pairs) {
      my $primary_name   = $pair->primary->name;
      my $secondary_name = $pair->secondary->name;

      $params->{primaryName}   = $primary_name;
      $params->{secondaryName} = $secondary_name;

      # Prepare arguments for pair summarizer
      my @args = ($params);

      # Add cached buffs if available
      my $primary_buffs   = $cached_buffs->{$primary_name}->{primary};
      my $secondary_buffs = $cached_buffs->{$secondary_name}->{secondary};

      if ($primary_buffs || $secondary_buffs) {
        # Add cached buffs as second argument
        push @args,
          {
          primary_buffs   => $primary_buffs,
          secondary_buffs => $secondary_buffs
          };
        $job->log_debug(
          "Enqueueing pair $primary_name/$secondary_name with cached buffs");
      }
      else {
        $job->log_debug(
          "Enqueueing pair $primary_name/$secondary_name without cached buffs");
      }

      # Enqueue individual pair job
      # Use timestamp-based priority to ensure newer sessions process first
      # Priority range is -100 to 100; higher = processes sooner
      # Increment by 1 every 10 seconds from base time, capped at 99
      my $base_time = $job->app->config('APP_START_TIME')
        // 1734000000;    # Dec 12, 2024 reference
      my $priority = min(99, int((time() - $base_time) / 10));

      my $pair_job_id = $job->minion->enqueue(
        'summarize_pair' => \@args => {
          attempts => 3,
          priority => $priority,
          notes    => {
            prebuild_run_id => $job->info->{notes}->{prebuild_run_id},
            batch_job_id    => $job->id
          }
        }
      );

      push @spawned_job_ids, $pair_job_id;
      $jobs_enqueued++;
      # Store spawned job IDs for potential cleanup
      $job->note(spawned_jobs => \@spawned_job_ids);
    }

    $job->log_info("Enqueued $jobs_enqueued pair summarizer jobs");
    return $job->finish("Batch pair processing initiated");
  }
}

1;

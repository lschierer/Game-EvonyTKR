use v5.42.0;
use experimental qw(class);
use utf8::all;

package Game::EvonyTKR::External::LinkChecker::Coordinator;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use List::AllUtils qw(uniq);

sub task_name {'coordinate_link_check'}

sub register_task ($class, $app) {
  $app->minion->add_task($class->task_name => __PACKAGE__);
}

sub run ($job, $start_url, $worker_count = 4) {
  my $all_checked      = {};
  my $all_broken       = {};
  my @pending_sections = ($start_url);
  my @active_jobs;

  while (@pending_sections || @active_jobs) {
    # Start new workers for pending sections
    while (@pending_sections && @active_jobs < $worker_count) {
      my $section = shift @pending_sections;
      next if $all_checked->{$section};

      my $worker_id = $job->minion->enqueue('check_links_section',
        [$section, $all_checked, 2, 0]);
      push @active_jobs, $worker_id;
      $job->app->log->info("Started worker $worker_id for section: $section");
    }

    # Check completed jobs
    @active_jobs = grep {
      my $worker_job = $job->minion->job($_);
      my $state      = $worker_job->info->{state};

      if ($state eq 'finished') {
        my $results = $worker_job->info->{result};

        # Merge results
        $all_checked->%* = ($all_checked->%*, $results->{checked}->%*);
        $all_broken->%*  = ($all_broken->%*,  $results->{broken}->%*);

        # Add external refs as new sections to check
        for my $ext_ref (@{ $results->{external_refs} }) {
          push @pending_sections, $ext_ref unless $all_checked->{$ext_ref};
        }

        $job->app->log->info("Worker $_ completed, found " .
            keys($results->{checked}->%*) . " URLs");
        0;    # Remove from active list
      }
      elsif ($state eq 'failed') {
        $job->app->log->error("Worker $_ failed");
        0;    # Remove from active list
      }
      else {
        1;    # Keep in active list
      }
    } @active_jobs;

    sleep 1 if @active_jobs;    # Brief pause before next check
  }

  # Generate final report
  my $report = {
    total_checked => scalar keys %$all_checked,
    broken_links  => $all_broken,
    summary       => []
  };

  for my $url (sort keys %$all_broken) {
    push @{ $report->{summary} }, "BROKEN: $url (status: $all_broken->{$url})";
  }

  $job->finish($report);
}

1;

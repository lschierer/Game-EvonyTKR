use v5.42.0;
use experimental qw(class);
use utf8::all;

package LinkChecker::DistributedCommand;
use Mojo::Base -base, -signatures;
use Minion;
use HTTP::Tiny;
use HTML::LinkExtor;
use File::HomeDir::Tiny ();
require Path::Tiny;
use URI;
use List::AllUtils qw(uniq);
use List::Util;
use POSIX qw(strftime);
use Fcntl qw(:flock);

has 'start_url';
has 'worker_count' => 4;
has 'delay'        => 0.1;
has 'debug'        => 0;
has 'minion';
has 'shared_state' => sub { { checked => {}, broken => {}, metrics => {} } };
has 'log_file'     => sub { return Path::Tiny::path(File::HomeDir::Tiny::home)->child('var/log/Perl/dist/')->child(__PACKAGE__)->child('linkchecker_access.log'); };

sub init ($self) {
  # Create Minion instance with temporary SQLite file
  my $temp_db = "/tmp/linkchecker_$$.db";
  $self->minion(Minion->new(SQLite => $temp_db));

  my $base_host = URI->new($self->start_url)->host;

  # Ensure log directory exists
  if ($self->log_file =~ m{^(.+)/[^/]+$}) {
    my $log_dir = $1;
    unless (-d $log_dir) {
      mkdir $log_dir or warn "Could not create log directory $log_dir: $!";
    }
  }

  # Clear or create log file
  if (open my $fh, '>', $self->log_file) {
    print $fh "# LinkChecker Access Log - " . localtime() . "\n";
    print $fh "# Format: [timestamp] status duration url_with_fragment\n";
    close $fh;
  }

  # Register the task
  $self->minion->add_task(
    check_url_section => sub ($job, $urls, $depth = 1) {
      $self->_process_urls($job, $urls, $depth, $base_host);
    }
  );
}

sub _log_access ($self, $url, $status, $duration) {
  return unless $self->log_file;

  my $timestamp = strftime("%Y-%m-%d %H:%M:%S", localtime());
  my $log_line = sprintf("[%s] %s %.3fs %s\n",
    $timestamp, $status, $duration, $url);

  # Thread-safe logging with file locking
  if (open my $fh, '>>', $self->log_file) {
    flock($fh, LOCK_EX);
    print $fh $log_line;
    flock($fh, LOCK_UN);
    close $fh;
  }
}

sub execute ($self) {
  say "Starting distributed link check with "
    . $self->worker_count
    . " workers";
  say "Starting URL: " . $self->start_url;
  say "Access log: " . $self->log_file;

  # Start worker processes
  my @worker_pids;
  for my $i (1 .. $self->worker_count) {
    my $pid = fork();
    if ($pid == 0) {
      # Child process - become a worker
      my $worker = $self->minion->worker;
      $worker->run;
      exit;
    }
    else {
      push @worker_pids, $pid;
      say "Started worker process $pid" if $self->debug;
    }
  }

  # Give workers time to start
  sleep 1;

  my %pending_internal =
    ($self->start_url => 0);    # 0=pending, -1=in_progress, 1=done
  my %pending_external = ();
  my @active_jobs;

  while (%pending_internal || %pending_external || @active_jobs) {
    # Start jobs for internal URLs (with recursion)
    while (@active_jobs < $self->worker_count * 2) {
      my @available =
        grep { $pending_internal{$_} == 0 } keys %pending_internal;
      last unless @available;

      my @batch = splice(@available, 0, 5);    # Random 5 due to hash key order
      last unless @batch;

      # Mark as in progress
      $pending_internal{$_} = -1 for @batch;

      my $job_id = $self->minion->enqueue(check_url_section => [\@batch, 1]);
      push @active_jobs, $job_id;
      my $remaining =
        scalar(grep { !$pending_internal{$_} } keys %pending_internal);
      my $msg = sprintf('Queued internal batch job %s: %s. %d jobs remaining.',
        $job_id, join(', ', @batch), $remaining);
      say $msg if $self->debug;
    }

    # Start jobs for external URLs (no recursion)
    while (@active_jobs < $self->worker_count * 2) {
      my @available =
        grep { $pending_external{$_} == 0 } keys %pending_external;
      last unless @available;

      my @batch = splice(@available, 0, 10);
      last unless @batch;

      # Mark as in progress
      $pending_external{$_} = -1 for @batch;

      my $job_id = $self->minion->enqueue(check_url_section => [\@batch, 0]);
      push @active_jobs, $job_id;
      say "Queued external batch job $job_id: " . join(', ', @batch)
        if $self->debug;
    }

    # Check completed jobs
    @active_jobs = grep {
      my $job  = $self->minion->job($_);
      my $info = $job->info;

      if ($info->{state} eq 'finished') {
        my $results = $info->{result};

        # Merge results into shared state
        for my $url (keys $results->{checked}->%*) {
          $self->shared_state->{checked}{$url} = $results->{checked}{$url};
        }
        for my $url (keys $results->{broken}->%*) {
          $self->shared_state->{broken}{$url} = $results->{broken}{$url};
        }
        for my $url (keys $results->{metrics}->%*) {
          $self->shared_state->{metrics}{$url} = $results->{metrics}{$url};
        }

        # Add new URLs to pending queues (filter out already processed)
        for my $url (@{ $results->{new_internal} // [] }) {
          $pending_internal{$url} = 0
            unless exists $pending_internal{$url}
            || $self->shared_state->{checked}{$url};
        }
        for my $url (@{ $results->{new_external} // [] }) {
          $pending_external{$url} = 0
            unless exists $pending_external{$url}
            || $self->shared_state->{checked}{$url};
        }

        # Mark completed URLs as done
        for my $url (keys $results->{checked}->%*) {
          $pending_internal{$url} = 1 if exists $pending_internal{$url};
          $pending_external{$url} = 1 if exists $pending_external{$url};
        }

        say "Job $_ completed: " .
          keys($results->{checked}->%*) . " URLs checked"
          if $self->debug;
        0;    # Remove from active
      }
      elsif ($info->{state} eq 'failed') {
        say "Job $_ failed: " . ($info->{result} // 'Unknown error');
        0;    # Remove from active
      }
      else {
        1;    # Keep active
      }
    } @active_jobs;

    sleep 1;
  }

  # Stop workers and cleanup
  kill 'TERM', @worker_pids;
  waitpid($_, 0) for @worker_pids;

  # Clean up temp database
  my $temp_db = "/tmp/linkchecker_$$.db";
  unlink $temp_db if -f $temp_db;

  $self->_print_results;
}

sub _process_urls ($self, $job, $urls, $depth, $base_host) {
  my $results = {
    checked      => {},
    broken       => {},
    metrics      => {},
    new_internal => [],
    new_external => []
  };

  for my $url_with_fragment (@$urls) {
    # Parse URL to separate fragment from the rest
    my $uri = URI->new($url_with_fragment);
    my $fragment = $uri->fragment;

    # Create URL without fragment for HTTP request
    my $url_no_fragment = $uri->clone;
    $url_no_fragment->fragment(undef);
    my $url_for_request = $url_no_fragment->as_string;

    my $start_time = time();
    my ($status, $size) = $self->_check_url_with_metrics($url_for_request, $base_host);
    my $duration = time() - $start_time;

    # Log with full URL including fragment
    $self->_log_access($url_with_fragment, $status, $duration);

    # Store in results using URL without fragment (to avoid duplicates in state)
    $results->{checked}{$url_with_fragment} = $status;

    # Only collect metrics for internal URLs
    my $url_host = $uri->host // '';
    if ($url_host eq $base_host) {
      $results->{metrics}{$url_with_fragment} = {
        duration => $duration,
        size     => $size,
        status   => $status
      };
    }

    if ($status !~ /^2/) {
      $results->{broken}{$url_with_fragment} = $status;
      next;
    }

    if ($depth > 0) {
      # Only extract links from the page once (use URL without fragment)
      my $links = $self->_extract_links($url_for_request);
      for my $link (@$links) {
        my $link_host = URI->new($link)->host // '';
        if ($link_host eq $base_host) {
          push @{ $results->{new_internal} }, $link;
        }
        else {
          push @{ $results->{new_external} }, $link;
        }
      }
    }

    select(undef, undef, undef, $self->delay) if $self->delay;
  }

  $job->finish($results);
}

sub _check_url_with_metrics ($self, $url, $base_host) {
  my $http =
    HTTP::Tiny->new(timeout => 30, agent => 'LinkChecker-Distributed/1.0');
  my $size = 0;

  for my $attempt (1 .. 3) {
    my $response = $http->get($url);

    # Capture content size for internal URLs
    my $url_host = URI->new($url)->host // '';
    if ($url_host eq $base_host && $response->{content}) {
      $size = length($response->{content});
    }

    return ($response->{status}, $size)
      if $response->{success} || $response->{status} < 500;
    select(undef, undef, undef, 0.5 * $attempt) if $attempt < 3;
  }
  return (500, $size);
}

sub _check_url ($self, $url) {
  my ($status, $size) = $self->_check_url_with_metrics($url, '');
  return $status;
}

sub _extract_links ($self, $url) {
  my $http =
    HTTP::Tiny->new(timeout => 30, agent => 'LinkChecker-Distributed/1.0');
  my $response = $http->get($url);
  return [] unless $response->{success} && $response->{content};

  my $extractor = HTML::LinkExtor->new(undef, $url);
  $extractor->parse($response->{content});

  my @links;
  for my $link_array ($extractor->links) {
    my ($tag, %attrs) = @$link_array;
    my $href = $attrs{href} || $attrs{src};
    next unless $href && $href !~ /^mailto:/;
    push @links, URI->new($href)->abs($url)->as_string;
  }
  return [uniq @links];
}

sub _print_results ($self) {
  my $total         = keys $self->shared_state->{checked}->%*;
  my $broken_count  = keys $self->shared_state->{broken}->%*;
  my $metrics_count = keys $self->shared_state->{metrics}->%*;

  say "\nLink checking complete:";
  say "Total URLs checked: $total";
  say "Internal URLs with metrics: $metrics_count";
  say "Broken links found: $broken_count";
  say "Access log written to: " . $self->log_file;

  if ($broken_count) {
    say "\nBroken links:";
    for my $url (sort keys $self->shared_state->{broken}->%*) {
      my $status = $self->shared_state->{broken}{$url};
      say "  $url (status: $status)";
    }
  }

  if ($metrics_count && $self->debug) {
    say "\nPerformance metrics (internal URLs):";

    # Sort by duration (slowest first) - filter out undefined URLs
    my @valid_urls =
      grep { defined $_ && exists $self->shared_state->{metrics}{$_} }
      keys $self->shared_state->{metrics}->%*;

    return unless @valid_urls;

    my @sorted_urls = sort {
      $self->shared_state->{metrics}{$b}{duration}
        <=> $self->shared_state->{metrics}{$a}{duration}
    } @valid_urls;

    say sprintf("%-8s %-10s %-s", "Duration", "Size(KB)", "URL");
    say "-" x 80;

    my $show_count = @sorted_urls > 20 ? 20 : @sorted_urls;
    for my $i (0 .. $show_count - 1) {
      my $url     = $sorted_urls[$i];
      my $m       = $self->shared_state->{metrics}{$url};
      my $size_kb = sprintf("%.1f", ($m->{size} // 0) / 1024);
      say sprintf("%-8.2fs %-10s %s", $m->{duration} // 0, $size_kb, $url);
    }

    # Summary stats
    my @durations =
      map { $self->shared_state->{metrics}{$_}{duration} // 0 } @sorted_urls;
    my @sizes =
      map { $self->shared_state->{metrics}{$_}{size} // 0 } @sorted_urls;

    if (@durations) {
      my $avg_duration = (List::Util::sum(@durations)) / @durations;
      my $avg_size     = (List::Util::sum(@sizes)) / @sizes;

      say "\nSummary:";
      say sprintf("Average load time: %.2fs",  $avg_duration);
      say sprintf("Average page size: %.1fKB", $avg_size / 1024);
      say sprintf("Slowest page: %.2fs",       $durations[0]  // 0);
      say sprintf("Fastest page: %.2fs",       $durations[-1] // 0);
    }
  }
}

1;

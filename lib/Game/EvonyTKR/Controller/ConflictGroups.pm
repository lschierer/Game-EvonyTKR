use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Service::Cache;
use namespace::autoclean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',            -role;
  use List::AllUtils qw( all any none );
  use Carp;

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';

  sub getBase($self) {
    return $base;
  }

  sub get_conflict_detector {
    state $cd = Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
    );
    return $cd;
  }

  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . __PACKAGE__);
    $c->SUPER::register($app, $config);

    my $routes          = $app->routes->any($base);
    my $controller_name = $c->controller_name();

    $app->helper(
      get_conflict_detector => sub {
        return $c->get_conflict_detector();
      }
    );

    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->add_navigation_item({
      title  => 'General Conflict Groups',
      path   => $base,
      parent => '/Reference/',
      order  => 60,
    });

    Mojo::IOLoop->timer(
      0.01 => sub {
        $c->logger->debug(
          __PACKAGE__ . ' calling schedule_cached_conflicts_merge');
        $c->schedule_cached_conflicts_merge($app);
      }
    );
  }

  sub do_merge_cached_conflicts ($c, $app, $cd, $cc, $merged) {
    $c->logger->debug(sprintf(
      'do_merge_cached_conflicts called with merged: %s',
      Data::Printer::np($merged)));
    if (my $by_general = $merged->{by_general}) {
      while (my ($general, $conflicts) = each %$by_general) {
        $cd->by_general->{$general} =
          { %{ $cd->by_general->{$general} // {} }, %$conflicts };
      }
    }

    if (my $groups = $merged->{groups_by_conflict_type}) {
      while (my ($type, $new_groups) = each %$groups) {
        my $existing = $cd->groups_by_conflict_type->{$type} //= [];
        my %seen     = map { $_ => 1 } @$existing;
        push @$existing, grep { !$seen{$_}++ } @$new_groups;
      }
    }
  }

  sub schedule_cached_conflicts_merge($c, $app) {
    my $cd       = $c->get_conflict_detector();
    my $cc       = $c->conflict_cache();
    my $delay    = 30;
    my $last_run = 0;
    my $found    = 0;

    # Check if prebuild has run at all yet
    my $prebuild_jobs = $app->minion->jobs({
      tasks  => ['external_prebuild'],
      states => ['finished']
    })->total;

    unless ($prebuild_jobs > 0) {
      $c->logger->debug('schedule_cached_conflicts_merge: '
          . 'prebuild not finished yet, retrying in '
          . $delay * 1.5);
      Mojo::IOLoop->timer(
        $delay * 1.5 => sub { $c->schedule_cached_conflicts_merge($app) });
      return;
    }

    my $active_jobs = $app->minion->jobs({
      tasks  => ['reduce_coordinator'],
      states => ['active', 'inactive', 'finished']
    })->each(sub {
      my $info = $_;
      my $job  = $app->minion->job($info->{id});
      if (defined($job) && $job->info->{state} ne 'failed') {
        if (defined($info->{notes}->{processed})
          && scalar(keys($info->{notes}->{processed}->%*)) > 0) {
          if ($info->{state} eq 'finsihed') {
            $c->logger->debug(sprintf(
              'schedule_cached_conflicts_merge found finished '
                . 'reduce_coordinator jid %s with notes %s and result %s',
              $info->{id}, Data::Printer::np($info->{notes}),
              $info->{result}
            ));
            $last_run = 1;
          }
          $found = 1;
          return;
        }
        else {
          $c->logger->debug(sprintf(
            'schedule_cached_conflicts_merge skipping '
              . 'reduce_coordinator jid %s with notes %s',
            $info->{id}, Data::Printer::np($info->{notes})
          ));
        }
      }
      else {
        $c->logger->error(sprintf(
          'schedule_cached_conflicts_merge detects '
            . 'reduce_coordinator %s in error state %s',
          $info->{id}, $info->{result}
        ));
      }
    });

    unless ($found) {
      $c->logger->debug('schedule_cached_conflicts_merge has not '
          . 'found a reduce_coordinator, next run in '
          . $delay * 2);
      my $retry_timer;
      $retry_timer = Mojo::IOLoop->timer(
        $delay * 2 => sub {
          $c->logger->debug(sprintf(
            'retry timer %s fired after %s delay',
            $retry_timer, $delay * 2
          ));
          $c->schedule_cached_conflicts_merge($app);
        }
      );
      $c->logger->debug("Set timer with ID: $retry_timer");
      return;
    }

    # Track what we've already processed to avoid reprocessing
    state $last_processed_timestamp = 0;

    my $cas_val = $c->conflict_cache->gets('merged_conflicts');
    my $merged  = $$cas_val[1];

    unless ($merged || $last_run) {
      if ($merged) {
        $c->do_merge_cached_conflicts($app, $cd, $cc, $merged);
      }
      else {
        $c->logger->warn('schedule_cached_conflicts_merge detects finished '
            . 'reduce_coordinator when $merged is undefined');
      }
      $c->logger->info(
        sprintf('schedule_cached_conflicts_merge complete for %s', __PACKAGE__)
      );
      return;
    }

    $c->logger->debug(sprintf(
      'schedule_cached_conflicts_merge has cas_val %s and merged %s',
      Data::Printer::np($cas_val),
      Data::Printer::np($merged)
    ));

    my $current_timestamp = $merged->{timestamp} // 0;
    unless ($current_timestamp <= $last_processed_timestamp) {
      $last_processed_timestamp = $current_timestamp;
      $c->logger->debug(
        'schedule_cached_conflicts_merge calling do_merge_cached_conflicts');
      # Process only new data efficiently
      $c->do_merge_cached_conflicts($app, $cd, $cc, $merged);
    }
    else {
      $c->logger->debug(
"$current_timestamp <= $last_processed_timestamp or undefined merged hash."
      );
    }

    # Check jobs less frequently and with shorter timeout
    if ($last_run == 0) {
      $c->logger->debug(
        'schedule_cached_conflicts_merge will rerun in ' . $delay);
      Mojo::IOLoop->timer(
        $delay => sub { $c->schedule_cached_conflicts_merge($app) });
    }
    else {
      $c->logger->debug(
'schedule_cached_conflicts_merge will not run again, last_run was detected.'
      );
    }
  }

  sub index ($c) {
    $c->logger->debug("Rendering conflict groups index");

    my $detector = $c->get_conflict_detector();
    $c->logger->debug(sprintf('there are %s generals in the by_general index',
      scalar keys $detector->by_general->%*));
    my $groups = $detector->groups_by_conflict_type;
    my $pairs  = $detector->by_general;

    $c->stash(
      groups   => $groups,
      pairs    => $pairs,
      linkBase => $base,
    );

    return $c->render(template => '/general conflict groups/index');
  }
}
1;
__END__

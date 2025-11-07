use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Service::Cache;
use namespace::autoclean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use List::AllUtils qw( all any none );
  use Carp;

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';

  sub getBase($self) {
    return $base;
  }

  has 'pair_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs:');
  };

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts:');
  };

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
    $c->logger->info("Registering routes for " . ref($c));
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

    $c->schedule_cached_coflicts_merge($app);
  }

  sub do_merge_cached_conflicts ($c, $app, $cd, $cc, $merged) {
    if (my $by_general = $merged->{by_general}) {
      while (my ($general, $conflicts) = each %$by_general) {
        $cd->by_general->{$general} = { %{$cd->by_general->{$general} // {}}, %$conflicts };
      }
    }

    if (my $groups = $merged->{groups_by_conflict_type}) {
      while (my ($type, $new_groups) = each %$groups) {
        my $existing = $cd->groups_by_conflict_type->{$type} //= [];
        my %seen = map { $_ => 1 } @$existing;
        push @$existing, grep { !$seen{$_}++ } @$new_groups;
      }
    }
  }

  sub schedule_cached_coflicts_merge($c, $app) {
    my $cd = $c->get_conflict_detector();
    my $cc = $c->conflict_cache();

    my $active_jobs = $app->minion->jobs({
      tasks  => ['external_prebuild', 'monitor_create_pairs', 'create_pairs'],
      states => ['active', 'inactive']
    })->total;

    # Track what we've already processed to avoid reprocessing
    state $last_processed_timestamp = 0;

    my ($cas_token, $merged) = $cc->gets('merged_conflicts');
    unless ($merged || $active_jobs) {
      # merged will not be set for quite a while,
      # as about 600 jobs have to complete before
      # the first conflict iformation is available.
      # Do not return early unless there is both
      # a lack of data to process and a lack of
      # jobs to give us data in the future.
      return;
    };

    my $current_timestamp = $merged->{timestamp} // 0;
    unless($current_timestamp <= $last_processed_timestamp) {
      $last_processed_timestamp = $current_timestamp;

      # Process only new data efficiently
      $c->do_merge_cached_conflicts($app, $cd, $cc, $merged);
    }

    # Check jobs less frequently and with shorter timeout

    if ($active_jobs > 0) {
      Mojo::IOLoop->timer(60 => sub { $c->schedule_cached_coflicts_merge($app) });
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

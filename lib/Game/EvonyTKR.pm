use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Minion::Backend::SQLite;
require Mojolicious::Plugin::Minion;

#require Game::EvonyTKR::Controller::Root;
require Game::EvonyTKR::Controller::ControllerBase;
require Game::EvonyTKR::External::JobBase;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious',                   -strict, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role,   -signatures;
  use Log::Any::Adapter;
  use Log::Log4perl;
  use Mojo::File::Share qw(dist_dir );
  use Mojo::Loader      qw(find_modules load_class);
  use Fcntl             qw(:flock);
  use Mojo::File;
  use POSIX qw(setsid);
  use Scalar::Util 'weaken';
  use Carp;
  use diagnostics;
  use Env qw(DEPLOYMENT_TIME HOSTNAME );
  our $VERSION = 'v0.50.0';

  BEGIN {
    Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
  }

  sub startup ($app) {
    # Set up Log::Any adapter BEFORE accessing $app->logger
    Log::Any::Adapter->set('Log4perl');
    $app->plugin('Log::Any' => { logger => 'Log::Log4perl' });
    $app->log_debug('setting up logging');

    # Debug: Why is startup() being called?
    my $is_minion = _this_is_a_minion_process();
    my $parent_pid = getppid();
    $app->log->info(sprintf(
      'Mojolicious Logging initialized for process "%s" (parent: %s, is_minion: %s, MINION_WORKER_CHILD: %s)',
      $$, $parent_pid, $is_minion ? 'YES' : 'NO', $ENV{MINION_WORKER_CHILD} // 'unset'
    ));

    _init_core($app);    # runs in web *and* worker
    _init_minion($app);

    # web-only: routes/UI and optional worker spawning
    $app->hook(
      before_server_start => sub ($server, $app) {

        # routes, UIs, helpers that need HTTP server
        _init_web($app);
        # optional: only if you want web proc to fork workers
        Mojo::IOLoop->timer(
          1 => sub {
            my $is_web = _this_proc_is_a_web_server($server);
            my $is_spawner = _i_am_the_one_spawner($app);
            my $is_minion = _this_is_a_minion_process();

            $app->log->debug(sprintf(
              'Worker spawn check in PID %s: is_web=%s, is_spawner=%s, is_minion=%s',
              $$, $is_web ? 'YES' : 'NO', $is_spawner ? 'YES' : 'NO', $is_minion ? 'YES' : 'NO'
            ));

            return unless $is_web; # has acceptors?
            return unless $is_spawner;         # spawn once only
            return if $is_minion; # don't spawn from minion cmd

            $app->log->info("SPAWNING MINION WORKERS from PID $$");
            _spawn_minion_workers($app);
          }
        );
      }
    );

    # optional: worker lifecycle breadcrumbs
    Mojo::IOLoop->next_tick(sub {
      $app->plugins->emit(mojo_worker_started => { app => $app });
    });
  }

  sub _this_proc_is_a_web_server ($server) {
    # Minion commands and plain perl procs have no HTTP acceptors
    my $acceptors =
      eval { $server->can('acceptors') ? scalar @{ $server->acceptors } : 0 }
      // 0;
    return $acceptors > 0;
  }

  sub _this_is_a_minion_process {
    return 1 if $ENV{MINION_WORKER_CHILD};
    return 1 if ($ENV{MOJO_COMMAND} && $ENV{MOJO_COMMAND} eq 'minion');
    # belt-and-suspenders: if started like `script/app minion worker ...`
    return 1 if grep { $_ eq 'minion' } @ARGV;
    return 0;
  }

  sub _i_am_the_one_spawner ($app) {
    # File lock so only one preforked web worker “wins”
    state $fh;
    $fh //= Mojo::File->new($app->home->child('spawn_minion.lock'))->open('>>');
    return 0 unless $fh;
    return
      flock($fh, LOCK_EX | LOCK_NB)
      ;    # true only in the first process that grabs it
  }

  sub _init_core ($app) {
    my $distDir = dist_dir('Game::EvonyTKR');
    my $mode    = $app->mode;
    my $home    = Mojo::Home->new->detect;
    Env::import();

    my $config = $app->plugin('NotYAMLConfig' => { module => 'YAML::PP' });
    $app->config(distDir        => $distDir);
    $app->config(mode           => $app->mode);
    $app->config(APP_START_TIME => time());
    $app->config(
      'EvonyTKR-Environment' => {
        DEPLOYMENT_TIME => $DEPLOYMENT_TIME // 'unknown',
        HOSTNAME        => $HOSTNAME  // `hostname`,
      }
    );

    $app->secrets($config->{secrets});

    if(_this_is_a_minion_process()){
      foreach my $envkey (keys %{ $app->config->{'EvonyTKR-Environment'} }) {
        if (defined $envkey) {
          my $envValue = $app->config->{'EvonyTKR-Environment'}->{$envkey}
            // 'Undefined';
          $app->log->info("EvonyTKR-Environnment variable $envkey is $envValue");
        }
        else {
          $app->log->warn('undefined envkey in EvonyTKR-Environment!');
        }
      }
    }

    push @{ $app->plugins->namespaces }, 'Game::EvonyTKR::Plugins';
  }

  sub _init_minion($app) {
    # this defines helpers needed by both the web and worker processes.
    #
    $app->plugin('Game::EvonyTKR::Plugins::Sqlite');

    # Apply PRAGMAs for ALL future connections first
    my $sqlite = $app->minion->backend->sqlite;
    $sqlite->on(
      connection => sub ($sqlite, $dbh) {
        $dbh->do('PRAGMA journal_mode=WAL');
        $dbh->do('PRAGMA synchronous=NORMAL');
        $dbh->do('PRAGMA temp_store=MEMORY');
        $dbh->do('PRAGMA foreign_keys=ON');
        $dbh->do('PRAGMA busy_timeout=8000');
        # Disable memory-mapped I/O for compatibility with EBS volumes
        $dbh->do('PRAGMA mmap_size=0');
        # Ensure normal locking mode (not exclusive)
        $dbh->do('PRAGMA locking_mode=NORMAL');
        # Increase cache size for better performance
        $dbh->do('PRAGMA cache_size=-64000');    # 64MB cache
      }
    );

    # Now it's safe to open a handle
    my $db = $sqlite->db;
    $db->ping;

    # Run migrations/repair ONLY in the
    # web parent (not in forked or exec'd workers)
    my $is_worker_child = $ENV{MINION_WORKER_CHILD};
    my $is_minion_cmd   = ($0 =~ /minion(?:\.pl)?$/i)
      || ($ENV{MOJO_COMMAND} && $ENV{MOJO_COMMAND} eq 'minion');

    unless ($is_worker_child || $is_minion_cmd) {
      $sqlite->migrations->name('evonytkr')
        ->from_data('Game::EvonyTKR', 'migrations')
        ->migrate;

      $app->minion->repair;
      $app->ensure_lock_table_sqlite($db);
    }

    if ($app->mode eq 'development') {
      $app->minion->remove_after(7200);
    }

    my @task_plugins = find_modules 'Game::EvonyTKR::External',
      { recursive => 1 };
    foreach my $module (sort @task_plugins) {
      # make sure Prebuild loads last.
      next if ($module eq 'Game::EvonyTKR::External::Prebuild');
      if (my $e = load_class($module)) {
        my $errmessage = sprintf('loading module "%s" failed: %s', $module, $e);
        print STDERR $errmessage;
        $app->log->error($errmessage);
        croak($errmessage);
      }
      next if ($module eq 'Game::EvonyTKR::External::Common');
      next if ($module eq 'Game::EvonyTKR::External::JobBase');
      $app->plugin($module);
    }
    my $module = 'Game::EvonyTKR::External::Prebuild';
    if (my $e = load_class($module)) {
      my $errmessage = sprintf('loading module "%s" failed: %s', $module, $e);
      print STDERR $errmessage;
      $app->log->error($errmessage);
      croak($errmessage);
    }
    $app->plugin($module);
  }

  sub _init_web ($app) {
    my $processmsg = "Detected non-minion process '$0' at pid $$";
    $app->log->info($processmsg);
    say $processmsg;

    my $distDir = dist_dir('Game::EvonyTKR');

    # Template and static paths
    push @{ $app->renderer->paths }, $distDir->child('templates')->to_string;
    push @{ $app->static->paths },   $distDir->child('public')->to_string;

    $app->plugin('DefaultHelpers');
    $app->defaults(layout => 'default');

    push @{ $app->routes->namespaces },  'Game::EvonyTKR::Controller';
    push @{ $app->plugins->namespaces }, 'Game::EvonyTKR::Controller';
    push @{ $app->preload_namespaces },  'Game::EvonyTKR::Controller';

    # Navigation
    eval { $app->plugin('Game::EvonyTKR::Plugins::Navigation'); } or do {
      $app->log->error('Failed to load Navigation Plugin');
      croak('Failed to load Navigation Plugin');
    };

    my @controllerplugins = find_modules 'Game::EvonyTKR::Controller';
    $app->log_info(
      sprintf('found %s controller plugins', scalar(@controllerplugins)));
    foreach my $module (@controllerplugins) {
      if (my $e = load_class($module)) {
        my $errmessage = sprintf('loading module "%s" failed: %s', $module, $e);
        print STDERR $errmessage;
        $app->log->error($errmessage);
        croak($errmessage);
      }
      else {
        next if ($module eq 'Game::EvonyTKR::Controller::ControllerBase');
        eval {
          $app->plugin($module);
          $app->log_debug("loaded $module");
        } or do {
          $app->log_error(sprintf('failed to load module %s: %s', $module, $@));
        }
      }
    }

    if ($app->mode eq 'development') {
      # start the web UI for debugging
      $app->plugin('Minion::Admin');
    }
  }

  my %WORKER_PIDS;

  sub _spawn_minion_workers ($app) {
    return if $ENV{MINION_WORKER_CHILD};
    my $start_workers = $ENV{START_MINION_WORKERS} // 1;
    # Production defaults for T4G Large (2 vCPUs):
    # 2 workers × 1 job = 2 concurrent processes (matches CPU count)
    # Development: More aggressive for local multi-core machines
    my $worker_count  = $ENV{MINION_WORKERS}       // ($app->mode eq 'development' ? 4 : 2);
    my $job_count     = $ENV{MINION_JOB_COUNT}
      // $app->mode eq 'development' ? 5 : 1;
    return unless $start_workers;

    for (1 .. $worker_count) {
      my $pid = fork // die "fork failed: $!";
      if ($pid) { $WORKER_PIDS{$pid} = 1; next }

      # --- child path ---
      $ENV{MINION_WORKER_CHILD} = 1;    # prevents recursion on load
      POSIX::nice(10);
      exec($^X, $0, 'minion', 'worker', '-j', $job_count)
        or die "exec failed: $!";
    }

    # Reap *our* children periodically (doesn't interfere with Mojo/Hypnotoad)
    Mojo::IOLoop->recurring(
      1 => sub {
        while ((my $kid = waitpid(-1, POSIX::WNOHANG)) > 0) {
          delete $WORKER_PIDS{$kid};
        }
      }
    );
  }

  # Optional: graceful stop on normal shutdown (no signal handlers needed)
  END {
    return unless %WORKER_PIDS;
    kill 'TERM', keys %WORKER_PIDS;
    my $deadline = time + 10;
    while (%WORKER_PIDS && time < $deadline) {
      while ((my $kid = waitpid(-1, POSIX::WNOHANG)) > 0) {
        delete $WORKER_PIDS{$kid};
      }
      select undef, undef, undef, 0.1;
    }
    kill 'KILL', keys %WORKER_PIDS if %WORKER_PIDS;
  }
};

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut

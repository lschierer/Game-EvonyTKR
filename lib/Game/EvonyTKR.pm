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
require Game::EvonyTKR::Log::Config;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -strict, -signatures;
  use Log::Any::Adapter;
  use Log::Log4perl;
  use Mojo::Base 'Game::EvonyTKR::Log::Config', -role, -signatures;
  use Mojo::File::Share qw(dist_dir );
  use Mojo::Loader      qw(find_modules load_class);
  use Fcntl             qw(:flock);
  use Mojo::File;
  use POSIX qw(setsid);
  use Scalar::Util 'weaken';
  use Carp;
  use diagnostics;
  use Env qw(DEPLOYMENT_TIME HOSTNAME IMAGE_TAG IMAGE_URI);
  our $VERSION = 'v0.50.0';


  sub startup ($app) {
    $app->logger->debug('setting up logging');
    Log::Any::Adapter->set('Log4perl');
    $app->plugin('Log::Any' => { logger => 'Log::Log4perl' });
    $app->log->info(sprintf('Mojolicious Logging initialized'));

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
            return unless _this_proc_is_a_web_server($server); # has acceptors?
            return unless _i_am_the_one_spawner($app);         # spawn once only
            return if _this_is_a_minion_process(); # don't spawn from minion cmd

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
    my $config  = $app->plugin('NotYAMLConfig' => { module => 'YAML::PP' });
    my $distDir = dist_dir('Game::EvonyTKR');
    my $mode    = $app->mode;
    Env::import();
    $app->config(distDir        => $distDir);
    $app->config(APP_START_TIME => time());
    $app->config(
      'EvonyTKR-Environment' => {
        DEPLOYMENT_TIME => $DEPLOYMENT_TIME,
        HOSTNAME        => $HOSTNAME,
        IMAGE_TAG       => $IMAGE_TAG,
        IMAGE_URI       => $IMAGE_URI,
      }
    );
    my $home = Mojo::Home->new->detect;
    $app->secrets($config->{secrets});
    # Logging setup

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
      }
    );

    # Now it's safe to open a handle
    my $db = $sqlite->db;
    $db->ping;

# Run migrations/repair ONLY in the web parent (not in forked or exec'd workers)
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

    my @task_plugins = find_modules 'Game::EvonyTKR::External', {recursive => 1};
    foreach my $module (@task_plugins) {
      if(my $e = load_class($module)){
        my $errmessage = sprintf('loading module "%s" failed: %s', $module, $e);
        print STDERR $errmessage;
        $app->log->error($errmessage);
        croak($errmessage);
      }
      next if ($module eq 'Game::EvonyTKR::External::Common');
      next if ($module eq 'Game::EvonyTKR::External::JobBase');
      $app->plugin($module);
    }


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
    foreach my $module (@controllerplugins) {
      if(my $e = load_class($module)){
        my $errmessage = sprintf('loading module "%s" failed: %s', $module, $e);
        print STDERR $errmessage;
        $app->log->error($errmessage);
        croak($errmessage);
      }
    }

    # Last the Static Pages
    # Register last for lowest priority
    $app->plugin('Game::EvonyTKR::Plugins::StaticPages');

    if ($app->mode eq 'development') {
      # start the web UI for debugging
      $app->plugin('Minion::Admin');
    }
  }

  my %WORKER_PIDS;

  sub _spawn_minion_workers ($app) {
    return if $ENV{MINION_WORKER_CHILD};
    my $start_workers = $ENV{START_MINION_WORKERS} // 1;
    my $worker_count  = $ENV{MINION_WORKERS}       // 4;
    return unless $start_workers;

    for (1 .. $worker_count) {
      my $pid = fork // die "fork failed: $!";
      if ($pid) { $WORKER_PIDS{$pid} = 1; next }

      # --- child path ---
      $ENV{MINION_WORKER_CHILD} = 1;    # prevents recursion on load
      POSIX::nice(10);
      exec($^X, $0, 'minion', 'worker', '-j', '5') or die "exec failed: $!";
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

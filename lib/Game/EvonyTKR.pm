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

require GitRepo::Reader;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -strict, -signatures;
  use Log::Any::Adapter;
  use Log::Log4perl;
  use Mojo::File::Share qw(dist_dir );
  use Mojo::Loader      qw(find_modules load_class);
  use POSIX             qw(setsid);
  use Scalar::Util 'weaken';
  use Carp;
  use Env qw(DEPLOYMENT_TIME HOSTNAME IMAGE_TAG IMAGE_URI);
  our $VERSION = 'v0.50.0';

  my $l4p;

  sub startup ($app) {
    _init_core($app);      # runs in web *and* worker
    _init_minion($app);    # runs in web *and* worker (tasks visible to workers)

    # web-only: routes/UI and optional worker spawning
    $app->hook(
      before_server_start => sub ($server, $app) {
        _init_web($app);    # routes, UIs, helpers that need HTTP server
        _spawn_minion_workers($app)
          ;    # optional: only if you want web proc to fork workers
      }
    );

    # optional: worker lifecycle breadcrumbs
    Mojo::IOLoop->next_tick(sub {
      $app->plugins->emit(mojo_worker_started => { app => $app });
    });
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
    $l4p = Game::EvonyTKR::Log::Config->logger();
    Log::Any::Adapter->set('Log4perl');

    $app->plugin('Log::Any' => { logger => 'Log::Log4perl' });

    $app->log->info(sprintf('Mojolicious Logging initialized'));

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
    # First Plugins that provide helpers but do not define routes
    my $dbPath = Mojo::File->new('minion.db');

    $app->log->debug("dbPath is $dbPath");
    $app->plugin(
      Minion => {
        SQLite => "sqlite:$dbPath?"
          . 'sqlite_use_immediate_transaction=1&busy_timeout=30000',
      }
    );

    my $db = $app->minion->backend->sqlite->db;
    $app->minion->backend->sqlite->on(
      connection => sub ($sqlite, $dbh) {
        $dbh->do('PRAGMA journal_mode=WAL');
        $dbh->do('PRAGMA synchronous=NORMAL');
        $dbh->do('PRAGMA temp_store=MEMORY');
        $dbh->do('PRAGMA foreign_keys=ON');
        $dbh->do('PRAGMA busy_timeout=8000');
      }
    );
    $db->ping;    # Blocks until backend is ready

    $app->minion->backend->sqlite->migrations->name('evonytkr')
      ->from_data('Game::EvonyTKR', 'migrations')
      ->migrate;

    $app->minion->repair;
    $app->plugin('Game::EvonyTKR::Plugins::Sqlite');

    $app->ensure_lock_table_sqlite($db);

    if ($app->mode eq 'development') {
      # no long lived jobs in case I forget ot erase the sqlite file
      $app->minion->remove_after(7200);
    }
    $app->plugin('Game::EvonyTKR::External::Prebuild');
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

    my $RepoData = GitRepo::Reader->new(source_dir => $distDir,);

    $app->helper(get_repo_data => sub { return $RepoData });

    push @{ $app->routes->namespaces },  'Game::EvonyTKR::Controller';
    push @{ $app->plugins->namespaces }, 'Game::EvonyTKR::Controller';
    push @{ $app->preload_namespaces },  'Game::EvonyTKR::Controller';

    $app->plugin('Game::EvonyTKR::Plugins::Markdown');
    # Navigation
    eval { $app->plugin('Game::EvonyTKR::Plugins::Navigation'); } or do {
      $l4p->logcroak('Failed to load Navigation Plugin');
    };

    my @controllerplugins = find_modules 'Game::EvonyTKR::Controller';
    foreach my $module (@controllerplugins) {
      eval {
        load_class $module;
        $app->plugin($module);
      };
      if ($@) {
        print STDERR "Error caught loading module: $@";
        $l4p->logcroak("loading module '$module' failed: $@");
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

  sub _spawn_minion_workers ($app) {
    my $start_workers = $ENV{START_MINION_WORKERS} // 1;
    my $worker_count  = $ENV{MINION_WORKERS}       // 4;
    my @worker_pids;

    # ---- Fork workers after DB is ready ----
    if ($start_workers && !$ENV{MINION_WORKER_CHILD}) {
      for my $i (1 .. $worker_count) {
        my $pid = fork();
        defined $pid or die "fork failed: $!";
        if ($pid) { push @worker_pids, $pid; next }

        # child
        $ENV{MINION_WORKER_CHILD} = 1;
        # (optional) remove setsid() so Ctrl-C hits the whole group naturally:
        # setsid();  # <- comment this OUT so parent can send signals to group
        POSIX::nice(10);

        # Pick a deterministic concurrency if you want (default may be high)
        # Replace 5 with what you expect for each child
        exec($^X, $0, 'minion', 'worker', '-j', '5') or die "exec failed: $!";
      }

      # Reap children automatically
      $SIG{CHLD} = 'IGNORE';

      # Graceful shutdown: send TERM to the process group (includes workers)
      # Make *this* script the group leader, then kill negative PGID on exit.
      my $pgid = $$;
      setpgrp(0, 0);    # become group leader
          # Catch Ctrl-C , TERM and QUIT (all 3 appear to be necessary)
      $SIG{INT}  = sub { $app->stop_all($pgid) };
      $SIG{TERM} = sub { $app->stop_all($pgid) };
      $SIG{QUIT} = sub { $app->stop_all($pgid) };
    }
  }

  sub stop_all ($app, $pgid) {
    # First try TERM (graceful), then KILL after a grace period
    kill 'TERM', -$pgid;
    my $deadline = time + 30;
    while (time < $deadline) { select undef, undef, undef, 0.1 }
    kill 'KILL', -$pgid;
    exit 0;
  }
};

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut

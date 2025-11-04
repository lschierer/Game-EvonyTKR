use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders;
require Game::EvonyTKR::External::General::Pair::CreatePairs;
require Game::EvonyTKR::External::General::Pair::MonitorCreatePairs;
require Game::EvonyTKR::External::General::Loader;
require Game::EvonyTKR::External::General::LoadAll;
require Game::EvonyTKR::External::Book::Loader;
require Game::EvonyTKR::External::Book::LoadAllBuiltins;
require Game::EvonyTKR::External::Book::LoadAllGenerics;
require Game::EvonyTKR::External::Specialty::Loader;
require Game::EvonyTKR::External::Specialty::LoadAllSpecialties;

package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Home;
  use Mojo::File;
  use POSIX 'strftime';
  use Time::HiRes 'time';
  use experimental qw(class);
  use Carp;

  state $OnlyOnePrebuild = 0;

  state $generalCache;
  state $prereqs = {};

  sub register ($plugin, $app, $conf = {}) {
    if (not defined $plugin) {
      say '$plugin ont defined in register for ' . __PACKAGE__ . $$;
      return;
    }
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__ . $$;
      say $errmessage;
      return;
    }
    $plugin->SUPER::register($app, $conf);

    my $db = $app->minion->backend->sqlite->db;
    ensure_lock_table_sqlite($db);

    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $plugin->logger->error($errmessage);
      say $errmessage;
      return;
    }
    $plugin->logger->debug(
      sprintf('register function for "%s" %s', __PACKAGE__, $$));

    # Register main prebuild orchestration task
    $app->minion->add_task(external_prebuild => __PACKAGE__);
    my $plugins = [
      'Game::EvonyTKR::External::Book::Loader',
      'Game::EvonyTKR::External::Book::LoadAllBuiltins',
      'Game::EvonyTKR::External::Book::LoadAllGenerics',
      'Game::EvonyTKR::External::General::Loader',
      'Game::EvonyTKR::External::General::LoadAll',
      'Game::EvonyTKR::External::General::Pair::CreatePairs',
      'Game::EvonyTKR::External::General::Pair::LoadAllPairBuilders',
      'Game::EvonyTKR::External::General::Pair::MonitorCreatePairs',
      'Game::EvonyTKR::External::Specialty::Loader',
      'Game::EvonyTKR::External::Specialty::LoadAllSpecialties',
    ];

    my @tasks = values $app->minion->tasks->%*;
    foreach my $task (@tasks) {
      $plugin->logger->debug(sprintf('task is %s, %s',
        ref($task) // 'undef ref',
        blessed($task) // 'undef blessed'));
    }
    foreach my $prereq ($plugins->@*) {
      $prereqs->{$prereq} = 0;
      my $signal = $prereq =~ s/::/_/gr;
      $app->plugins->on(
        $signal => sub {
          $plugin->logger->info(sprintf('detected %s ready', $prereq));
          return $plugin->prebuildPrerequisites({ $prereq => 1 });
        }
      );
      eval { $app->plugin($prereq); } or do {
        $plugin->logger->error("Error loading task plugin $prereq: $@");
      };

    }

    $plugin->prebuild_init($app);

    $plugin->logger->info(
      sprintf('%s register function complete for %s', __PACKAGE__, $$));
  }

  sub now_epoch () { int(time()) }

  sub ensure_lock_table_sqlite ($db) {
    $db->query(q{
      CREATE TABLE IF NOT EXISTS app_locks (
        name        TEXT PRIMARY KEY,
        owner       TEXT NOT NULL,
        expires_at  INTEGER NOT NULL,
        updated_at  INTEGER NOT NULL
      )
    });
    $db->query(q{
      CREATE INDEX IF NOT EXISTS app_locks_expires_idx
      ON app_locks (expires_at)
    });
  }

  sub try_acquire_lock_sqlite ($db, $name, $owner, $ttl_s) {
    my $now = now_epoch();
    my $exp = $now + $ttl_s;

    # 1) Try to insert new lock
    my $res = eval {
      $db->query(
        'INSERT OR IGNORE INTO app_locks(name, owner, expires_at, updated_at)
         VALUES (?, ?, ?, ?)',
        $name, $owner, $exp, $now
      );
      1;
    };
    return 1 if $res && $db->dbh->rows;    # inserted → we own it

    # 2) If exists, try to take over only if stale
    $res = $db->query(
      'UPDATE app_locks
         SET owner = ?, expires_at = ?, updated_at = ?
       WHERE name = ? AND expires_at < ?',
      $owner, $exp, $now, $name, $now
    );
    return $res->rows > 0;                 # true if we stole a stale lock
  }

  sub refresh_lock_sqlite ($db, $name, $owner, $ttl_s) {
    my $now = now_epoch();
    my $exp = $now + $ttl_s;
    # Only the current owner may refresh
    my $res = $db->query(
      'UPDATE app_locks
          SET expires_at = ?, updated_at = ?
        WHERE name = ? AND owner = ?',
      $exp, $now, $name, $owner
    );
    return $res->rows > 0;
  }

  sub release_lock_sqlite ($db, $name, $owner) {
    my $res = $db->query('DELETE FROM app_locks WHERE name = ? AND owner = ?',
      $name, $owner);
    return $res->rows > 0;
  }

  sub prebuild_init ($plugin, $app) {
    # wait for prerequisites...
    while (!$plugin->prebuildPrerequisites) { sleep 5 }

    if (my $g =
      $app->minion->guard('external_prebuild:bootstrap', 15, { limit => 1 })) {
      # only the guard holder gets here
      my $existing = $app->minion->jobs({
        tasks  => ['external_prebuild'],
        states => [qw(inactive delayed active)]
      })->total;

      unless ($existing) {
        my $jid = $app->minion->enqueue(
          'external_prebuild' => [{}] => {
            priority => 100,
            attempts => 3,
            expire   => 7200,
            notes    => { uniq => 'external_prebuild' },
          }
        );
        $plugin->logger->info("Queued external_prebuild $jid");
      }
    }
  }

  sub prebuildPrerequisites ($plugin, $args = {}) {
    foreach my $key (keys $args->%*) {
      $prereqs->{$key} = $args->{$key};
    }

    if (List::AllUtils::none { $_ == 0 } values $prereqs->%*) {
      return 1;
    }
    $plugin->logger->debug(
      sprintf('failed prebuildPrerequisites: %s',
        Data::Printer::np($prereqs, multiline => 0))
    );
    return 0;
  }

  # Main prebuild orchestration job
  sub run ($job, @args) {
    if (not defined($job)) {
      say '$job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    else {
      $job->logger->debug(sprintf(
        'minion in %s is a %s;%s',
        __PACKAGE__, ref($job->minion), blessed($job->minion)
      ));
    }
    $job->logger->debug('Prebuild orchestration starting');

    my $owner = $$ . '@' . ($ENV{HOSTNAME} // 'localhost');
    my $key   = 'prebuild_run';
    my $ttl   = 60;                                         # lock TTL (seconds)
    my $refresh_every = 20;                                 # heartbeat interval
    my $db            = $job->minion->backend->sqlite->db;
    my $pair_monitor_jid;

    unless (try_acquire_lock_sqlite($db, $key, $owner, $ttl)) {
      $job->note(skipped => 'another prebuild is running');
      return $job->finish('skipped');
    }

    my $alive = 1;
    local $SIG{TERM} = sub { $alive = 0 };
    my $timer_id;
    $timer_id = Mojo::IOLoop->recurring(
      $refresh_every => sub {
        refresh_lock_sqlite($db, $key, $owner, $ttl) or do {
          $alive = 0;
          $job->logger->error('Lost runtime lock; stopping prebuild.');
          Mojo::IOLoop->remove($timer_id) if $timer_id;
          release_lock_sqlite($db, $key, $owner); # safe even if we don’t own it
          $job->fail('lost lock');
        };
      }
    );

    # Import Books
    state $book_import_started = 0;
    # --- Import Books (replaces your existing block) ---
    my ($book_loop, $jid_generic, $jid_builtin);

    $book_loop = Mojo::IOLoop->recurring(
      5 => sub {
        # don’t start until prereqs pass
        unless ($job->prebuildPrerequisites) {
          $job->logger->debug(sprintf('cannot start books import prereqs: %s',
            Data::Printer::np($prereqs, multiline => 0)));
          return;
        }

        # enqueue once
        unless ($jid_generic && $jid_builtin) {
          $job->logger->info('launching load_all_generic_books');
          $jid_generic = $job->minion->enqueue(
            load_all_generic_books => [] => {
              attempts => 3,
              delay    => 1,
              expire   => 300,
              priority => 50,
            }
          );
          $job->logger->info('launching load_all_builtin_books');
          $jid_builtin = $job->minion->enqueue(
            load_all_builtin_books => [] => {
              attempts => 3,
              delay    => 1,
              expire   => 300,
              priority => 60,
            }
          );

          $job->note(
            book_jids => { generic => $jid_generic, builtin => $jid_builtin });
          $job->app->log->info(
            "Queued book imports generic=$jid_generic builtin=$jid_builtin");
          return;    # let the next tick do the monitoring
        }

        # monitor both
        my $jg = $job->minion->job($jid_generic);
        my $jb = $job->minion->job($jid_builtin);

        my $sg = $jg && $jg->info ? $jg->info->{state} : 'unknown';
        my $sb = $jb && $jb->info ? $jb->info->{state} : 'unknown';

        # bubble up minimal progress to the dashboard
        $job->note(book_status => { generic => $sg, builtin => $sb });

        # stop once both are terminal
        my %terminal = map { $_ => 1 } qw(finished failed);
        if ($terminal{$sg} && $terminal{$sb}) {
          Mojo::IOLoop->remove($book_loop);
          $book_loop = undef;

          # optional: fail early if any failed; otherwise continue prebuild
          if ($sg eq 'failed' || $sb eq 'failed') {
            my $eg =
              $jg && $jg->info
              ? ($jg->info->{result} // $jg->info->{notes}{error})
              : undef;
            my $eb =
              $jb && $jb->info
              ? ($jb->info->{result} // $jb->info->{notes}{error})
              : undef;
            my $msg =
                "book import failed"
              . ($eg ? " (generic: $eg)" : "")
              . ($eb ? " (builtin: $eb)" : "");
            $job->note(book_error => { generic => $eg, builtin => $eb });
           # if you want the overall prebuild to continue despite book failures,
           # just return here instead:
            return $job->app->log->error($msg);
          }

          $job->app->log->info("Book imports complete generic=$sg builtin=$sb");
        }
      }
    );

    # Import Specialties
    my ($specialty_loop, $jid_specialties);

    $specialty_loop = Mojo::IOLoop->recurring(
      5 => sub {
        # don't start until prereqs pass
        unless ($job->prebuildPrerequisites) {
          $job->logger->debug('cannot start specialty import prereqs not met');
          return;
        }

        # enqueue once
        unless ($jid_specialties) {
          $job->logger->info('launching load_all_specialties');
          $jid_specialties = $job->minion->enqueue(
            load_all_specialties => [] => {
              attempts => 3,
              delay    => 1,
              expire   => 300,
              priority => 40,
            }
          );

          $job->note(specialty_jid => $jid_specialties);
          $job->app->log->info("Queued specialty import $jid_specialties");
          return;
        }

        # monitor
        my $js = $job->minion->job($jid_specialties);
        my $ss = $js && $js->info ? $js->info->{state} : 'unknown';

        $job->note(specialty_status => $ss);

        # stop once terminal
        my %terminal = map { $_ => 1 } qw(finished failed);
        if ($terminal{$ss}) {
          Mojo::IOLoop->remove($specialty_loop);
          $specialty_loop = undef;

          if ($ss eq 'failed') {
            my $es =
              $js && $js->info
              ? ($js->info->{result} // $js->info->{notes}{error})
              : undef;
            my $msg = "specialty import failed" . ($es ? " ($es)" : "");
            $job->note(specialty_error => $es);
            return $job->app->log->error($msg);
          }

          $job->app->log->info("Specialty import complete: $ss");
        }
      }
    );

    ## Import Generals
    my $distDir = Mojo::Home->new;
    $distDir->detect('Game::EvonyTKR');
    my $generalCount = -1;
    state $general_import_started = 0;
    state $gl;
    my $loop1;
    $loop1 = Mojo::IOLoop->recurring(
      2 => sub {
        state $gljid;
        if ($job->prebuildPrerequisites && $general_import_started == 0) {
          $general_import_started = 1;
          $gljid                  = $job->minion->enqueue(
            load_all_generals => ['prebuild load_all_generals'] => {
              attempts => 3,
              delay    => 5 + rand(10),
              expire   => 7200,
              priority => 10,
            }
          );
          $job->note(load_all_generals => 'started');
          $gl = $job->minion->job($gljid);
        }
        elsif (defined($gl)) {
          if (defined $gl->info->{notes}->{generalCount}) {
            $generalCount = $gl->info->{notes}->{generalCount};
            $job->note(load_all_generals => 'finished');
            $job->note(generalCount      => $generalCount);
            $job->logger->info(
              sprintf('Found General Count in Notes: %s', $generalCount));
            $job->general_cache->set('generalCount', $generalCount);
            Mojo::IOLoop->remove($loop1);
          }
          if ($gl->info->{state} =~ /failed/i) {
            Mojo::IOLoop->remove($loop1);
            $job->note(load_all_generals => 'failed');
            my $errmessage =
              sprintf('general loading failed: %s', $gl->info->{result});
            $job->logger->error($errmessage);
            $job->fail($errmessage);
          }
          if ($gl->info->{state} =~ /finished/i) {
            $generalCount = $gl->info->{notes}->{generalCount};
            $job->note(load_all_generals => 'finished');
            $job->general_cache->set('generalCount', $generalCount);
            $job->logger->info(
              sprintf('General loading finished: %s',
                $gl->info->{result} // 'no result')
            );
            Mojo::IOLoop->remove($loop1);
          }
        }
        else {
          $job->logger->warn(sprintf(
          'unexpected loop1 state. prereqs: %s; '
          .'general_import_started: %s; gl: %s ',
            $job->prebuildPrerequisites,
            $general_import_started ? 'true' : 'false',
            defined($gl)            ? $gl    : 'undefined'
          ));
        }
      }
    );

    # need the file names for error handling
    my $generalDir = Mojo::File->new(
      $distDir->child('share', 'collections', 'data', 'generals'));
    my @suffixlist = ('.yaml', '.yml');
    my @yaml_files = $generalDir->list->map(sub {
      my $e = $_;
      if ($e->to_string =~ m/\.y[a]?ml$/) {
        return $e->basename(@suffixlist);
      }
      else {
      }
      return '';
    })->compact->each;

    my $loop2;
    $loop2 = Mojo::IOLoop->recurring(
      5 => sub {
        unless ($job->prebuildPrerequisites && $general_import_started) {
          $job->logger->debug('not ready to start pair building yet.');
          return;
        }
        # this should be duplicate
        # but I'm having issues.
        my $generals    = $job->get_generals($job->app);
        my $cachedCount = scalar keys $generals->%*;
        if ($cachedCount >= $generalCount && $generalCount >= 0) {
          $job->logger->info(sprintf(
            'cached count %s == expected count %s',
            $cachedCount, $generalCount
          ));
          Mojo::IOLoop->remove($loop2);
          #Start pair building workflow
          my $pair_workflow_jid = $job->minion->enqueue(
            'load_all_pair_builders' => [] => {
              priority => 50,
              attempts => 5,
              expire   => 3600,
            }
          );
          $pair_monitor_jid = $job->minion->enqueue(
            'monitor_create_pairs' => [{}] => {
              priority => 90,
              attempts => 5,
              delay    => 15,
              expire   => 7200,
            }
          );
        }
        elsif ($cachedCount >= $generalCount - 5) {
          # We're missing exactly one - let's find which one
          my %cached_names     = map { $_ => 1 } keys $generals->%*;
          my $missing_generals = [];
          for my $yaml_file (@yaml_files) {
            my $general_name    = $yaml_file =~ s/\.ya?ml$//r;
            my $normalized_name = $job->normalize($general_name);
            unless (exists $cached_names{$normalized_name}) {
              push @$missing_generals, $general_name;
            }
          }
          $job->logger->error(sprintf(
            'Missing %d general(s): %s (cached: %d, expected: %d)',
            scalar(@$missing_generals), join(', ', @$missing_generals),
            $cachedCount,               $generalCount
          ));
        }
        else {
          $job->logger->debug(sprintf(
            'cached count %s less than expected count %s; generals is %s',
            $cachedCount, $generalCount, ref($generals)
          ));
        }
      }
    );

    # Monitor completion
    my $loop3;
    $loop3 = Mojo::IOLoop->recurring(
      10 => sub {
        return if (not defined($pair_monitor_jid));

        # Check if pair workflow completed
        my $completed_pairs = $job->minion->jobs({
          tasks  => ['load_all_pair_builders'],
          states => ['finished']
        })->total;

        my $active_pairs = $job->minion->jobs({
          tasks  => ['load_all_pair_builders'],
          states => ['active', 'inactive']
        })->total;

        # Check monitor job for results
        my $monitor_job = $job->minion->job($pair_monitor_jid);
        if (
          ($monitor_job && $monitor_job->info->{state} eq 'finished')
          || ( $monitor_job
            && $monitor_job->info->{state} eq 'inactive'
            && $monitor_job->info->{retries} > 0)
        ) {
          # Collect incremental results
          my $pairs_by_type = $monitor_job->info->{notes}->{pairs_by_type}
            // {};
          my $conflicts = $monitor_job->info->{notes}->{conflicts} // {};

          $job->note(pairs_by_type => $pairs_by_type);
          $job->note(conflicts     => $conflicts);

          # if result is final
          if ( exists($monitor_job->info->{result})
            && length($monitor_job->info->{result})
            && $monitor_job->info->{result} eq 'all pair builders complete') {
            
            # Set completion flag in cache
            require Game::EvonyTKR::Service::Cache;
            my $cache = Game::EvonyTKR::Service::Cache->new(namespace => 'pairs:');
            $cache->set('pair_building_complete', 1);
            $job->logger->info('Set pair_building_complete flag in cache');
            
            Mojo::IOLoop->remove($loop3);
            Mojo::IOLoop->remove($timer_id) if $timer_id;
            release_lock_sqlite($db, $key, $owner);
            $job->finish('Prebuild orchestration complete');
          }
        }
      }
    );

    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}

1;

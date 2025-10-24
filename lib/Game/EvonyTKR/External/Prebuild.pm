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
require Game::EvonyTKR::External::General::Pair::Workflow;
require Game::EvonyTKR::External::General::Loader;

package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Home;
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  state $OnlyOnePrebuild = 0;

  state $generalCache;

  sub register ($plugin, $app, $conf = {}) {
    $plugin->SUPER::register($app, $conf);
    $plugin->logger->debug(sprintf('register function for "%s"', __PACKAGE__));

    # Register main prebuild orchestration task
    $app->minion->add_task(external_prebuild => __PACKAGE__);
    $app->plugin('Game::EvonyTKR::External::General::Loader');
    $app->plugin('Game::EvonyTKR::External::General::Pair::Workflow');

    # Register pair workflow tasks
    #$app->plugin('Game::EvonyTKR::External::General::Pair::Workflow');
    my $tasks = $app->minion->tasks();
    my @tns   = keys %$tasks;
    $plugin->logger->debug(
      sprintf('registered tasks include %s', join ', ', @tns));
    foreach my $tn (@tns) {
      if ($tn eq 'pair_worker') {
        $plugin->prebuildPrerequisites({ pair_workflow_loaded => 1 });
      }
      elsif ($tn eq 'load_general') {
        $plugin->prebuildPrerequisites({ general_loader_job_ready => 1 });
      }
    }

    my $pair_workflow_loaded = 0;
    $app->plugins->on(
      pair_workflow_loaded => sub {
        return $plugin->prebuildPrerequisites({ pair_workflow_loaded => 1 });
      }
    );

    $app->plugins->on(
      general_loader_job_ready => sub {
        return $plugin->prebuildPrerequisites(
          { general_loader_job_ready => 1 });
      }
    );

    $OnlyOnePrebuild = $plugin->get_value('OnlyOnePrebuild');
    if (!$OnlyOnePrebuild) {
      $OnlyOnePrebuild = $plugin->set_value('OnlyOnePrebuild', 1);

      if (my $guard =
        $app->minion->guard('external_prebuild', 0, { limit => 1 })) {
        my $prebuildJid = $plugin->startPrebuild($app);
        $plugin->monitorPrebuild($app, $prebuildJid);
      }
      else {
        $plugin->logger->debug(
          'failed to get external_prebuild guard, not starting.');
      }
    }
    else {
      $plugin->logger->debug('OnlyOnePrebuild prevented restart');
    }

    my $mojo_worker_started = 0;

  }

  sub prebuildPrerequisites ($plugin, $args = {}) {
    state $prereqs = {
      pair_workflow_loaded     => 0,
      general_loader_job_ready => 0,
    };

    foreach my $key (keys $args->%*) {
      $prereqs->{$key} = $args->{$key};
    }

    if (List::AllUtils::none { $_ = 0 } values $prereqs->%*) {
      return 1;
    }
    $plugin->logger->debug(
      sprintf('failed prebuildPrerequisites: %s',
        Data::Printer::np($prereqs, multiline => 0))
    );
    return 0;
  }

  sub startPrebuild ($plugin, $app) {
    my $jid = $app->minion->enqueue(
      'external_prebuild' => [{}] => {
        priority => 100,
        attempts => 3,
        expire   => 7200,
      }
    );
    $plugin->logger->info("Started prebuild orchestrator job $jid");
    return $jid;
  }

  sub monitorPrebuild ($plugin, $app, $prebuildJid) {
    my $loop;
    my $retryCount = 0;
    my $maxRetries = 5;

    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        my $job = $app->minion->job($prebuildJid);
        unless ($job) {
          $plugin->logger->error(
            "prebuildJid $prebuildJid is not associated with a valid job.");
          $retryCount++;
          if ($retryCount >= $maxRetries) {
            Mojo::IOLoop->remove($loop);
          }
          return;
        }

        my $info  = $job->info;
        my $notes = $info->{notes} // {};

        # Check for pairs completion and emit to Mojolicious
        if (my $pairs_by_type = $notes->{pairs_by_type}) {
          $plugin->logger->debug('detected pairs_by_type update');
          $app->plugins->emit(pairs_by_type => $pairs_by_type);
        }

        # Check for conflicts completion and emit to Mojolicious
        if (my $conflicts = $notes->{conflicts}) {
          $plugin->logger->debug('detected conflicts update');
          $app->plugins->emit(conflicts_complete => $conflicts);
        }

        # Check if prebuild is complete
        if ($info->{state} eq 'finished') {
          $plugin->logger->info('Prebuild orchestration complete');
          $app->plugins->emit(
            prebuild_complete => {
              pairs     => $notes->{pairs_by_type},
              conflicts => $notes->{conflicts}
            }
          );
          Mojo::IOLoop->remove($loop);
        }
        elsif ($info->{state} eq 'failed') {
          $plugin->logger->error(
            "Prebuild failed: " . ($info->{result} // 'unknown error'));
          Mojo::IOLoop->remove($loop);
        }
      }
    );
  }

  sub launch_general_import ($plugin, $distDir) {
    my $generalDir = Mojo::File->new(
      $distDir->child('share', 'collections', 'data', 'generals'));
    my @suffixlist            = ('.yaml', '.yml');
    my $generalFileCollection = $generalDir->list->map(sub {
      my $e = $_;
      if ($e->to_string =~ m/\.y[a]?ml$/) {
        return $e->basename(@suffixlist);
      }
      else {
      }
      return '';
    })->compact;
    my $generalCount = $generalFileCollection->size;
    $plugin->logger->debug(sprintf(
      'there are %s general files found in "%s" - a %s.',
      $generalCount, $generalDir, blessed($generalDir)
    ));
    my @GeneralFiles = $generalFileCollection->each;
    foreach my $index (0 .. $#GeneralFiles) {
      my $general_name = $GeneralFiles[$index];
      $general_name = $plugin->normalize($general_name);
      $plugin->logger->info(
        sprintf('launching import for "%s"', $general_name));
      # force copies of the variables general_name
      # and index just in case it matters.
      $plugin->app->minion->enqueue(
        load_general => [{
          general_name => "$general_name",  ##<--  general_name comes from here.
          index        => 0+ $index,
        }] => {
          attempts => 3,
          delay    => rand(5),
          priority => 10,
          expire   => 7200,
        }
      );
    }
    return $generalCount;
  }

  # Main prebuild orchestration job
  sub run ($plugin, @args) {
    $plugin->SUPER::run(@args);
    $plugin->logger->debug('Prebuild orchestration starting');

    # Import Generals
    $generalCache = $plugin->create_general_cache()
      unless defined $generalCache;
    my $distDir = Mojo::Home->new;
    $distDir->detect('Game::EvonyTKR');
    my $generalCount = -1;
    state $general_import_started = 0;
    my $loop1;
    $loop1 = Mojo::IOLoop->recurring(
      5 => sub {
        if ($plugin->prebuildPrerequisites && $general_import_started == 0) {
          $general_import_started = 1;
          $generalCount           = $plugin->launch_general_import($distDir);
          $plugin->set_value('generalCount', $generalCount, $generalCache);
          Mojo::IOLoop->remove($loop1);
        }
      }
    );

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
        unless ($plugin->prebuildPrerequisites && $general_import_started) {
          $plugin->logger->debug('not ready to start pair building yet.');
          return;
        }
        # this should be duplicate
        # but I'm having issues.
        $generalCache = $plugin->create_general_cache()
          unless defined $generalCache;
        my $generals    = $plugin->get_generals($generalCache);
        my $cachedCount = scalar keys $generals->%*;
        if ($cachedCount >= $generalCount) {
          $plugin->logger->info(sprintf(
            'cached count %s == expected count %s',
            $cachedCount, $generalCount
          ));
          Mojo::IOLoop->remove($loop2);
          #Start pair building workflow
          my $pair_workflow_jid = $plugin->app->minion->enqueue(
            'build_all_pairs' => [{}] => {
              priority => 50,
              attempts => 5,
              expire   => 3600,
            }
          );
        }
        elsif ($cachedCount >= $generalCount - 5) {
          # We're missing exactly one - let's find which one
          my %cached_names     = map { $_ => 1 } keys $generals->%*;
          my $missing_generals = [];
          for my $yaml_file (@yaml_files) {
            my $general_name    = $yaml_file =~ s/\.ya?ml$//r;
            my $normalized_name = $plugin->normalize($general_name);
            unless (exists $cached_names{$normalized_name}) {
              push @$missing_generals, $general_name;
            }
          }
          $plugin->logger->error(sprintf(
            'Missing %d general(s): %s (cached: %d, expected: %d)',
            scalar(@$missing_generals), join(', ', @$missing_generals),
            $cachedCount,               $generalCount
          ));
        }
        else {
          $plugin->logger->debug(sprintf(
            'cached count %s less than expected count %s; generals is %s',
            $cachedCount, $generalCount, ref($generals)
          ));
        }
      }
    );

    # Start monitoring
    my $monitor_jid = $plugin->app->minion->enqueue(
      'monitor_pair_builders' => [{}] => {
        priority => 90,
        attempts => 5,
        delay    => 15,
        expire   => 7200,
      }
    );

    # Monitor completion
    my $loop3;
    $loop3 = Mojo::IOLoop->recurring(
      10 => sub {
        # Check if pair workflow completed
        my $completed_pairs = $plugin->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['finished']
        })->total;

        my $active_pairs = $plugin->app->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['active', 'inactive']
        })->total;

        # Check monitor job for results
        my $monitor_job = $plugin->app->minion->job($monitor_jid);
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

          $plugin->note(pairs_by_type => $pairs_by_type);
          $plugin->note(conflicts     => $conflicts);

          # if result is final
          if ( exists($monitor_job->info->{result})
            && length($monitor_job->info->{result})
            && $monitor_job->info->{result} eq 'all pair builders complete') {
            Mojo::IOLoop->remove($loop3);
            $plugin->finish('Prebuild orchestration complete');
          }
        }
      }
    );

    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}

1;

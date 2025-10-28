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
require Game::EvonyTKR::External::General::LoadAll;
require Game::EvonyTKR::External::Book::Loader;
require Game::EvonyTKR::External::Book::LoadAllBuiltins;
require Game::EvonyTKR::External::Book::LoadAllGenerics;

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
      'Game::EvonyTKR::External::General::Loader',
      'Game::EvonyTKR::External::General::LoadAll',
      'Game::EvonyTKR::External::General::Pair::Workflow',
      'Game::EvonyTKR::External::Book::Loader',
      'Game::EvonyTKR::External::Book::LoadAllBuiltins',
      'Game::EvonyTKR::External::Book::LoadAllGenerics',
    ];

    my @tasks = values $app->minion->tasks->%*;
    foreach my $task (@tasks) {
      $plugin->logger->debug(sprintf('task is %s, %s',
        ref($task) // 'undef ref',
        blessed($task) // 'undef blessed'));
      say sprintf(
        'task is %s, %s, %s',
        ref($task) // 'undef ref',
        blessed($task) // 'undef blessed', "$task"
      );
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
      $app->plugin($prereq);
    }

    while (!$plugin->prebuildPrerequisites()) {
      $plugin->logger->info('prerquisits not met, %s',
        Data::Printer::np($prereqs, multiline => 0));
      sleep(5);
    }
    $OnlyOnePrebuild = $plugin->get_value('OnlyOnePrebuild');
    if (!$OnlyOnePrebuild) {
      $OnlyOnePrebuild = $plugin->set_value('OnlyOnePrebuild', 1);
      my $jid = 0;
      if (not(defined($app->minion) && defined($app->minion->backend))) {
        $plugin->logger->error('$app is in a wierd state');
        return;
      }
      if (my $guard =
        $app->minion->guard('external_prebuild', 0, { limit => 1 })) {
        $jid = $app->minion->enqueue(
          'external_prebuild' => [{}] => {
            priority => 100,
            attempts => 3,
            expire   => 7200,
          }
        );
        $plugin->logger->info("Started prebuild orchestrator job $jid");
        Mojo::IOLoop->timer(
          1 => sub {
            $plugin->monitorPrebuild($app, $jid);
          }
        );
      }
      else {
        $plugin->logger->debug(
          'failed to get external_prebuild guard, not starting.');
      }
    }
    else {
      $plugin->logger->debug('OnlyOnePrebuild prevented restart');
    }

    $plugin->logger->info(
      sprintf('%s register function complete for %s', __PACKAGE__, $$));
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

  sub monitorPrebuild ($job, $app, $prebuildJid) {
    state $retryCount = 0;
    my $maxRetries = 5;

    my $pbj = $app->minion->job($prebuildJid);
    unless ($pbj) {
      my $erm = "prebuildJid $prebuildJid is not associated with a valid job.";
      $job->logger->error($erm);
      $retryCount++;
      if ($retryCount >= $maxRetries) {
        return $job->fail($erm);
      }
      return $job->retry({ delay => 30 });
    }

    my $info  = $pbj->info;
    my $notes = $info->{notes} // {};

    # Check for pairs completion and emit to Mojolicious
    if (my $pairs_by_type = $notes->{pairs_by_type}) {
      $job->logger->debug('detected pairs_by_type update');
      $app->plugins->emit(pairs_by_type => $pairs_by_type);
    }

    # Check for conflicts completion and emit to Mojolicious
    if (my $conflicts = $notes->{conflicts}) {
      $job->logger->debug('detected conflicts update');
      $app->plugins->emit(conflicts_complete => $conflicts);
    }

    # Check if prebuild is complete
    if ($info->{state} eq 'finished') {
      $job->logger->info('Prebuild orchestration complete');
      $app->plugins->emit(
        prebuild_complete => {
          pairs     => $notes->{pairs_by_type},
          conflicts => $notes->{conflicts}
        }
      );
    }
    elsif ($info->{state} eq 'failed') {
      my $erm = "Prebuild failed: " . ($info->{result} // 'unknown error');
      $job->logger->error($erm);
      return $job->fail($erm);
    }
    return $job->retry({ delay => 5 });
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

    # Import Books
    state $book_import_started = 0;
    my $book_loop;
    $book_loop = Mojo::IOLoop->recurring(
      5 => sub {
        $job->note(prereqs => Data::Printer::np($prereqs, multiline => 0));
        if ($job->prebuildPrerequisites && $book_import_started == 0) {
          $book_import_started = 1;
          $job->minion->enqueue(
            load_all_generic_books => [] => {
              attempts => 3,
              delay    => 1,
              expire   => 300,
              priority => 50,
            }
          );
          $job->minion->enqueue(
            load_all_builtin_books => [] => {
              attempts => 3,
              delay    => 1,
              expire   => 300,
              priority => 60,
            }
          );
        }
      }
    );

    # Import Generals
    $generalCache = $job->create_general_cache()
      unless defined $generalCache;
    my $distDir = Mojo::Home->new;
    $distDir->detect('Game::EvonyTKR');
    my $generalCount = -1;
    state $general_import_started = 0;
    state $gl;
    my $loop1;
    $loop1 = Mojo::IOLoop->recurring(
      5 => sub {
        state $gljid;
        if ($job->prebuildPrerequisites && $general_import_started == 0) {
          $general_import_started = 1;
          $gljid                  = $job->minion->enqueue(
            load_all_generals => ['prebuild load_all_generals'] => {
              attempts => 3,
              delay    => rand(10),
              expire   => 7200,
              priority => 10,
            }
          );
          $gl = $job->minion->job($gljid);
          $gl->on(
            finish => sub ($glj,) {
              $generalCount = $glj->notes->{generalCount};
              $job->set_value('generalCount', $generalCount, $generalCache);
              Mojo::IOLoop->remove($loop1);
            }
          );
          $gl->on(
            failed => sub($glj, $err) {
              Mojo::IOLoop->remove($loop1);
              my $errmessage = sprintf('general loading failed: %s', $err);
              $job->logger->error($errmessage);
              $job->fail($errmessage);
            }
          );
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
        $generalCache = $job->create_general_cache()
          unless defined $generalCache;
        my $generals    = $job->get_generals($generalCache);
        my $cachedCount = scalar keys $generals->%*;
        if ($cachedCount >= $generalCount) {
          $job->logger->info(sprintf(
            'cached count %s == expected count %s',
            $cachedCount, $generalCount
          ));
          Mojo::IOLoop->remove($loop2);
          #Start pair building workflow
          my $pair_workflow_jid = $job->minion->enqueue(
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

    # Start monitoring
    my $monitor_jid = $job->minion->enqueue(
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
        my $completed_pairs = $job->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['finished']
        })->total;

        my $active_pairs = $job->minion->jobs({
          tasks  => ['build_all_pairs'],
          states => ['active', 'inactive']
        })->total;

        # Check monitor job for results
        my $monitor_job = $job->minion->job($monitor_jid);
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
            Mojo::IOLoop->remove($loop3);
            $job->finish('Prebuild orchestration complete');
          }
        }
      }
    );

    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }
}

1;

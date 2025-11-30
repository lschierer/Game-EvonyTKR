use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Covenant::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::File;

  sub task_name {'load_all_covenants'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    return
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_ascending_attributes', 'load_all_builtin_books',
        'load_all_specialties',          'load_ascending_attributes',
        'load_book',                     'load_specialty',
        'load_all_generals',             'load_general',
      ]
      ));

    $job->logger->info('Starting load_all_covenants job');

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $CovenantsDir = $collectionDir->child('covenants');

    my @files =
      $CovenantsDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;

    $job->logger->info(
      sprintf('Found %d Covenant files to process', scalar @files));

    my $enqueued_count = 0;
    my $skipped_count  = 0;
    my @job_ids        = ();

    foreach my $file (@files) {
      # Extract covenant name from filename
      my $covenant_name = $file->basename('.yaml', '.yml');

      # Check if already in persistence
      if ($job->persistence->get_covenant($covenant_name)) {
        $job->logger->debug(sprintf(
          'Skipping %s - already in persistence', $covenant_name));
        $skipped_count++;
        next;
      }

      my $job_id = $job->minion->enqueue(
        'load_covenant' => [$file->to_string] => {
          attempts => 3,
          delay    => rand(10),
          priority => 20,
        }
      );
      $job->logger->debug(sprintf(
        'Enqueued load_covenant job %s for file %s',
        $job_id, $file->basename
      ));
      push @job_ids, $job_id;
      $enqueued_count++;
    }

    $job->logger->info(sprintf(
      'Enqueued %d load_covenant jobs, skipped %d already in persistence',
      $enqueued_count, $skipped_count
    ));

    # Wait for all child jobs to complete
    if (@job_ids) {
      $job->logger->info(
        sprintf('Waiting for %d child jobs to complete', scalar @job_ids));

      my $finished = 0;
      my $failed   = 0;

      while (1) {
        my $all_done = 1;
        $finished = 0;
        $failed   = 0;

        for my $jid (@job_ids) {
          my $info = $job->minion->job($jid);
          next unless ($info && $info->{state});

          if ($info->{state} eq 'finished') {
            $finished++;
          }
          elsif ($info->{state} eq 'failed') {
            $failed++;
          }
          else {
            $all_done = 0;
          }
        }

        last if $all_done;
        sleep 2;
      }

      $job->logger->info(sprintf(
        'Child jobs completed: %d finished, %d failed',
        $finished, $failed
      ));
    }

    $job->covenant_cache->set(total_covenants => scalar(@files));

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    my $msg = 'load_all_covenants job completed';
    $job->logger->info($msg);
    $job->finish($msg);
  }
}

1;

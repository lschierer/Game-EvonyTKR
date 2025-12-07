use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::File;
  use YAML::PP;
  use Scalar::Util;
  require Game::EvonyTKR::Model::General;
  use Carp;

  sub task_name {'load_general'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, $filename) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run([$filename]);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    $job->log_debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));
    $job->log_debug("Loading general from file: $filename");

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalDir    = $collectionDir->child('generals');
    my @suffixList    = ('.yaml', '.yml');

    $job->log_debug(sprintf(
      'Searching for file matching "%s" in %s',
      $filename, $generalDir
    ));

    my ($generalFile) = $generalDir->list->grep(sub {
      my $nn = $job->normalize($filename);
      $nn = Mojo::File->new($nn)->basename(@suffixList);
      my $cf = $job->normalize($_->basename(@suffixList));
      if ($nn eq $cf) {
        $job->log_debug(sprintf(
          'Matched file: %s (normalized: "%s" == "%s")',
          $_->to_string, $nn, $cf
        ));
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless (defined $generalFile) {
      my $available = join(', ',
        map { $_->basename } $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ })->each
      );
      $job->log_error(sprintf(
        'No file found matching "%s". Available files: %s',
        $filename, $available
      ));
      return $job->fail("No general file found for: $filename");
    }

    unless (-f $generalFile && -r $generalFile) {
      $job->log_error("Cannot read general file: $generalFile");
      return $job->fail("Cannot read general file: $generalFile");
    }

    $job->log_info(sprintf('Loading general from: %s', $generalFile->to_string));

    my $data       = $generalFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    my $general = Game::EvonyTKR::Model::General->from_hash($hashObject);
    unless ($general) {
      $job->log_error("Failed to import General from $generalFile");
      return $job->fail("Failed to import General from $generalFile");
    }

    # Populate builtin book
    $general->populateBuiltinBook();
    my $max_retries = 10;
    unless (defined($general->builtInBook)
      && Scalar::Util::blessed($general->builtInBook)
      && $general->builtInBook->DOES('Game::EvonyTKR::Model::Book')) {
      my $errmessage = sprintf(
        'Failed to retrieve builtin book "%s" for general "%s"',
        ($general->builtInBookName // 'no builtInBookName'),
        ($general->name            // 'No general Name')
      );
      if ($job->retries < $max_retries) {
        $job->note(error => $errmessage);
        return $job->retry({ delay => 15 });
      }
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }

    if ($general->ascending) {
      unless ($general->populateAscendingAttributes()) {
        my $errmessage = sprintf(
          'failed to populate Ascending Attributes' . 'for general "%s"',
          $general->name,
        );
        if ($job->retries < $max_retries) {
          $job->note(error => $errmessage);
          return $job->retry({ delay => 15 });
        }
        $job->log_error($errmessage);
        return $job->fail($errmessage);
      }
    }

    # Add to cache
    $job->add_general($general);

    my $message = sprintf('Successfully loaded general: %s', $general->name);
    $job->log_info($message);
    $job->note(general => $general);
    $job->finish($message);
  }

}
1;
__END__

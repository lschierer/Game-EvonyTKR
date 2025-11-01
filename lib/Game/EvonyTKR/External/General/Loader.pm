use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',                                -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::File;
  use YAML::PP;
  use Scalar::Util;
  require Game::EvonyTKR::Model::General;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(load_general => __PACKAGE__);
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
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));
    $job->logger->debug("Loading general from file: $filename");

    my $app = $job->app;
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalDir = $collectionDir->child('generals');
    my @suffixList = ('.yaml', '.yml');
    my ($generalFile) = $generalDir->list->grep(sub {
      my $nn = $job->normalize($filename);
      $nn = Mojo::File->new($nn)->basename(@suffixList);
      my $cf = $job->normalize($_->basename(@suffixList));
      if($nn eq $cf){
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless (-f $generalFile && -r $generalFile) {
      $job->logger->error("Cannot read general file: $generalFile");
      return $job->fail("Cannot read general file: $generalFile");
    }

    my $data       = $generalFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    my $general = Game::EvonyTKR::Model::General->from_hash($hashObject);
    unless ($general) {
      $job->logger->error("Failed to import General from $generalFile");
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
        return $job->retry({ delay => 30 });
      }
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    # Add to cache
    $job->add_general($general);

    $job->logger->info(
      sprintf('Successfully loaded general: %s', $general->name));
  }

}
1;
__END__

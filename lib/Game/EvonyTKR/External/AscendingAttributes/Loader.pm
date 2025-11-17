use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::AscendingAttributes::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',      -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::AscendingAttributes', -role;
  use Mojo::File;
  use YAML::PP;
  use Encode;
  require Game::EvonyTKR::Model::AscendingAttributes;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(load_ascending_attributes => __PACKAGE__);
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
    $job->logger->debug("Loading AscendingAttribute from file: $filename");

    my $ascendingAttributeFile =
      Mojo::File->new(Encode::decode_utf8($filename));

    unless (-f $ascendingAttributeFile && -r $ascendingAttributeFile) {
      $job->logger->error(
        "Cannot read AscendingAttribute file: $ascendingAttributeFile");
      return $job->fail(
        "Cannot read AscendingAttribute file: $ascendingAttributeFile");
    }

    my $data       = $ascendingAttributeFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    unless (exists $hashObject->{general} && length($hashObject->{general})) {
      $job->logger->error(sprintf(
      'general is required for a AscendingAttribute. Cannot import "%s"',
      $ascendingAttributeFile ));
      return $job->fail("general is required for a AscendingAttribute");
    }

    my $ascendingAttribute =
      Game::EvonyTKR::Model::AscendingAttributes->from_hash($hashObject);
    unless ($ascendingAttribute) {
      $job->logger->error(
        "Failed to import AscendingAttribute from $ascendingAttributeFile");
      return $job->fail(
        "Failed to import AscendingAttribute from $ascendingAttributeFile");
    }

    # Add to cache
    $job->add_ascending_attribute($ascendingAttribute);
    $job->note(ascending_attribute => $ascendingAttribute);
    $job->logger->info(
      sprintf('Successfully loaded AscendingAttribute: %s',
        $ascendingAttribute->general)
    );
  }
}

1;

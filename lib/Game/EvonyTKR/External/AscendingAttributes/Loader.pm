use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::AscendingAttributes::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::File;
  use YAML::PP;
  use Encode;
  require Game::EvonyTKR::Model::AscendingAttributes;

  sub task_name {'load_ascending_attributes'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    return 1;
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
    $job->log_debug("Loading AscendingAttribute from file: $filename");

    my $ascendingAttributeFile =
      Mojo::File->new(Encode::decode_utf8($filename));

    unless (-f $ascendingAttributeFile && -r $ascendingAttributeFile) {
      $job->log_error(
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
      $job->log_error(sprintf(
        'general is required for a AscendingAttribute. Cannot import "%s"',
        $ascendingAttributeFile));
      return $job->fail("general is required for a AscendingAttribute");
    }

    my $ascendingAttribute =
      Game::EvonyTKR::Model::AscendingAttributes->from_hash($hashObject);
    unless ($ascendingAttribute) {
      $job->log_error(
        "Failed to import AscendingAttribute from $ascendingAttributeFile");
      return $job->fail(
        "Failed to import AscendingAttribute from $ascendingAttributeFile");
    }

    # Add to cache
    $job->add_ascending_attribute($ascendingAttribute);
    $job->note(ascending_attribute => $ascendingAttribute);
    $job->log_info(
      sprintf('Successfully loaded AscendingAttribute: %s',
        $ascendingAttribute->general)
    );
  }
}

1;

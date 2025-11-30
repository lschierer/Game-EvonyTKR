use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Specialty::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',             -signatures;
  use Mojo::File;
  use YAML::PP;
  use Encode;
  require Game::EvonyTKR::Model::Specialty;

  sub task_name {'load_specialty'}

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
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));
    $job->logger->debug("Loading specialty from file: $filename");

    my $specialtyFile = Mojo::File->new(Encode::decode_utf8($filename));

    unless (-f $specialtyFile && -r $specialtyFile) {
      $job->logger->error("Cannot read specialty file: $specialtyFile");
      return $job->fail("Cannot read specialty file: $specialtyFile");
    }

    my $data       = $specialtyFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    unless (exists $hashObject->{name} && length($hashObject->{name})) {
      $job->logger->error(
        "Name is required for a Specialty. Cannot import $specialtyFile");
      return $job->fail("Name is required for a Specialty");
    }

    my $specialty = Game::EvonyTKR::Model::Specialty->from_hash($hashObject);
    unless ($specialty) {
      $job->logger->error("Failed to import Specialty from $specialtyFile");
      return $job->fail("Failed to import Specialty from $specialtyFile");
    }

    # Add to cache
    $job->add_specialty($specialty);
    $job->note(specialty => $specialty);
    $job->logger->info(
      sprintf('Successfully loaded specialty: %s', $specialty->name));
  }
}

1;

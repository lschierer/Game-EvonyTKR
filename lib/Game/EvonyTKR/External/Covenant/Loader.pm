use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Covenant;

package Game::EvonyTKR::External::Covenant::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',           -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',                -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals',  -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Covenants', -role;
  use Mojo::File;
  use YAML::PP;
  use Encode;
  require Game::EvonyTKR::Model::Covenant;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(load_covenant => __PACKAGE__);
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
    $job->logger->debug("Loading Covenant from file: $filename");

    my $CovenantFile = Mojo::File->new(Encode::decode_utf8($filename));

    unless (-f $CovenantFile && -r $CovenantFile) {
      $job->logger->error("Cannot read Covenant file: $CovenantFile");
      return $job->fail("Cannot read Covenant file: $CovenantFile");
    }

    my $data       = $CovenantFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    my @errors;
    unless ($hashObject->{name} && length($hashObject->{name})) {
      my $errmessage = 'name is a required attribute.';
      push @errors, $errmessage;
    }
    unless ($hashObject->{generals} && length($hashObject->{generals})) {
      my $errmessage = 'generals is a required attribute.';
      push @errors, $errmessage;
    }
    unless ($hashObject->{levels} && length($hashObject->{levels})) {
      my $errmessage = 'levels is a required attribute.';
      push @errors, $errmessage;
    }

    my $covenant =
      Game::EvonyTKR::Model::Covenant->from_hash($hashObject);
    unless ($covenant) {
      my $errmessage = sprintf('failed to create covenant from %s',
        Data::Printer::np($hashObject, multiline => 0));
      push @errors, $errmessage;
      $job->run_fail(@errors);
    }

    my $result = $job->add_covenant($covenant);
    unless ($result) {
      my $errmessage = sprintf('failed to add covenant %s to the cache',
        $covenant->primary->name);
      push @errors, $errmessage;
      $job->run_fail(@errors);
    }

    if (scalar(@errors)) {
      $job->run_fail(@errors);
    }

    $job->note(Covenant => $covenant);
    my $complete =
      sprintf('finished loading covenant %s', $covenant->primary->name);
    $job->logger->info($complete);
    return $job->finish($complete);
  }

  sub run_fail ($job, @errors) {
    $job->logger->error(join('; ', @errors));
    $job->fail(join('; ', @errors));
  }
}
1;
__END__

use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Book;

package Game::EvonyTKR::External::General::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',               -role;
  use Mojo::File;
  use experimental qw(class);
  use diagnostics;
  use Carp;

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $taskClass->logger->error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $taskClass->logger->error($errmessage);
      say $errmessage;
      return;
    }
    $taskClass->logger->debug('Registering General Loader workflow tasks');
    $app->minion->add_task(load_general => __PACKAGE__);

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
    $job->logger->debug(
      sprintf('::General::Loader log level is %s',
        Log::Log4perl::Level::to_level($job->logger->level()))
    );
    my $params       = shift @args;
    my $general_name = $job->normalize($params->{general_name});
    my $index        = $params->{index};
    my @suffixlist   = ('.yaml', '.yml');
    state $generalCache;
    unless (my $lock =
      $job->minion->guard("load_general: $general_name", 300, { limit => 1 })) {
      $job->finish(sprintf(
        'import for "%s" has already started; %s exiting.',
        $general_name, $index
      ));
    }
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new->to_string())
      ->child('share/collections/data/');
    my $generalsDir = $collectionDir->child('generals');
    my ($generalFile) = $generalsDir->list->sort->grep(sub {
      my $b = $job->normalize($_->basename(@suffixlist));
      if ($_ =~ m/\.y[a]?ml$/ && $b eq $general_name) {
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless ($generalFile) {
      $job->logger->error("no file found for general with name $general_name");
      return $job->fail("no file found for general with name $general_name");
    }
    my $general;
    $generalCache = $job->create_general_cache() unless defined($generalCache);
    $general      = $job->get_general($general_name, $generalCache);
    if ( defined($general)
      && ref($general) eq 'HASH'
      && blessed($general)
      && $general->isa('Game::EvonyTKR::Model::General')) {
      my $result =
        sprintf('returning already loaded general %s', $general_name);
      $job->logger->info($result);
      return $job->finish($result);
    }
    $general = $job->load_single_general($generalFile, $index);
    unless ($general) {
      my $result = sprintf(
        'No general returned by load_single_general'
          . ' for general with name "%s"',
        $general_name
      );
      $job->logger->error($result);
      return $job->fail($result);
    }

    $generalCache = $job->create_general_cache() unless defined($generalCache);
    my $storeResult = $job->add_general($general->normalize($general->name),
      $general, $generalCache);
    my $result;
    if (defined $storeResult && $storeResult eq '1') {
      $result = sprintf('general cached: %s', $job->normalize($general->name));
      $job->logger->info($result);
    }
    elsif (defined $storeResult && $storeResult eq '0') {
      my $result = sprintf('add_general reports inability to set "%s"',
        $job->normalize($general->name));
      $job->logger->error($result);
      return $job->fail($result);
    }
    else {
      my $result = sprintf(
        'add_general reports "%s" error setting "%s"',
        defined $result ? $result : 'unknown',
        $job->normalize($general->name)
      );
      $job->logger->error($result);
      return $job->info->fail($result);
    }
    return $job->finish($result);
  }

  sub load_single_general ($job, $generalFile, $index) {
    $job->logger->debug("processing $generalFile, file # $index");
    my $data       = $generalFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    my $g = Game::EvonyTKR::Model::General->from_hash($hashObject);
    unless ($g) {
      $job->logger->error(sprintf(
        'failed to build general from %s', $generalFile));
      return undef;
    }

    $g->populateBuiltinBook();
    my $max_retries = 10;
    unless (defined($g->builtInBook)
      && Scalar::Util::blessed($g->builtInBook)
      && $g->builtInBook->DOES('Game::EvonyTKR::Model::Book')) {
      my $errmessage = sprintf(
        'Failed to retrieve builtin book "%s" for general "%s"',
        ($g->builtInBookName // 'no builtInBookName'),
        ($g->name            // 'No general Name')
      );
      if (defined($job) && $job->retries < $max_retries) {
        $job->note(error => $errmessage);
        return $job->retry({ delay => 30 });
      }

      $job->logger->error($errmessage);
      return defined($job) ? $job->fail($errmessage) : undef;
    }
    else {
      $job->logger->debug(sprintf(
        '%s is in fact a %s which %s',
        $g->builtInBook->name,
        blessed($g->builtInBook),
        $g->builtInBook->DOES('Game::EvonyTKR::Model::Book')
        ? 'isa Game::EvonyTKR::Model::Book'
        : 'fails isa Game::EvonyTKR::Model::Book'
      ));
    }

    $job->logger->debug(sprintf('returning general %s.', $g->name));
    return $g;
  }
}
1;
__END__

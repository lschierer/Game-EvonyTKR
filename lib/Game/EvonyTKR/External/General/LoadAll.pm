use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::External::Common;

package Game::EvonyTKR::External::General::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',               -role;
  use Mojo::File;
  use experimental qw(class);
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
    $app->minion->add_task(load_all_generals => __PACKAGE__);

    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, $taskName) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run([$taskName]);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(
      sprintf('::General::Loader log level is %s',
        Log::Log4perl::Level::to_level($job->logger->level()))
    );
    unless (my $lock =
      $job->minion->guard(sprintf('%s job', __PACKAGE__), 300, { limit => 1 }))
    {
      $job->finish(sprintf('%s started; %s exiting.', $taskName, __PACKAGE__));
    }
    my $distDir    = Mojo::Home->new->detect('Game::EvonyTKR');
    my $generalDir = Mojo::File->new($distDir->to_string)
      ->child('share', 'collections', 'data', 'generals');
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
    $job->logger->debug(sprintf(
      'there are %s general files found in "%s" - a %s.',
      $generalCount, $generalDir, blessed($generalDir)
    ));
    my @GeneralFiles = $generalFileCollection->each;
    foreach my $index (0 .. $#GeneralFiles) {
      my $general_name = $GeneralFiles[$index];
      $general_name = $job->normalize($general_name);
      $job->logger->info(sprintf('launching import for "%s"', $general_name));
      # force copies of the variables general_name
      # and index just in case it matters.
      $job->minion->enqueue(
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
    $job->note(generalCount => $generalCount);
    return $job->finish(sprintf('started %s load_general jobs', $generalCount));
  }
}

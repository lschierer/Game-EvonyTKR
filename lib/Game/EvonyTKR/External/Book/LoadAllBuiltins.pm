use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

require Game::EvonyTKR::External::Common;

package Game::EvonyTKR::External::Book::LoadAllBuiltins {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',       -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',            -role;
  use List::AllUtils qw( any none uniq all );
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  state $bookCache;

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
    $taskClass->logger->debug('Registering Book Loader workflow tasks');
    $app->minion->add_task(load_all_builtin_books => __PACKAGE__);

    $taskClass->logger->info(sprintf('emitting signal for %s', __PACKAGE__));
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
    my @list;
    push @list, $job->list_builtin_books($job->app)->@*;
    $job->logger->info('list of builtin books is ' . Data::Printer::np(@list));
    my $maxIndex = scalar(@list) - 1;
    foreach my $index (0 .. $maxIndex) {
      my $entry = $list[$index];
      $job->minion->enqueue(
        load_book => [
          $entry,
          {
            index      => $index,
            is_generic => 0,
            is_builtin => 1,
            filebase   => $entry,
            suffixlist => ['.yaml', '.yml'],
          }
        ] => {
          attempts => 3,
          delay    => rand(10),
          expire   => 300,
          priority => 20,
        }
      );
    }
  }
}
1;
__END__

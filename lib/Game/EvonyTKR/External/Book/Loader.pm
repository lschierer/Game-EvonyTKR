use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Mojo::Home;
require Game::EvonyTKR::Model::Book;

package Game::EvonyTKR::External::Book::Loader {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',       -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',            -role;
  use Mojo::File;
  use experimental qw(class);
  use Carp;

  sub task_name {'load_book'}

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
    my $entry  = shift @args;
    my $params = shift @args;
    my $index  = $params->{index}, my $suffixlist = $params->{suffixlist};
    my $bookType;
    if ($params->{is_generic} && (not $params->{is_builtin})) {
      $job->logger->debug(sprintf(
        'detected generic job %s via %s and %s',
        $entry,
        $params->{is_generic} ? 'is_generic true' : 'is_generic false',
        $params->{is_builtin} ? 'is_builtin true' : 'is_builtin false'
      ));
      $bookType = 'generic';
      return $job->load_generic($entry, $index, $suffixlist);
    }
    elsif ((not $params->{is_generic}) && $params->{is_builtin}) {
      $job->logger->debug(sprintf(
        'detected builtin job %s via %s and %s',
        $entry,
        $params->{is_generic} ? 'is_generic true' : 'is_generic false',
        $params->{is_builtin} ? 'is_builtin true' : 'is_builtin false'
      ));
      $bookType = 'builtin';
      return $job->load_builtin($entry, $index, $suffixlist);
    }
    else {
      my $errmessage = sprintf('inconsistent params for load_book job: %s',
        Data::Printer::np($params, multiline => 0));
      $job->logger->error($errmessage);
      $job->fail($errmessage);
      return;
    }
  }

  sub load_generic ($job, $entry, $index, $suffixlist) {
    unless (my $lock =
      $job->minion->guard("load_book: $entry", 300, { limit => 1 })) {
      $job->finish(sprintf(
        'import for "%s" has already started; %s exiting.',
        $entry, $index
      ));
    }
    my $book;
    my @parts = split ' ', $entry;
    my $level = $parts[1] unless ($#parts < 1);
    my $name  = join ' ', @parts[2 .. $#parts] unless ($#parts < 2);
    unless (defined($level)) {
      my $errmessage =
        sprintf('failed to parse "%s" as a generic book filename', $entry);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    unless (defined($name)) {
      my $errmessage =
        sprintf('failed to parse "%s" as a generic book filename', $entry);
      $job->logger->error($errmessage);
      $job->fail($errmessage);
      return;
    }
    $book = $job->get_generic_book($name, $level);

    if ( defined($book)
      && ref($book)
      && $book->isa('Game::EvonyTKR::Model::Book')) {
      my $result = sprintf('returning already loaded book "%s"',
        sprintf('Level %s %s', $book->level, $book->name));
      $job->logger->info($result);
      return $job->finish($result);
    }
    else {
      $job->logger->debug(sprintf('proceeding to import %s from file', $entry));
    }

    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $bookDir    = $collectionDir->child('generic books');
    my $ne         = lc($job->normalize($entry));
    my ($bookFile) = $bookDir->list->sort->grep(sub {
      my $b = lc($job->normalize($_->basename(@$suffixlist)));
      if ($_ =~ m/\.ya?ml$/ && $b eq $ne) {
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless (defined($bookFile) && length($bookFile)) {
      my $errmessage =
        sprintf('failed to find file for entry "%s" in %s', $entry, $bookDir);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    my $bd  = $bookFile->slurp('UTF-8');
    my $bho = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($bd);
    $book = Game::EvonyTKR::Model::Book->from_hash($bho);

    unless ($book
      && Scalar::Util::blessed($book)
      && $book->isa('Game::EvonyTKR::Model::Book')) {
      my $errmessage = sprintf('failed to load book for file "%s"', $bookFile);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    my $add_result = $job->add_generic_book($book);
    if (defined($add_result) && $add_result == 1) {
      my $result = sprintf('imported %s',
        sprintf('Level %s %s', $book->level, $book->name));
      $job->logger->info($result);
      $job->note(book_imported => $book);
      return $job->finish($result);
    }
    else {
      my $errmessage = sprintf('add to cache for %s failed: %s',
        $entry, $add_result // 'undef add result');
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

  }

  sub load_builtin ($job, $entry, $index, $suffixlist) {
    unless (my $lock =
      $job->minion->guard("load_book: $entry", 300, { limit => 1 })) {
      $job->finish(sprintf(
        'import for "%s" has already started; %s exiting.',
        $entry, $index
      ));
    }
    my $book;
    $book = $job->get_builtin_book($entry);

    if ( defined($book)
      && ref($book)
      && $book->isa('Game::EvonyTKR::Model::Book')) {
      my $result = sprintf('returning already loaded book "%s"', $book->name);
      $job->logger->info($result);
      return $job->finish($result);
    }

    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $bookDir    = $collectionDir->child('skill books');
    my $ne         = lc($job->normalize($entry));
    my ($bookFile) = $bookDir->list->sort->grep(sub {
      my $b = lc($job->normalize($_->basename(@$suffixlist)));
      if ($_ =~ m/\.ya?ml$/ && $b eq $ne) {
        return 1;
      }
      return 0;
    })->head(1)->each;

    unless (defined($bookFile) && length($bookFile)) {
      my $errmessage =
        sprintf('failed to find file for entry "%s" in %s', $entry, $bookDir);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    my $bd  = $bookFile->slurp('UTF-8');
    my $bho = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($bd);
    $book = Game::EvonyTKR::Model::Book->from_hash($bho);

    unless ($book
      && Scalar::Util::blessed($book)
      && $book->isa('Game::EvonyTKR::Model::Book')) {
      my $errmessage = sprintf('failed to load book for file "%s"', $bookFile);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    my $add_result = $job->add_builtin_book($book);
    if (defined($add_result) && $add_result == 1) {
      my $result = sprintf('imported %s', $book->name);
      $job->logger->info($result);
      $job->note(book_imported => $book);
      return $job->finish($result);
    }
    else {
      my $errmessage = sprintf('add to cache for %s failed: %s',
        $entry, $add_result // 'undef add result');
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

  }
}
1;
__END__

use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

require Game::EvonyTKR::Model::Buff::Summarizer;

package Game::EvonyTKR::External::General::Pair::Summarizer {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',              -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs',        -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use experimental qw(class);
  use List::AllUtils qw(any all none uniq);
  use Const::Fast;
  use Carp;

  state $generalCache;
  state $pair;

  has 'ypp' => sub {
    state $yp //= YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1']);
    return $yp;
  };

  has 'collection_dir' => sub {
    my $home = Mojo::Home->new->detect('Game::EvonyTKR');
    return $home->child('share/collections/data')
  };

  # call the first parameter a taskClass here
  # to emphasize that this is acting on the task as a meta-entity
  # and not on a single instance of it
  # register is the entry point when *creating a task*
  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(summarize_pair => __PACKAGE__);

    $app->plugins->emit(summarize_pair_job_ready => 1);
  }

  # here the first parameter indicates that this is prepresenting
  # a single instance. this conforms with the fact that
  # run is the entry point when *executing a job*
  has 'run' => sub ($job, @args) {
    $job->SUPER::run(@args);

    $generalCache = $job->create_general_cache()
      unless (defined($generalCache));

    my (
      $runId,                $primaryName,         $secondaryName,
      $tt,                   $activationType,      $ascendingLevel,
      $primaryCovenantLevel, $primarySpecialty1,   $primarySpecialty2,
      $primarySpecialty3,    $primarySpecialty4,   $secondaryCovenantLevel,
      $secondarySpecialty1,  $secondarySpecialty2, $secondarySpecialty3,
      $secondarySpecialty4,
    ) = shift @args;

    my $primarySpecialties = [
      $primarySpecialty1, $primarySpecialty2,
      $primarySpecialty3, $primarySpecialty4
    ];
    my $secondarySpecialties = [
      $secondarySpecialty1, $secondarySpecialty2,
      $secondarySpecialty3, $secondarySpecialty4
    ];
    my $validatedparams = $job->validatePairParams(
      $ascendingLevel,         $primaryCovenantLevel, $primarySpecialties,
      $secondaryCovenantLevel, $secondarySpecialties,
    );

    my @errmessage;
    unless (defined($runId) && length($runId)) {
      push @errmessage, sprintf('runId must be defined');
    }
    unless (defined($primaryName) && length($primaryName)) {
      push @errmessage, 'primaryName must be defined';
    }
    unless (defined($secondaryName) && length($secondaryName)) {
      push @errmessage, 'secondaryName must be defined';
    }
    unless (defined($tt) && length($tt) && $job->ValidateGeneralType($tt)) {
      push @errmessage,
        sprintf(
        'must provide a valid general type from %s, not "%s"',
        join(',',
          map { sprintf('"%s"', $_) } $job->GeneralKeys->@*,
          defined($tt) ? $tt : 'undefined')
        );
    }
    unless (defined(
           $activationType
        && length($activationType)
        && any { $activationType eq $_ }
      $job->AllowedBuffActivationValues->@*
    )) {
      push @errmessage,
        sprintf(
        'must provide a valid activation type from %s, not "%s"',
        join(',',
          map { sprintf('"%s"', $_) } $job->AllowedBuffActivationValues->@*),
        defined($activationType) ? $activationType : 'undef'
        );
    }

    my $pair = $job->get_pair($job->wire_pair_to_key({
      primary   => $primaryName,
      secondary => $secondaryName,
      type      => $tt,
    }));

    unless ($pair) {
      push @errmessage,
        sprintf(
        'cannot retrieve pair for primary "%s", secondary "%s", type "%s"',
        $primaryName, $secondaryName, $tt);
    }

    if (scalar(@errmessage)) {
      $job->logger->error(join ' ', @errmessage);
      $job->fail(join ' ', @errmessage);
      return;
    }

    #TODO get covenants;
    #TODO get specialties
    #TODO get ascending attributes
  };

  has 'load_best_skill_books' =>
    sub ($job, $general, $ascendingLevel, $targetType, $activationType) {
    my $key = $activationType eq 'PvM' ? 'PvM' : 'default';

    my @books;
    my $generic_dir       = $job->collection_dir->child('generic books');
    my @sorted_book_names = sort {
      $job->BestSkillBooks->{$targetType}->{$key}->{$a}
        <=> $job->BestSkillBooks->{$targetType}->{$key}->{$b}
    } keys %{ $job->BestSkillBooks->{$targetType}->{$key} };

    foreach my $book_name (@sorted_book_names) {
      my ($book_file) = $generic_dir->children(qr/\Q$book_name\E\.yaml/i);
      if (defined($book_file) && $book_file->is_file()) {
        my $data   = $book_file->slurp_utf8;
        my $object = $job->ypp->load_string($data);
        my $book =
          Game::EvonyTKR::Model::Book::SkillBook->from_hash($object,);

        $job->logger->info(sprintf(
          'picked book %s for general %s', $book->name, $general->name
        ));
        push @books, $book;
        last if (scalar @books >= 10);

      }
      else {
        $job->logger->warn("cannot find file for $book_name");
        next;
      }
    }
    # certain books need to be there or the buff summarizer will
    # spew errors.
    foreach my $attr ('Attack', 'Defense', 'HP') {
      foreach my $tt ('Mounted Troop', 'Ranged Troop', 'Ground Troop',
        'Siege Machine') {
        unless (any { $_->name =~ /$tt $attr/ } @books) {
          my ($book_file) =
            $generic_dir->children(qr/\QLevel 4 $tt $attr\E\.yaml$/i);
          if ($book_file && $book_file->is_file()) {
            my $data   = $book_file->slurp_utf8;
            my $object = $job->ypp->load_string($data);
            my $book =
              Game::EvonyTKR::Model::Book::SkillBook->from_hash($object);

            $job->logger->debug(sprintf(
              'picked book %s for general %s',
              $book->name, $general->name
            ));
            push @books, $book;
          }
        }
      }
    }
    return \@books;
    };
}

1;
__END__
has 'summarize_primary' => sub ($job, $activationType, $targetType) {
  my $general = $pair->primary;
  my $bestBooks =
    $job->load_best_skill_books($general, $targetType, $activationType);

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    general             => $general,
    books               => $bestBooks,
    covenant            => undef,
    ascendingAttributes => undef,
    isPrimary           => 1,
    targetType          => $targetType,
    activationType      => $activationType,
    ascendingLevel      => $ascendingLevel,
    covenantLevel       => undef,
    specialty1          => undef,
    specialty2          => undef,
    specialty3          => undef,
    specialty4          => undef,
  );
  # Do all the heavy computation here
  $bsum1->updateBuffs();
  $bsum1->updateDebuffs();
};

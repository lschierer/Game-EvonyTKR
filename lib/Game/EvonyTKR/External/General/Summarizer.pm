use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Summarizer;

package Game::EvonyTKR::External::General::Summarizer {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',              -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals',     -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Covenants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books',        -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Const::Fast;
  use List::AllUtils qw(any all none uniq);
  use Carp;

  has 'ypp' => sub {
    state $yp //= YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1']
    );
    return $yp;
  };

  has 'collection_dir' => sub {
    my $home = Mojo::Home->new->detect('Game::EvonyTKR');
    return $home->child('share/collections/data');
  };

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task(
      summarize_general => sub ($job, @args) {
        $taskClass->new(app => $app, minion => $app->minion)->run($job, @args);
      }
    );
    $app->plugins->emit(summarize_general_job_ready => 1);
  }

  sub run ($job, @args) {
    $job->SUPER::run(@args);

    my (
      $generalName,    $isPrimary,     $targetType, $activationType,
      $ascendingLevel, $covenantLevel, $specialty1, $specialty2,
      $specialty3,     $specialty4,    $books,
    ) = @args;

    # Validate required parameters
    my @errmessage;
    unless (defined($generalName) && length($generalName)) {
      push @errmessage, 'generalName must be defined';
    }
    unless (defined($targetType) && length($targetType)) {
      push @errmessage, 'targetType must be defined';
    }
    unless (defined($activationType) && length($activationType)) {
      push @errmessage, 'activationType must be defined';
    }

    if (scalar(@errmessage)) {
      $job->logger->error(join ' ', @errmessage);
      $job->fail(join ' ', @errmessage);
      return;
    }

    # Get general
    my $general = $job->get_general($generalName);
    unless ($general) {
      my $err = "Cannot retrieve general: $generalName";
      $job->logger->error($err);
      $job->fail($err);
      return;
    }

    # Get covenant
    my $covenant = $job->get_covenant($generalName);
    unless ($covenant) {
      $job->logger->warn("No covenant found for $generalName");
    }

    # Get ascending attributes (primary only)
    my $ascendingAttributes;
    if ($isPrimary && $general->ascending) {
      $general->populateAscendingAttributes();
      unless ($general->ascendingAttributes) {
        my $errmessage = sprintf('failed to get ascending attributes for "%s"',
          $general->name);
        $job->logger->error($errmessage);
        $job->fail($errmessage);
      }
      $ascendingAttributes = $general->ascendingAttributes;
    }

    # Load books if not provided
    $general->populateBuiltinBook() unless ($general->builtInBook);
# TODO: Handle partial conflicts (when other general has conflicting book)
# It may be worth displaying in the UI even though it doesn't affect a single general
# TODO: Verify book compatibility with general
    unless ($books && ref($books) eq 'ARRAY' && @$books) {
      $books = [
        $job->load_best_skill_books($general, $targetType, $activationType)->@*,
        $job->load_mandatory_skill_books()->@*,
      ];
    }

    # ensure specialities are loaded
    $general->populateSpecialties()
      unless (scalar($general->specialties)
      && all { ref($_) && $_->isa('Game::EvonyTKR::Model::Specialty') }
      $general->specialties->@*);

    # Create summarizer
    my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      general             => $general,
      books               => $books,
      covenant            => $covenant,
      ascendingAttributes => $ascendingAttributes,
      isPrimary           => $isPrimary // 1,
      targetType          => $targetType,
      activationType      => $activationType,
      ascendingLevel      => $ascendingLevel // 'red5',
      covenantLevel       => $covenantLevel  // 'civilization',
      specialty1          => $specialty1     // 'gold',
      specialty2          => $specialty2     // 'gold',
      specialty3          => $specialty3     // 'gold',
      specialty4          => $specialty4     // 'gold',
    );

    # Compute buffs and debuffs
    $summarizer->updateBuffs();
    $summarizer->updateDebuffs();

    # Return results
    $job->finish({
      general   => $generalName,
      isPrimary => $isPrimary,
      buffs     => $summarizer->buffValues,
      debuffs   => $summarizer->debuffValues,
    });
  }

  sub load_best_skill_books ($job, $general, $targetType, $activationType) {
    my $key = $activationType eq 'PvM' ? 'PvM' : 'default';

  # TODO: Implement book conflict detection
  # TODO: For pairs, need to check conflicts with other general's books
  # TODO: Handle partial conflicts (book works for self but conflicts with pair)

    my @books;
    my $generic_dir       = $job->collection_dir->child('generic books');
    my @sorted_book_names = sort {
      $job->BestSkillBooks->{$targetType}->{$key}->{$a}
        <=> $job->BestSkillBooks->{$targetType}->{$key}->{$b}
    } keys %{ $job->BestSkillBooks->{$targetType}->{$key} };

    my $level = Game::EvonyTKR::Role::Constants::Books->bestLevel;

    foreach my $book_name (@sorted_book_names) {
      my $book = $job->get_generic_book($book_name, $level);
      unless ($book && ref($book) && $book->isa('Game::EvonyTKR::Model::Book'))
      {
        $job->logger->error("Cannot find $book_name");
        next;
      }
      $job->logger->info(
        sprintf('Picked book "%s" for "%s"', $book_name, $general->name));
      push @books, $book;
      last if (scalar @books >= 3);    # Single general gets 3 books
    }

    return \@books;
  }

  sub load_mandatory_skill_books ($job) {
    my @books;
    my $level = Game::EvonyTKR::Role::Constants::Books->bestLevel;
    # Ensure required books are present for buff summarizer
    foreach my $attr ('Attack', 'Defense', 'HP') {
      foreach my $tt ('Mounted Troop', 'Ranged Troop', 'Ground Troop',
        'Siege Machine') {
        my $book_name = sprintf('Level %s %s %s', $level, $tt, $attr);
        unless (any { $_->name eq $book_name } @books) {
          my $book = $job->get_generic_book($book_name, $level);
          unless ($book) {
            $job->logger->error("Cannot find $book_name");
            next;
          }
          push @books, $book;
        }
      }
    }
    return \@books;
  }
}

1;
__END__

=head1 NAME

Game::EvonyTKR::External::General::Summarizer - Minion task for summarizing general buffs

=head1 DESCRIPTION

This task computes buff and debuff summaries for a single general. It can be used
standalone for single general tables or called by the pair summarizer for computing
pair buffs.

=head1 PARAMETERS

=over 4

=item generalName - Name of the general to summarize

=item isPrimary - Boolean, whether this is a primary general (affects ascending attributes)

=item targetType - General type (ground_specialist, mounted_specialist, etc)

=item activationType - Buff activation type (Overall, PvM, Attacking, etc)

=item ascendingLevel - Ascending level (red5, purple3, etc)

=item covenantLevel - Covenant level (civilization, faith, etc)

=item specialty1-4 - Specialty levels (gold, orange, purple, etc)

=item books - Optional arrayref of Book objects. If not provided, will compute BestBooks

=back

=cut

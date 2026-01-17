use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Summarizer::Single;

package Game::EvonyTKR::External::General::Summarizer {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',              -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Covenants',           -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Specialties',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Books',                          -role;
  use Const::Fast;
  use List::AllUtils qw(any all none uniq);
  use Carp;

  my $isStub = 0;

  # when this package is extended, call this.
  sub parentIsStub {
    $isStub = 1;
  }

  has 'ypp' => sub {
    state $yp //= YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1']
    );
    return $yp;
  };

  has [
    'generalName',   'targetType', 'activationType', 'ascendingLevel',
    'covenantLevel', 'books',      'specialty1',     'specialty2',
    'specialty3',    'specialty4'
  ] => '';
  has isPrimary => 0;

  has ['general', 'covenant'] => undef;

  has params => sub { {} };

  has summarizer => undef;

  sub task_name {'summarize_general'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    $app->plugins->emit(summarize_general_job_ready => 1);
  }

  sub populateFromArgs($job, @args) {
    $job->note(isStub => $isStub);
    if (($isStub == 0) && (scalar(@args) < 11)) {
      my $errmessage =
          'Missing required args, each of '
        . 'generalName isPrimary targetType activationType'
        . 'ascendingLevel covenantLevel '
        . 'specialty1 specialty2 specialty3 specialty4 books'
        . ' must be sent in that order.';
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->generalName($args[0])    if (scalar(@args) >= 1);
    $job->isPrimary($args[1])      if (scalar(@args) >= 2);
    $job->targetType($args[2])     if (scalar(@args) >= 3);
    $job->activationType($args[3]) if (scalar(@args) >= 4);
    $job->ascendingLevel($args[4]) if (scalar(@args) >= 5);
    $job->covenantLevel($args[5])  if (scalar(@args) >= 6);
    $job->specialty1($args[6])     if (scalar(@args) >= 7);
    $job->specialty2($args[7])     if (scalar(@args) >= 8);
    $job->specialty3($args[8])     if (scalar(@args) >= 9);
    $job->specialty4($args[9])     if (scalar(@args) >= 10);
    $job->books($args[10])         if (scalar(@args) >= 11);

    # Validate required parameters
    my @errmessage;
    unless (length($job->generalName)) {
      push @errmessage, 'generalName must be defined';
    }
    if (($isStub == 0) && (length($job->targetType) < 1)) {
      push @errmessage, 'targetType must be defined';
    }
    if (($isStub == 0) && (length($job->activationType) < 1)) {
      push @errmessage, 'activationType must be defined';
    }

    if (scalar(@errmessage)) {
      $job->logger->error(join ' ', @errmessage);
      $job->fail(join ' ', @errmessage);
      return;
    }

    # Get general
    $job->general($job->get_general($job->generalName));
    unless ($job->general) {
      my $err = sprintf('Cannot retrieve general: %s',
        ref($job->generalName)
        ? Data::Printer::np($job->generalName)
        : $job->generalName);
      $job->logger->error($err);
      return $job->fail($err);
    }

    $job->params({
      general        => $job->general,
      isPrimary      => $job->isPrimary // 1,
      targetType     => $job->targetType,
      activationType => $job->activationType,
      ascendingLevel => $job->ascendingLevel // 'red5',
      covenantLevel  => $job->covenantLevel  // 'civilization',
      specialty1     => $job->specialty1     // 'gold',
      specialty2     => $job->specialty2     // 'gold',
      specialty3     => $job->specialty3     // 'gold',
      specialty4     => $job->specialty4     // 'gold',
    });

    my $validation = $job->validateParams($job->params);
    unless ($validation == 1) {
      return $validation;
    }

    # Get covenant
    $job->covenant($job->get_covenant($job->generalName));
    unless ($job->covenant) {
      $job->logger->warn(
        sprintf('No covenant found for "%s"', $job->generalName));
    }

    # Get ascending attributes (primary only)
    if ($job->isPrimary && $job->general->ascending) {
      $job->general->populateAscendingAttributes();
      unless ($job->general->ascendingAttributes) {
        my $errmessage = sprintf('failed to get ascending attributes for "%s"',
          $job->general->name);
        $job->logger->error($errmessage);
        $job->fail($errmessage);
      }
    }

    # Load books if not provided
    $job->general->populateBuiltinBook() unless ($job->general->builtInBook);

# TODO: Handle partial conflicts (when other general has conflicting book)
# It may be worth displaying in the UI even though it doesn't affect a single general
# TODO: Verify book compatibility with general
    unless ($job->books
      && ref($job->books)
      && ref($job->books) eq 'ARRAY'
      && scalar(@{ $job->books }) >= 3) {
      my $tt =
        length($job->targetType) ? $job->targetType : $job->general->type->[0];
      my $at = length($job->activationType) ? $job->activationType : 'default';
      $job->books([
        $job->load_best_skill_books($job->general, $tt, $at)->@*,
        $job->load_mandatory_skill_books()->@*,
      ]);
    }

    # ensure specialities are loaded
    $job->general->populateSpecialties()
      unless (scalar($job->general->specialties)
      && all { ref($_) && $_->isa('Game::EvonyTKR::Model::Specialty') }
      $job->general->specialties->@*);
    return 1;
  }

  sub run ($job, @args) {
    $job->SUPER::run(@args);

    my $populateSuccess = $job->populateFromArgs(@args);
    unless ($populateSuccess && $populateSuccess == 1) {
      return $populateSuccess;
    }

    # if this is a stub, then the parent extending this package
    # is responsible to run the summary and finish the task.
    # return early from this run method so that we do not
    # accidentally end the whole task.
    return if ($isStub);

    $job->summarize();

    # Return results
    return $job->finish({
      general   => $job->generalName,
      isPrimary => $job->isPrimary,
      buffs     => $job->summarizer->buffValues,
      debuffs   => $job->summarizer->debuffValues,
    });
  }

  sub summarize ($job) {
    # Note serializable params for debugging
    $job->note(
      Summarizer_params => {
        $job->params->%*,
        has_covenant  => defined($job->covenant)                     ? 1 : 0,
        has_ascending => defined($job->general->ascendingAttributes) ? 1 : 0,
        book_count    => scalar(@{ $job->books }),
        book_names    => [map { $_->name } @{ $job->books }],
      }
    );

    # Create summarizer
    $job->summarizer(
      Game::EvonyTKR::Model::Buff::Summarizer::Single->new($job->params->%*));

    # Compute buffs and debuffs
    $job->summarizer->updateBuffs();
    $job->summarizer->updateDebuffs();
  }

  sub validateParams($job, $params) {
    unless (ref($params->{general})
      && blessed($params->{general})
      && $params->{general}->isa('Game::EvonyTKR::Model::General')) {
      my $em = sprintf('job id "%s" requires a General, not "%s"',
        $job->info->{id},
        ref($params->{general}) ? blessed($params->{general}) : 'scalar');

      $job->logger->error($em);
      return $job->fail($em);
    }

    if ($params->{isPrimary}) {
      if ($params->{ascendingLevel} =~ /red/) {
        unless (any { $_ eq $params->{ascendingLevel} }
          $job->AscendingAttributeLevelValues(1)) {
          my $em = sprintf(
            'red ascending level "%s" is invalid, must be one of %s',
            $params->{ascendingLevel},
            join ', ',
            map { sprintf('"%s"', $_) } $job->AscendingAttributeLevelValues(1),
          );
          $job->logger->error($em);
          return $job->fail($em);
        }
      }
      else {
        unless (any { $_ eq $params->{ascendingLevel} }
          $job->AscendingAttributeLevelValues(0)) {
          my $em = sprintf(
            'ascending level "%s" is invalid, must be one of %s',
            $params->{ascendingLevel},
            join ', ',
            map { sprintf('"%s"', $_) } $job->AscendingAttributeLevelValues(0),
          );
          $job->logger->error($em);
          return $job->fail($em);
        }
      }
    }

    if ($job->covenantLevel eq '') {
      $job->covenantLevel('none');
      $job->params->{covenantLevel} = 'none';
    }

    unless (any { $_ eq $params->{covenantLevel} }
      $job->CovenantCategoryValues->@*) {
      my $em = sprintf(
        'covenant level "%s" is invalid, must be one of %s',
        $params->{covenantLevel},
        join ', ', map { sprintf('"%s"', $_) } $job->CovenantCategoryValues->@*
      );
      $job->logger->error($em);
      return $job->fail($em);
    }

    foreach my $index (1 .. 4) {
      my $specialtyLevel = $params->{"specialty${index}"};
      unless ($job->is_valid_specialty_level($specialtyLevel)) {
        my $em = sprintf('specialty level "%s" is invalid, must be one of %s',
          $specialtyLevel, join ', ',
          map { sprintf('"%s"', $_) } $job->SpecialtyLevelValues->@*);
        $job->logger->error($em);
        return $job->fail($em);
      }
    }

    return 1;
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

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

  has 'ypp' => sub {
    state $yp //= YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1']
    );
    return $yp;
  };

  sub task_name {'summarize_general'}

  sub register ($taskClass, $app, $conf = {}) {
    return unless $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
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
      $job->log_error(join ' ', @errmessage);
      $job->fail(join ' ', @errmessage);
      return;
    }

    # Get general
    my $general = $job->get_general($generalName);
    unless ($general) {
      my $err = sprintf('Cannot retrieve general: %s',
        ref($generalName) ? Data::Printer::np($generalName) : $generalName);
      $job->log_error($err);
      return $job->fail($err);
    }

    # Get covenant
    my $covenant = $job->get_covenant($generalName);
    unless ($covenant) {
      $job->log_warn("No covenant found for $generalName");
    }

    # Get ascending attributes (primary only)
    if ($isPrimary && $general->ascending) {
      $general->populateAscendingAttributes();
      unless ($general->ascendingAttributes) {
        my $errmessage = sprintf('failed to get ascending attributes for "%s"',
          $general->name);
        $job->log_error($errmessage);
        $job->fail($errmessage);
      }
    }

    # Load books if not provided
    $general->populateBuiltinBook() unless ($general->builtInBook);
# TODO: Handle partial conflicts (when other general has conflicting book)
# It may be worth displaying in the UI even though it doesn't affect a single general
# TODO: Verify book compatibility with general
    unless ($books && ref($books) eq 'ARRAY' && @$books && scalar(@$books) >= 3)
    {
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

    my $params = {
      general        => $general,
      isPrimary      => $isPrimary // 1,
      targetType     => $targetType,
      activationType => $activationType,
      ascendingLevel => $ascendingLevel // 'red5',
      covenantLevel  => $covenantLevel  // 'civilization',
      specialty1     => $specialty1     // 'gold',
      specialty2     => $specialty2     // 'gold',
      specialty3     => $specialty3     // 'gold',
      specialty4     => $specialty4     // 'gold',
    };

    $job->validateParams($params);

    # Note serializable params for debugging
    $job->note(
      Summarizer_params => {
        generalName    => $generalName,
        isPrimary      => $isPrimary // 1,
        targetType     => $targetType,
        activationType => $activationType,
        ascendingLevel => $ascendingLevel // 'red5',
        covenantLevel  => $covenantLevel  // 'civilization',
        specialty1     => $specialty1     // 'gold',
        specialty2     => $specialty2     // 'gold',
        specialty3     => $specialty3     // 'gold',
        specialty4     => $specialty4     // 'gold',
        has_covenant   => defined($covenant)                     ? 1 : 0,
        has_ascending  => defined($general->ascendingAttributes) ? 1 : 0,
        book_count     => scalar(@$books),
        book_names     => [map { $_->name } @$books],
      }
    );

    # Create summarizer
    my $summarizer =
      Game::EvonyTKR::Model::Buff::Summarizer::Single->new($params->%*);

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

  sub validateParams($job, $params) {
    unless (ref($params->{general})
      && blessed($params->{general})
      && $params->{general}->isa('Game::EvonyTKR::Model::General')) {
      my $em = sprintf('job id "%s" requires a General, not "%s"',
        $job->info->{id},
        ref($params->{general}) ? blessed($params->{general}) : 'scalar');

      $job->log_error($em);
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
          $job->log_error($em);
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
          $job->log_error($em);
          return $job->fail($em);
        }
      }

    }

    unless (any { $_ eq $params->{covenantLevel} }
      $job->CovenantCategoryValues->@*) {
      my $em = sprintf(
        'covenant level "%s" is invalid, must be one of %s',
        $params->{covenantLevel},
        join ', ', map { sprintf('"%s"', $_) } $job->CovenantCategoryValues->@*
      );
      $job->log_error($em);
      return $job->fail($em);
    }

    foreach my $index (1 .. 4) {
      my $specialtyLevel = $params->{"specialty${index}"};
      unless ($job->is_valid_specialty_level($specialtyLevel)) {
        my $em = sprintf('specialty level "%s" is invalid, must be one of %s',
          $specialtyLevel, join ', ',
          map { sprintf('"%s"', $_) } $job->SpecialtyLevelValues->@*);
        $job->log_error($em);
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

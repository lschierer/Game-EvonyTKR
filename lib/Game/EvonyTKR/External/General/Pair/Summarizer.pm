use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Summarizer::Pair;

package Game::EvonyTKR::External::General::Pair::Summarizer {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',              -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Covenants',           -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Specialties',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Pairs',             -role;
  use List::AllUtils qw(any all none uniq);
  use Mojo::JSON     qw(encode_json);
  use Scalar::Util   qw(blessed);
  use Const::Fast;
  use Carp;

  sub task_name {'summarize_pair'}

  sub register ($taskClass, $app, $conf = {}) {
    return unless $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    $app->plugins->emit(summarize_pair_job_ready => 1);
  }

  sub run ($job, @args) {
    $job->SUPER::run(@args);

    my $params        = shift @args;
    my $runId         = $params->{runId};
    my $primaryName   = $params->{primaryName};
    my $secondaryName = $params->{secondaryName};

    my $activationType = $params->{activationType};
    $params->{ascendingLevel}         //= 'none';
    $params->{primaryCovenantLevel}   //= 'none';
    $params->{primarySpecialty1}      //= 'none';
    $params->{primarySpecialty2}      //= 'none';
    $params->{primarySpecialty3}      //= 'none';
    $params->{primarySpecialty4}      //= 'none';
    $params->{secondaryCovenantLevel} //= 'none';
    $params->{secondarySpecialty1}    //= 'none';
    $params->{secondarySpecialty2}    //= 'none';
    $params->{secondarySpecialty3}    //= 'none';
    $params->{secondarySpecialty4}    //= 'none';

    # Validate required parameters
    my @errmessage;
    unless (defined($runId) && length($runId)) {
      push @errmessage, 'runId must be defined';
    }
    unless (defined($primaryName) && length($primaryName)) {
      push @errmessage, 'primaryName must be defined';
    }
    unless (defined($secondaryName) && length($secondaryName)) {
      push @errmessage, 'secondaryName must be defined';
    }
    unless (exists($params->{targetType}) && length($params->{targetType})) {
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

    # Get both generals
    $job->log_debug(sprintf(
      'Getting primary general "%s" (normalized: "%s")',
      $primaryName, $job->normalize($primaryName)
    ));
    my $primary = $job->get_general(lc($job->normalize($primaryName)));
    unless ($primary) {
      my $err = sprintf('Cannot retrieve primary general: %s', $primaryName);
      $job->log_error($err);
      return $job->fail($err);
    }
    $job->log_debug(sprintf('Got primary general: %s', $primary->name));

    $job->log_debug(sprintf(
      'Getting secondary general "%s" (normalized: "%s")',
      $secondaryName, $job->normalize($secondaryName)
    ));
    my $secondary = $job->get_general(lc($job->normalize($secondaryName)));
    unless ($secondary) {
      my $err =
        sprintf('Cannot retrieve secondary general: %s', $secondaryName);
      $job->log_error($err);
      return $job->fail($err);
    }
    $job->log_debug(sprintf('Got secondary general: %s', $secondary->name));

    # Get the pair object
    my $pair = $job->get_pair($job->wire_pair_to_key({
      primary   => $primaryName,
      secondary => $secondaryName,
      type      => $params->{targetType},
    }));

    unless ($pair) {
      my $err = sprintf(
        'Cannot retrieve pair for primary "%s", secondary "%s", type "%s"',
        $primaryName, $secondaryName, $params->{targetType});
      $job->log_error($err);
      return $job->fail($err);
    }

    # Get covenants
    $job->log_debug(sprintf('Getting covenant for primary: %s', $primaryName));
    my $primaryCovenant = $job->get_covenant($primaryName);
    unless ($primaryCovenant) {
      $job->log_warn("No covenant found for primary $primaryName");
    }
    else {
      $job->log_debug(sprintf('Got covenant for primary: %s', $primaryName));
    }

    $job->log_debug(sprintf('Getting covenant for secondary: %s', $secondaryName));
    my $secondaryCovenant = $job->get_covenant($secondaryName);
    unless ($secondaryCovenant) {
      $job->log_warn("No covenant found for secondary $secondaryName");
    }
    else {
      $job->log_debug(sprintf('Got covenant for secondary: %s', $secondaryName));
    }

    # Get ascending attributes (primary only)
    if ($pair->primary->ascending) {
      $pair->primary->populateAscendingAttributes();
      unless ($pair->primary->ascendingAttributes) {
        my $errmessage =
          sprintf('Failed to get ascending attributes for primary "%s"',
          $pair->primary->name);
        $job->log_error($errmessage);
        $job->fail($errmessage);
      }
    }

    # Populate builtin books for both generals in the pair
    $pair->primary->populateBuiltinBook() unless ($pair->primary->builtInBook);
    $pair->secondary->populateBuiltinBook()
      unless ($pair->secondary->builtInBook);

    # Populate specialties for both generals in the pair
    $pair->primary->populateSpecialties()
      unless (scalar($pair->primary->specialties->@*)
      && all { ref($_) && $_->isa('Game::EvonyTKR::Model::Specialty') }
      $pair->primary->specialties->@*);

    $pair->secondary->populateSpecialties()
      unless (scalar($pair->secondary->specialties->@*)
      && all { ref($_) && $_->isa('Game::EvonyTKR::Model::Specialty') }
      $pair->secondary->specialties->@*);

    # Debug: verify specialties are loaded
    $job->log_debug(sprintf(
      'Primary %s has %d specialties, Secondary %s has %d specialties',
      $pair->primary->name,   scalar($pair->primary->specialties->@*),
      $pair->secondary->name, scalar($pair->secondary->specialties->@*)
    ));

    $job->validateParams($params);

    # Add the pair object to params for the summarizer
    $params->{pair} = $pair;
    # Convert targetType to proper troop type key
    $params->{targetType} = $job->string_to_trooptype($params->{targetType});
    $job->log_debug(sprintf(
      '%s using tt %s to retrieve results',
      __PACKAGE__, $params->{targetType}
    ));

    # Map primary covenant/specialty params to base Summarizer attributes
    $params->{covenantLevel} = $params->{primaryCovenantLevel};
    $params->{specialty1}    = $params->{primarySpecialty1};
    $params->{specialty2}    = $params->{primarySpecialty2};
    $params->{specialty3}    = $params->{primarySpecialty3};
    $params->{specialty4}    = $params->{primarySpecialty4};

    # Note serializable params for debugging
    $job->note(
      PairSummarizer_params => {
        $params->%*,
        pair => {
          primary   => $pair->primary->to_hash(),
          secondary => $pair->secondary->to_hash(),
          type      => $pair->type
        },
        has_primary_covenant   => defined($primaryCovenant)     ? 1 : 0,
        has_secondary_covenant => defined($secondaryCovenant)   ? 1 : 0,
        has_ascending => defined($primary->ascendingAttributes) ? 1 : 0,
      }
    );

    # Create summarizer
    $job->log_debug('Creating pair summarizer');
    my $summarizer =
      Game::EvonyTKR::Model::Buff::Summarizer::Pair->new($params->%*);

    # Compute buffs and debuffs
    $job->log_debug('Computing buffs');
    $summarizer->updateBuffs();
    $job->log_debug('Computing debuffs');
    $summarizer->updateDebuffs();

    # Load full general objects for serialization
    my $primary_general   = $job->get_general($primaryName);
    my $secondary_general = $job->get_general($secondaryName);

    unless ($primary_general && $secondary_general) {
      return $job->fail(
        "Failed to load generals: $primaryName, $secondaryName");
    }

    # Flatten buffs/debuffs to match client schema
    my $buffs   = $summarizer->pairBuffValues;
    my $debuffs = $summarizer->pairDebuffValues;

    $job->log_debug(sprintf(
      'Buff values for %s/%s: %s',
      $primaryName, $secondaryName,
      Data::Printer::np($buffs, multiline => 0)
    ));

    # Return results
    $job->finish({
      status => 'complete',
      result => encode_json({
        runId => $runId,
        data  => {
          primary     => $primary_general->to_hash(),
          secondary   => $secondary_general->to_hash(),
          marchbuff   => $buffs->{ $params->{targetType} }->{'March Size'} // 0,
          attackbuff  => $buffs->{ $params->{targetType} }->{'Attack'}     // 0,
          defensebuff => $buffs->{ $params->{targetType} }->{'Defense'}    // 0,
          hpbuff      => $buffs->{ $params->{targetType} }->{'HP'}         // 0,
          groundattackdebuff   => $debuffs->{'Ground Troops'}->{'Attack'}  // 0,
          grounddefensedebuff  => $debuffs->{'Ground Troops'}->{'Defense'} // 0,
          groundhpdebuff       => $debuffs->{'Ground Troops'}->{'HP'}      // 0,
          mountedattackdebuff  => $debuffs->{'Mounted Troops'}->{'Attack'} // 0,
          mounteddefensedebuff => $debuffs->{'Mounted Troops'}->{'Defense'}
            // 0,
          mountedhpdebuff     => $debuffs->{'Mounted Troops'}->{'HP'}      // 0,
          rangedattackdebuff  => $debuffs->{'Ranged Troops'}->{'Attack'}   // 0,
          rangeddefensedebuff => $debuffs->{'Ranged Troops'}->{'Defense'}  // 0,
          rangedhpdebuff      => $debuffs->{'Ranged Troops'}->{'HP'}       // 0,
          siegeattackdebuff   => $debuffs->{'Siege Machines'}->{'Attack'}  // 0,
          siegedefensedebuff  => $debuffs->{'Siege Machines'}->{'Defense'} // 0,
          siegehpdebuff       => $debuffs->{'Siege Machines'}->{'HP'}      // 0,
        }
      })
    });
  }

  sub validateParams($job, $params) {

    my $key = $job->wire_pair_to_key({
      type      => $params->{targetType},
      primary   => $params->{primaryName},
      secondary => $params->{secondaryName},
    });
    $params->{pair} = $job->get_pair($key);
    unless (ref($params->{pair})
      && blessed($params->{pair})
      && $params->{pair}->isa('Game::EvonyTKR::Model::General::Pair')) {
      my $em = sprintf('job id %s cannot find a valid Pair for params %s',
        $job->info->{id}, Data::Printer::np($params, multiline => 0));

      $job->log_error($em);
      return $job->fail($em);
    }

    # Validate ascending level
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

    # Validate covenant levels
    foreach my $level_param (qw(primaryCovenantLevel secondaryCovenantLevel)) {
      unless (any { $_ eq $params->{$level_param} }
        $job->CovenantCategoryValues->@*) {
        my $em = sprintf(
          '%s "%s" is invalid, must be one of %s',
          $level_param, $params->{$level_param},
          join ', ',
          map { sprintf('"%s"', $_) } $job->CovenantCategoryValues->@*
        );
        $job->log_error($em);
        return $job->fail($em);
      }
    }

    # Validate primary specialty levels (1-4)
    foreach my $index (1 .. 4) {
      my $specialtyLevel = $params->{"specialty${index}"};
      unless ($job->is_valid_specialty_level($specialtyLevel)) {
        my $em = sprintf(
          'primary specialty%d level "%s" is invalid, must be one of %s',
          $index, $specialtyLevel, join ', ',
          map { sprintf('"%s"', $_) } $job->SpecialtyLevelValues->@*);
        $job->log_error($em);
        return $job->fail($em);
      }
    }

    # Validate secondary specialty levels (1-4)
    foreach my $index (1 .. 4) {
      my $specialtyLevel = $params->{"secondarySpecialty${index}"};
      unless ($job->is_valid_specialty_level($specialtyLevel)) {
        my $em = sprintf(
          'secondary specialty%d level "%s" is invalid, must be one of %s',
          $index, $specialtyLevel, join ', ',
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

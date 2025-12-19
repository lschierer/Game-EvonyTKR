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
  use Mojo::Base 'Game::EvonyTKR::Role::JSON',                           -role;
  use List::AllUtils qw(any all none uniq);
  use Scalar::Util   qw(blessed);
  use Const::Fast;
  use Carp;
  use Time::HiRes qw(time);

  sub task_name {'summarize_pair'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    $app->plugins->emit(summarize_pair_job_ready => 1);
  }

  sub run ($job, @args) {
    my $t_start = time();
    $job->SUPER::run(@args);

    my $params        = shift @args;
    my $cached_buffs  = shift @args;                # Optional precomputed buffs
    my $runId         = $params->{runId};
    my $primaryName   = $params->{primaryName};
    my $secondaryName = $params->{secondaryName};

    my %t;

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

    # Extract precomputed buffs if provided
    my $primary_buffs = $cached_buffs ? $cached_buffs->{primary_buffs} : undef;
    my $secondary_buffs =
      $cached_buffs ? $cached_buffs->{secondary_buffs} : undef;

    if ($primary_buffs) {
      $job->log_debug(
        "Using precomputed buffs for primary general: $primaryName");
    }
    if ($secondary_buffs) {
      $job->log_debug(
        "Using precomputed buffs for secondary general: $secondaryName");
    }

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
    my $primary = $job->get_general(lc($job->normalize($primaryName)));
    unless ($primary) {
      my $err = sprintf('Cannot retrieve primary general: %s', $primaryName);
      $job->log_error($err);
      return $job->fail($err);
    }

    my $secondary = $job->get_general(lc($job->normalize($secondaryName)));
    unless ($secondary) {
      my $err =
        sprintf('Cannot retrieve secondary general: %s', $secondaryName);
      $job->log_error($err);
      return $job->fail($err);
    }
    $t{get_generals} = time() - $t_start;

    # Get the pair object
    my $pair = $job->get_pair($job->wire_pair_to_key({
      primary   => $primaryName,
      secondary => $secondaryName,
      type      => $params->{targetType},
    }));
    $t{get_pair} = time() - $t_start - $t{get_generals};

    unless ($pair) {
      my $err = sprintf(
        'Cannot retrieve pair for primary "%s", secondary "%s", type "%s"',
        $primaryName, $secondaryName, $params->{targetType});
      $job->log_error($err);
      return $job->fail($err);
    }

    # Get covenants
    my $t_cov_start       = time();
    my $primaryCovenant   = $job->get_covenant($primaryName);
    my $secondaryCovenant = $job->get_covenant($secondaryName);
    $t{get_covenants} = time() - $t_cov_start;

    # Get ascending attributes (primary only)
    my $t_asc_start = time();
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
    $t{populate_ascending} = time() - $t_asc_start;

    # Populate builtin books for both generals in the pair
    my $t_books_start = time();
    $pair->primary->populateBuiltinBook() unless ($pair->primary->builtInBook);
    $pair->secondary->populateBuiltinBook()
      unless ($pair->secondary->builtInBook);
    $t{populate_books} = time() - $t_books_start;

    # Populate specialties for both generals in the pair
    my $t_spec_start = time();
    $pair->primary->populateSpecialties()
      unless (scalar($pair->primary->specialties->@*)
      && all { ref($_) && $_->isa('Game::EvonyTKR::Model::Specialty') }
      $pair->primary->specialties->@*);

    $pair->secondary->populateSpecialties()
      unless (scalar($pair->secondary->specialties->@*)
      && all { ref($_) && $_->isa('Game::EvonyTKR::Model::Specialty') }
      $pair->secondary->specialties->@*);
    $t{populate_specialties} = time() - $t_spec_start;

    my $t_validate_start = time();
    $job->validateParams($params);
    $t{validate_params} = time() - $t_validate_start;

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
    my $t_create_start = time();
    my $summarizer =
      Game::EvonyTKR::Model::Buff::Summarizer::Pair->new($params->%*);
    $t{create_summarizer} = time() - $t_create_start;

    # Compute buffs and debuffs
    $job->log_debug('Computing buffs');
    my $t_buffs_start = time();
    $summarizer->updatePrimaryBuffs($primary_buffs);
    # Opportunistic cache warming: if we had to compute, store for next time
    if (!$primary_buffs) {
      my $primary_key = $job->generate_buff_cache_key(
        $primaryName, 1, $params->{targetType}, $params->{activationType},
        $params->{ascendingLevel}, $params->{primaryCovenantLevel},
        $params->{primarySpecialty1}, $params->{primarySpecialty2},
        $params->{primarySpecialty3}, $params->{primarySpecialty4}
      );
      my $computed_primary = $summarizer->pairBuffValues;
      if ($computed_primary) {
        $job->store_buff_cache($primary_key, $computed_primary);
        $job->log_debug("Opportunistically cached primary buffs for $primaryName");
      }
    }

    $summarizer->updateSecondaryBuffs($secondary_buffs);
    $t{update_buffs} = time() - $t_buffs_start;

    $job->log_debug('Computing debuffs');
    my $t_debuffs_start = time();
    $summarizer->updateDebuffs();
    $t{update_debuffs} = time() - $t_debuffs_start;

    # Reuse already-loaded generals for serialization (no need to reload)
    my $primary_general   = $primary;
    my $secondary_general = $secondary;

    # Flatten buffs/debuffs to match client schema
    my $buffs   = $summarizer->pairBuffValues;
    my $debuffs = $summarizer->pairDebuffValues;

    $job->log_debug(sprintf(
      'Buff values for %s/%s: %s',
      $primaryName, $secondaryName,
      Data::Printer::np($buffs, multiline => 0)
    ));

    # Log timing breakdown
    my $t_total = time() - $t_start;
    $job->log_info(sprintf(
      'TIMING: total=%.3fs get_generals=%.3fs get_pair=%.3fs get_cov=%.3fs '
        . 'pop_asc=%.3fs pop_books=%.3fs pop_spec=%.3fs validate=%.3fs '
        . 'create_sum=%.3fs buffs=%.3fs debuffs=%.3fs',
      $t_total,
      $t{get_generals}         // 0,
      $t{get_pair}             // 0,
      $t{get_covenants}        // 0,
      $t{populate_ascending}   // 0,
      $t{populate_books}       // 0,
      $t{populate_specialties} // 0,
      $t{validate_params}      // 0,
      $t{create_summarizer}    // 0,
      $t{update_buffs}         // 0,
      $t{update_debuffs}       // 0
    ));

    # Return results
    $job->finish({
      status => 'complete',
      result => $job->encode({
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

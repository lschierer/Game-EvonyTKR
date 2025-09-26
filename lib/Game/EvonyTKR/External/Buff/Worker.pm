use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require Log::Log4perl;
require MIME::Base64;
require Path::Tiny;
require YAML::PP;
require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;
require Game::EvonyTKR::Model::AscendingAttributes;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Model::Book::Builtin;
require Game::EvonyTKR::Model::Book::SkillBook;
require Game::EvonyTKR::Model::Covenant;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Model::Specialty;
require Game::EvonyTKR::Model::Data;

package Game::EvonyTKR::External::Buff::Worker {
  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);

    $app->minion->add_task(
      pair_worker => sub ($job, $args) {
        eval {
          $logger->debug(sprintf(
            'pair worker job starting for runId %s pair %s/%s',
            $args->{runId}, $args->{general1}, $args->{general2},
          ));
          my $debug  = $app->mode eq 'development';
          my $worker = WorkerLogic->new(debug_enabled => $debug);
          my $result = $worker->calculate_buffs($args);
          $logger->debug(sprintf('result is %s', $result));
          $job->note(result => $result);
          my $runId = $args->{runId};
          $job->finish({
            status => 'complete',
            result  => $result,
          });
        };
        if ($@) {
          $logger->error("Job " . $job->id . " failed: $@");
          $job->fail($@);
        }

      }
    );
  }

  class WorkerLogic : isa(Game::EvonyTKR::Shared::Constants) {
    use Hash::Util    qw(lock_hash lock_hash_recurse lock_value);
    use Log::Log4perl qw(:levels);
    use Unicode::Normalize;
    use Unicode::CaseFold qw(fc);
    use Encode qw(is_utf8 decode_utf8 encode_utf8);
    use Carp;

    field $debug_enabled : param = 0;
    field $logger;

    field $mode;
    field $collection_dir;
    field $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    field $validator = Game::EvonyTKR::Model::Data->new();

    field $general1;
    field $general2;

    field $covenant1;
    field $covenant2;

    field $ascendingAttributes1;
    field $ascendingAttributes2;
    field $params;

    field $runId = 0;

    field $BestSkillBooks = {
      ground_specialist => {
        default => {
          'Level 4 Ground Troop Attack'       => 1,
          'Level 4 March Size'                => 2,
          'Level 4 Ground Troop HP'           => 3,
          'Level 4 Ground Troop Defense'      => 4,
          'Level 4 Siege Machine Range Bonus' => 5,
          'Level 4 Ranged Troop Range Bonus'  => 6,
          'Level 4 Ranged Troop Attack'       => 7,
          'Level 4 Ranged Troop HP'           => 8,
          'Level 4 Ranged Troop Defense'      => 9,
          'Level 4 Siege Machine Attack'      => 10,
        },
        PvM => {
          'Level 4 Ground Troop Attack Against Monster'  => 1,
          'Level 4 Ground Troop Attack'                  => 2,
          'Level 4 March Size'                           => 3,
          'Level 4 Ground Troop HP Against Monster'      => 4,
          'Level 4 Ground Troop HP'                      => 5,
          'Level 4 Ground Troop Defense Against Monster' => 6,
          'Level 4 Ground Troop Defense'                 => 7,
          'Level 4 Luck'                                 => 8,
        }
      },
      mounted_specialist => {
        default => {
          'Level 4 Mounted Troop Attack'      => 1,
          'Level 4 March Size'                => 2,
          'Level 4 Mounted Troop HP'          => 3,
          'Level 4 Mounted Troop Defense'     => 4,
          'Level 4 Siege Machine Range Bonus' => 5,
          'Level 4 Ranged Troop Range Bonus'  => 6,
          'Level 4 Ground Troop Attack'       => 7,
          'Level 4 Ground Troop HP'           => 8,
          'Level 4 Ground Troop Defense'      => 9,
          'Level 4 Siege Machine Attack'      => 10,
        },
        PvM => {
          'Level 4 Mounted Troop Attack Against Monster'  => 1,
          'Level 4 Mounted Troop Attack'                  => 2,
          'Level 4 March Size'                            => 3,
          'Level 4 Mounted Troop HP Against Monster'      => 4,
          'Level 4 Mounted Troop HP'                      => 5,
          'Level 4 Mounted Troop Defense Against Monster' => 6,
          'Level 4 Mounted Troop Defense'                 => 7,
          'Level 4 Luck'                                  => 8,
        },
      },
      ranged_specialist => {
        default => {
          'Level 4 Ranged Troop Attack'       => 1,
          'Level 4 March Size'                => 2,
          'Level 4 Ranged Troop HP'           => 3,
          'Level 4 Ranged Troop Defense'      => 4,
          'Level 4 Ranged Troop Range Bonus'  => 5,
          'Level 4 Siege Machine Range Bonus' => 6,
          'Level 4 Mounted Troop Attack'      => 7,
          'Level 4 Mounted Troop HP'          => 8,
          'Level 4 Mounted Troop Defense'     => 9,
          'Level 4 Siege Machine Attack'      => 10,
        },
        PvM => {
          'Level 4 Ranged Troop Attack Against Monster'  => 1,
          'Level 4 Ranged Troop Attack'                  => 2,
          'Level 4 March Size'                           => 3,
          'Level 4 Ranged Troop Range Bonus'             => 4,
          'Level 4 Ranged Troop HP Against Monster'      => 5,
          'Level 4 Ranged Troop HP'                      => 6,
          'Level 4 Ranged Troop Defense Against Monster' => 7,
          'Level 4 Ranged Troop Defense'                 => 8,
          'Level 4 Luck'                                 => 9,
        },
      },
      siege_specialist => {
        default => {
          'Level 4 Siege Machine Attack'      => 1,
          'Level 4 March Size'                => 2,
          'Level 4 Siege Machine HP'          => 3,
          'Level 4 Siege Machine Defense'     => 4,
          'Level 4 Siege Machine Range Bonus' => 5,
          'Level 4 Ranged Troop Range Bonus'  => 6,
          'Level 4 Ranged Troop Attack'       => 7,
          'Level 4 Ranged Troop HP'           => 8,
          'Level 4 Ranged Troop Defense'      => 9,
          'Level 4 Mounted Troop Attack'      => 10,
        },
        # DO NOT USE SIEGE AGAINST MONSTERS!!!
        PvM => {
          'Level 4 March Size'                => 1,
          'Level 4 Siege Machine Attack'      => 2,
          'Level 4 Siege Machine Range Bonus' => 3,
          'Level 4 Siege Machine HP'          => 4,
          'Level 4 Siege Machine Defense'     => 5,
          'Level 4 Luck'                      => 6,
        }
      }
    };

    ADJUST {

      $logger = Log::Log4perl->get_logger(__CLASS__);

      lock_hash_recurse(%$BestSkillBooks);
      my $distdir = File::Share::dist_dir('Game::EvonyTKR');
      $distdir        = Path::Tiny::path($distdir);
      $collection_dir = $distdir->child('collections/data/');

    }

    method worker_croak ($error_msg) {
      if ($debug_enabled) {
        if (defined($logger)) {
          $logger->logcroak($error_msg);
        }
        else {
          croak($error_msg);
        }
      }
      else {
        if (defined($logger)) {
          $logger->error($error_msg);
        }
        my $error_response = {
          runId => 0+ $runId,
          error => $error_msg
        };
        my $json =
          JSON::PP->new->utf8(0)->allow_blessed->convert_blessed->canonical;

        return MIME::Base64::encode_base64($json->encode($error_response));
        exit 1;
      }
    }

    method calculate_buffs ($args) {

      unless (length($args->{mode})) {
        $self->worker_croak('--mode is required');
      }
      unless ($args->{mode} eq 'single' or $args->{mode} eq 'pair') {
        $self->worker_croak(
          sprintf('mode must be "single" or "pair" not "%s".', $args->{mode}));
      }
      $mode = $args->{mode};

      return $self->execute($args);

    }

    method execute($opt) {
      if ($mode eq 'pair') {
        return $self->execute_pair($opt);
      }
      else {
        return $self->execute_single($opt);
      }
    }

    method validatePairParams($opt) {

      if ($opt->{runId}) {
        $runId = 0+ $opt->{runId};
      }
      else {
        $self->worker_croak('runId is required');
      }

      unless (length($opt->{general1})) {
        $self->worker_croak('--general1 is required');
      }
      if ($mode eq 'pair') {
        unless (length($opt->{general2})) {
          $self->worker_croak('--general2 is required in pair mode');
        }
        if ($opt->{isPrimary}) {
          $self->worker_croak('isPrimary cannot be specified in Pair mode');
        }
      }
      else {
        if (length($opt->{general2})) {
          $self->worker_croak('--general2 is not permitted in single mode');
        }
      }

      if ($opt->{general1} && length($opt->{general1})) {
        $general1 = $self->load_general($opt->{general1});
        unless ($general1) {
          $self->worker_croak(
            sprintf('general1 %s is not available', $opt->{general1}));
        }
      }
      else {
        $self->worker_croak('general1 was not specified.');
      }

      unless ($opt->{targetType} && length($opt->{targetType})) {
        my $type =
          ref $general1->type eq 'ARRAY'
          ? $general1->type->[0]
          : $general1->type;
        $logger->warn('targetType was not set!!'
            . 'falling back to the first type from general1'
            . $type);
        $opt->{targetType} = $type;
      }
      $logger->info('targetType is ' . $opt->{targetType});

      if ($opt->{general2} && length($opt->{general2})) {
        $general2 = $self->load_general($opt->{general2});
        unless ($general2) {
          $self->worker_croak(
            sprintf('general2 %s is not available', $opt->{general2}));
        }
      }
      else {
        $self->worker_croak('general2 was not specified.');
      }

      # Validate ascending level
      unless ($opt->{ascendingLevel} && length($opt->{ascendingLevel})) {
        $logger->warn('undefined ascendingLevel, using default "red5"');
        $opt->{ascendingLevel} = 'red5';
      }
      unless ($validator->checkAscendingLevel($opt->{ascendingLevel})) {
        $logger->warn(sprintf(
          'Invalid ascendingLevel: "%s" , using default "red5"',
          $opt->{ascendingLevel}));
        $opt->{ascendingLevel} = 'red5';
      }

      unless ($opt->{primaryCovenantLevel}
        && length($opt->{primaryCovenantLevel})) {
        $logger->warn(
          'convenant level not defined, using default "civilization"');
        $opt->{primaryCovenantLevel} = 'civilization';
      }
      unless ($validator->checkCovenantLevel($opt->{primaryCovenantLevel})) {
        $logger->warn(
          sprintf('Invalid covenantLevel using default "civilization"'));
        $logger->warn('invalid value was ' . $opt->{primaryCovenantLevel});
        $opt->{primaryCovenantLevel} = 'civilization';
      }

      my $primarySpecialties;
      push @$primarySpecialties,
          $opt->{primarySpecialty1}
        ? length($opt->{primarySpecialty1})
          ? $opt->{primarySpecialty1}
          : 'gold'
        : 'gold';
      push @$primarySpecialties,
          $opt->{primarySpecialty2}
        ? length($opt->{primarySpecialty2})
          ? $opt->{primarySpecialty2}
          : 'gold'
        : 'gold';
      push @$primarySpecialties,
          $opt->{primarySpecialty3}
        ? length($opt->{primarySpecialty3})
          ? $opt->{primarySpecialty3}
          : 'gold'
        : 'gold';
      push @$primarySpecialties,
          $opt->{primarySpecialty4}
        ? length($opt->{primarySpecialty4})
          ? $opt->{primarySpecialty4}
          : 'gold'
        : 'gold';
      unless (scalar(@$primarySpecialties) >= 3) {
        # there are 3 or 4 specialties
        $self->worker_croak(
          sprintf('insufficient specialties identified: %s',
            Data::Printer::np($primarySpecialties, multiline => 0))
        );
        return;
      }

      @$primarySpecialties =
        $validator->normalizeSpecialtyLevels($primarySpecialties->@*);

      unless ($opt->{secondaryCovenantLevel}
        && length($opt->{secondaryCovenantLevel})) {
        $logger->warn(
          'convenant level not defined, using default "civilization"');
        $opt->{secondaryCovenantLevel} = 'civilization';
      }
      unless ($validator->checkCovenantLevel($opt->{secondaryCovenantLevel})) {
        $logger->warn(
          sprintf('Invalid covenantLevel using default "civilization"'));
        $logger->warn('invalid value was ' . $opt->{secondaryCovenantLevel});
        $opt->{secondaryCovenantLevel} = 'civilization';
      }

      my $secondarySpecialties;
      push @$secondarySpecialties,
          $opt->{secondarySpecialty1}
        ? length($opt->{secondarySpecialty1})
          ? $opt->{secondarySpecialty1}
          : 'gold'
        : 'gold';
      push @$secondarySpecialties,
          $opt->{secondarySpecialty2}
        ? length($opt->{secondarySpecialty2})
          ? $opt->{secondarySpecialty2}
          : 'gold'
        : 'gold';
      push @$secondarySpecialties,
          $opt->{secondarySpecialty3}
        ? length($opt->{secondarySpecialty3})
          ? $opt->{secondarySpecialty3}
          : 'gold'
        : 'gold';
      push @$secondarySpecialties,
          $opt->{secondarySpecialty4}
        ? length($opt->{secondarySpecialty4})
          ? $opt->{secondarySpecialty4}
          : 'gold'
        : 'gold';
      unless (scalar(@$secondarySpecialties) >= 3) {
        # there are 3 or 4 specialties
        $self->worker_croak(
          sprintf('insufficient specialties identified: %s',
            Data::Printer::np($secondarySpecialties, multiline => 0))
        );
        return;
      }

      @$secondarySpecialties =
        $validator->normalizeSpecialtyLevels($secondarySpecialties->@*);

      return {
        targetType             => $opt->{targetType},
        ascendingLevel         => $opt->{ascendingLevel},
        primaryCovenantLevel   => $opt->{primaryCovenantLevel},
        primarySpecialties     => $primarySpecialties,
        secondaryCovenantLevel => $opt->{secondaryCovenantLevel},
        secondarySpecialties   => $secondarySpecialties,
      };
    }

    method execute_pair ($opt) {

      $params    = $self->validatePairParams($opt);
      $covenant1 = $self->load_covenant($general1);
      if (defined($covenant1)) {
        $logger->debug(
          'covenant1 is ' . Data::Printer::np($covenant1, multiline => 0));
        unless (Scalar::Util::reftype($covenant1)) {
          $self->worker_croak(
            'invalid covenant object: ' . Scalar::Util::reftype($covenant1));
          return;
        }
        unless (Scalar::Util::blessed($covenant1) eq
          'Game::EvonyTKR::Model::Covenant') {
          $self->worker_croak(
            'invalid covenant object: ' . Scalar::Util::blessed($covenant1));
          return;
        }
      }
      $self->load_specialities($general1);
      $self->load_builtin_book($general1);
      $ascendingAttributes1 = $self->load_ascending_attributes($general1);

      $covenant2 = $self->load_covenant($general2);
      if (defined($covenant2)) {
        $logger->debug(
          'covenant2 is ' . Data::Printer::np($covenant2, multiline => 0));
        unless (Scalar::Util::reftype($covenant2)) {
          $self->worker_croak(
            'invalid covenant object: ' . Scalar::Util::reftype($covenant2));
          return;
        }
        unless (Scalar::Util::blessed($covenant2) eq
          'Game::EvonyTKR::Model::Covenant') {
          $self->worker_croak(
            'invalid covenant object: ' . Scalar::Util::blessed($covenant2));
          return;
        }
      }
      $self->load_specialities($general2);
      $self->load_builtin_book($general2);

      my $bestBooks =
        $self->load_best_skill_books($general1, $params->{targetType},
        $opt->{activationType});

      my $bsum1 = Game::EvonyTKR::Model::Buff::Summarizer->new(
        general             => $general1,
        books               => $bestBooks,
        covenant            => $covenant1,
        ascendingAttributes => $ascendingAttributes1,
        isPrimary           => 1,
        targetType          => $params->{targetType},
        activationType      => $opt->{activationType},
        ascendingLevel      => $params->{ascendingLevel},
        covenantLevel       => $params->{primaryCovenantLevel},
        specialty1          => $params->{primarySpecialties}->[0],
        specialty2          => $params->{primarySpecialties}->[1],
        specialty3          => $params->{primarySpecialties}->[2],
        specialty4          => $params->{primarySpecialties}->[3],
      );
      # Do all the heavy computation here
      $bsum1->updateBuffs();
      $bsum1->updateDebuffs();

      my $bsum2 = Game::EvonyTKR::Model::Buff::Summarizer->new(
        general             => $general2,
        books               => $bestBooks,
        covenant            => $covenant2,
        ascendingAttributes => undef,
        isPrimary           => 0,
        targetType          => $params->{targetType},
        activationType      => $opt->{activationType},
        ascendingLevel      => $params->{ascendingLevel},
        covenantLevel       => $params->{secondaryCovenantLevel},
        specialty1          => $params->{secondarySpecialties}->[0],
        specialty2          => $params->{secondarySpecialties}->[1],
        specialty3          => $params->{secondarySpecialties}->[2],
        specialty4          => $params->{secondarySpecialties}->[3],
      );
      $bsum2->updateBuffs();
      $bsum2->updateDebuffs();

      my $bk = $params->{targetType} =~ s/_/ /r;
      $bk =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
      $bk =~ s/Siege Troops/Siege Machines/;
      $logger->debug("buffKey is " . Data::Printer::np($bk, multiline => 0));

      my $tsum = {};

      foreach my $type (keys %{ $bsum1->buffValues->{$bk} }) {
        my $t =
          $bsum1->buffValues->{$bk}->{$type} +
          $bsum2->buffValues->{$bk}->{$type};
        $logger->debug(sprintf(
          'for type %s, bsum1 is %s, bsum2 is %s total is %s',
          $type,
          $bsum1->buffValues->{$bk}->{$type},
          $bsum2->buffValues->{$bk}->{$type}, $t
        ));
        $tsum->{buffValues}->{$type} = $t;
      }

      foreach my $category (keys %{ $bsum1->debuffValues }) {
        $self->logger->debug("calc debuffs for $category");
        foreach my $type (keys %{ $bsum1->debuffValues->{$category} }) {
          $tsum->{debuffValues}->{$category}->{$type} =
            $bsum1->debuffValues->{$category}->{$type} +
            $bsum2->debuffValues->{$category}->{$type};
          $self->logger->debug("calc debuffs for $category -> $type: "
              . $tsum->{debuffValues}->{$category}->{$type});
        }
      }

      my $row = {
        primary            => $general1->to_hash,
        secondary          => $general2->to_hash,
        attackbuff         => $tsum->{buffValues}->{'Attack'},
        defensebuff        => $tsum->{buffValues}->{'Defense'},
        hpbuff             => $tsum->{buffValues}->{'HP'},
        marchbuff          => $tsum->{buffValues}->{'March Size'},
        groundattackdebuff =>
          $tsum->{debuffValues}->{'Ground Troops'}{'Attack'},
        grounddefensedebuff =>
          $tsum->{debuffValues}->{'Ground Troops'}{'Defense'},
        groundhpdebuff      => $tsum->{debuffValues}->{'Ground Troops'}{'HP'},
        mountedattackdebuff =>
          $tsum->{debuffValues}->{'Mounted Troops'}{'Attack'},
        mounteddefensedebuff =>
          $tsum->{debuffValues}->{'Mounted Troops'}{'Defense'},
        mountedhpdebuff    => $tsum->{debuffValues}->{'Mounted Troops'}{'HP'},
        rangedattackdebuff =>
          $tsum->{debuffValues}->{'Ranged Troops'}{'Attack'},
        rangeddefensedebuff =>
          $tsum->{debuffValues}->{'Ranged Troops'}{'Defense'},
        rangedhpdebuff    => $tsum->{debuffValues}->{'Ranged Troops'}{'HP'},
        siegeattackdebuff =>
          $tsum->{debuffValues}->{'Siege Machines'}{'Attack'},
        siegedefensedebuff =>
          $tsum->{debuffValues}->{'Siege Machines'}{'Defense'},
        siegehpdebuff => $tsum->{debuffValues}->{'Siege Machines'}{'HP'},
      };
      my $json =
        JSON::PP->new->utf8(0)->allow_blessed->convert_blessed->canonical;

      my $payload = $json->encode({ runId => 0+ $opt->{runId}, data => $row });

      $logger->debug(sprintf(
        'row is %s, payload is %s',
        Data::Printer::np($row, multiline => 0), $payload
      ));

      my $result = MIME::Base64::encode_base64($payload, '');
      if ($debug_enabled) {
        $self->logger->debug( "payload is $payload");
      }
      $self->logger->info($result);
      return $result;
    }

    method validateSingleParams($opt) {

      unless ($opt->{runId} && length($opt->{runId})) {
        $self->worker_croak('runId is required');
      }

      if ($opt->{general1} && length($opt->{general1})) {
        $general1 = $self->load_general($opt->{general1});
        unless ($general1) {
          $self->worker_croak(
            sprintf('general1 %s is not available', $opt->{general1}));
        }
      }
      else {
        $self->worker_croak('general1 was not specified.');
      }

      unless ($opt->{targetType} && length($opt->{targetType})) {
        my $type =
          ref $general1->type eq 'ARRAY'
          ? $general1->type->[0]
          : $general1->type;
        $logger->warn('targetType was not set!!'
            . 'falling back to the first type from general1'
            . $type);
        $opt->{targetType} = $type;
      }
      $logger->info('targetType is ' . $opt->{targetType});

      if ($opt->{isPrimary}) {
        # Validate ascending level
        unless (
          defined($opt->{ascendingLevel} && length($opt->{ascendingLevel}))) {
          $logger->warn('undefined ascendingLevel, using default "red5"');
          $opt->{ascendingLevel} = 'red5';
        }
        unless ($validator->checkAscendingLevel($opt->{ascendingLevel})) {
          $logger->warn(sprintf(
            'Invalid ascendingLevel: "%s" , using default "red5"',
            $opt->{ascendingLevel}));
          $opt->{ascendingLevel} = 'red5';
        }
      }
      else {
        $opt->{ascendingLevel} = 'none';
      }

      unless ($opt->{primaryCovenantLevel}
        && length($opt->{primaryCovenantLevel})) {
        $logger->warn(
          'convenant level not defined, using default "civilization"');
        $opt->{primaryCovenantLevel} = 'civilization';
      }
      unless ($validator->checkCovenantLevel($opt->{primaryCovenantLevel})) {
        $logger->warn(
          sprintf('Invalid covenantLevel using default "civilization"'));
        $logger->warn('invalid value was ' . $opt->{primaryCovenantLevel});
        $opt->{primaryCovenantLevel} = 'civilization';
      }

      my $primarySpecialties;
      push @$primarySpecialties,
          $opt->{primarySpecialty1}
        ? length($opt->{primarySpecialty1})
          ? $opt->{primarySpecialty1}
          : 'gold'
        : 'gold';
      push @$primarySpecialties,
          $opt->{primarySpecialty2}
        ? length($opt->{primarySpecialty2})
          ? $opt->{primarySpecialty2}
          : 'gold'
        : 'gold';
      push @$primarySpecialties,
          $opt->{primarySpecialty3}
        ? length($opt->{primarySpecialty3})
          ? $opt->{primarySpecialty3}
          : 'gold'
        : 'gold';
      push @$primarySpecialties,
          $opt->{primarySpecialty4}
        ? length($opt->{primarySpecialty4})
          ? $opt->{primarySpecialty4}
          : 'gold'
        : 'gold';
      unless (scalar(@$primarySpecialties) >= 3) {
        # there are 3 or 4 specialties
        $self->worker_croak(
          sprintf('insufficient specialties identified: %s',
            Data::Printer::np($primarySpecialties, multiline => 0))
        );
        return;
      }

      @$primarySpecialties =
        $validator->normalizeSpecialtyLevels($primarySpecialties->@*);

      return {
        targetType           => $opt->{targetType},
        ascendingLevel       => $opt->{ascendingLevel},
        primaryCovenantLevel => $opt->{primaryCovenantLevel},
        primarySpecialties   => $primarySpecialties,
      };
    }

    method execute_single ($opt) {
      $params    = $self->validateSingleParams($opt);
      $covenant1 = $self->load_covenant($general1);
      if (defined($covenant1)) {
        $logger->debug(
          'covenant1 is ' . Data::Printer::np($covenant1, multiline => 0));
        unless (Scalar::Util::reftype($covenant1)) {
          $self->worker_croak(
            'invalid covenant object: ' . Scalar::Util::reftype($covenant1));
          return;
        }
        unless (Scalar::Util::blessed($covenant1) eq
          'Game::EvonyTKR::Model::Covenant') {
          $self->worker_croak(
            'invalid covenant object: ' . Scalar::Util::blessed($covenant1));
          return;
        }
      }
      $self->load_specialities($general1);
      $self->load_builtin_book($general1);
      $ascendingAttributes1 = $self->load_ascending_attributes($general1);

      my $bestBooks =
        $self->load_best_skill_books($general1, $params->{targetType},
        $opt->{activationType});

      my $bsum = Game::EvonyTKR::Model::Buff::Summarizer->new(
        general             => $general1,
        books               => $bestBooks,
        covenant            => $covenant1,
        ascendingAttributes => $ascendingAttributes1,
        isPrimary           => $opt->{isPrimary},
        targetType          => $params->{targetType},
        activationType      => $opt->{activationType},
        ascendingLevel      => $params->{ascendingLevel},
        covenantLevel       => $params->{primaryCovenantLevel},
        specialty1          => $params->{primarySpecialties}->[0],
        specialty2          => $params->{primarySpecialties}->[1],
        specialty3          => $params->{primarySpecialties}->[2],
        specialty4          => $params->{primarySpecialties}->[3],
      );
      # Do all the heavy computation here
      $bsum->updateBuffs();
      $bsum->updateDebuffs();

      my $bk = $params->{targetType} =~ s/_/ /r;
      $bk =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
      $bk =~ s/Siege Troops/Siege Machines/;
      $logger->debug("buffKey is " . Data::Printer::np($bk, multiline => 0));

      my $row = {
        primary             => $general1->to_hash,
        attackbuff          => $bsum->buffValues->{$bk}{'Attack'},
        defensebuff         => $bsum->buffValues->{$bk}{'Defense'},
        hpbuff              => $bsum->buffValues->{$bk}{'HP'},
        marchbuff           => $bsum->buffValues->{$bk}{'March Size'},
        groundattackdebuff  => $bsum->debuffValues->{'Ground Troops'}{'Attack'},
        grounddefensedebuff =>
          $bsum->debuffValues->{'Ground Troops'}{'Defense'},
        groundhpdebuff      => $bsum->debuffValues->{'Ground Troops'}{'HP'},
        mountedattackdebuff =>
          $bsum->debuffValues->{'Mounted Troops'}{'Attack'},
        mounteddefensedebuff =>
          $bsum->debuffValues->{'Mounted Troops'}{'Defense'},
        mountedhpdebuff     => $bsum->debuffValues->{'Mounted Troops'}{'HP'},
        rangedattackdebuff  => $bsum->debuffValues->{'Ranged Troops'}{'Attack'},
        rangeddefensedebuff =>
          $bsum->debuffValues->{'Ranged Troops'}{'Defense'},
        rangedhpdebuff     => $bsum->debuffValues->{'Ranged Troops'}{'HP'},
        siegeattackdebuff  => $bsum->debuffValues->{'Siege Machines'}{'Attack'},
        siegedefensedebuff =>
          $bsum->debuffValues->{'Siege Machines'}{'Defense'},
        siegehpdebuff => $bsum->debuffValues->{'Siege Machines'}{'HP'},
      };
      my $json =
        JSON::PP->new->utf8(0)->allow_blessed->convert_blessed->canonical;

      my $payload = $json->encode({ runId => 0+ $opt->{runId}, data => $row });

      $logger->debug(sprintf(
        'row is %s, payload is %s',
        Data::Printer::np($row, multiline => 0), $payload
      ));

      my $result = MIME::Base64::encode_base64($payload);
      if ($debug_enabled) {
        $self->logger->debug( "payload is $payload");
      }
      $self->logger->info($result);
      return $result;
    }

    method load_general ($name) {
      $logger->debug('collection_dir is ' . $collection_dir->realpath());
      my $generals_dir = $collection_dir->child('generals');
      my $normalized_name = $self->normalize_name($name);
      my ($general_file) = grep {
          my $normalized_file = $self->normalize_name($_->basename('.yaml'));
          $normalized_file eq $normalized_name;
      } $generals_dir->children;

      unless (defined($general_file) && $general_file->is_file()) {
        $logger->debug("+++Missing General: '$name' ;; '$normalized_name'");
        $logger->debug('+++Raw general name: ' . unpack("H*", encode_utf8($name)));
        $logger->debug("+++After normalize_quotes: " . unpack("H*", encode_utf8($normalized_name)));
        $logger->debug('+++Available files: ');
        foreach my $child ($generals_dir->children){
          my $cn = decode_utf8($child->basename =~ s/\.yaml$//r);
          $logger->debug("+++'$cn' (bytes: " . unpack("H*", encode_utf8($cn)) . ")");
        }

        $self->worker_croak(
          "No YAML file present for $name in $generals_dir");
        return;
      }
      my $data    = $general_file->slurp_utf8;
      my $object  = $ypp->load_string($data);
      my $general = Game::EvonyTKR::Model::General->from_hash($object, $logger);
      $logger->debug(sprintf('general "%s" has been created', $general->name));

      return $general;
    }

    method load_covenant ($primary) {
      my $name            = $primary->name;
      my $covenants_dir   = $collection_dir->child('covenants');
      # Normalize both the search name and filenames
      my $normalized_name = $self->normalize_name($name);

      my ($covenant_file) = grep {
        my $normalized_file = $self->normalize_name($_->basename('.yaml'));
          $normalized_file eq $normalized_name;
      } $covenants_dir->children;

      if (defined($covenant_file) && $covenant_file->is_file()) {
        my $data   = $covenant_file->slurp_utf8;
        my $object = $ypp->load_string($data);
        my $c =
          Game::EvonyTKR::Model::Covenant->from_hash($object, $primary,
          $logger);
        return $c;
      } else {
        # not all generals *do* have convenants, so this is *probably* expected
        $logger->info("No covenant available for $name.");
      }
      return undef;
    }

    method load_specialities ($general) {
      my $specialty_dir = $collection_dir->child('specialties');
      foreach my $specialty_name ($general->specialtyNames->@*) {

        my $normalized_name = $self->normalize_name($specialty_name);
        my ($specialty_file) = grep {
            my $normalized_file = $self->normalize_name($_->basename('.yaml'));
            $normalized_file eq $normalized_name;
        } $specialty_dir->children;

        if (defined($specialty_file) && $specialty_file->is_file()) {
          my $data   = $specialty_file->slurp_utf8;
          my $object = $ypp->load_string($data);
          my $s = Game::EvonyTKR::Model::Specialty->from_hash($object, $logger);
          push @{ $general->specialties }, $s;
        }
        else {
          # specialties *should* exist.
          $logger->debug("----Available specialty files:");
          for my $file ($specialty_dir->children) {
            my $name = decode_utf8($file->basename =~ s/\.yaml$//r);
            $logger->debug("----'$name' (bytes: " . unpack("H*", encode_utf8($name)) . ")");
          }
          $self->worker_croak(sprintf(
            'specialty "%s" for general "%s" not found.',
            $specialty_name, $general->name
          ));
        }
      }
    }

    method load_builtin_book ($general) {
      my $book_dir    = $collection_dir->child('skill books');
      my $book_name   = $general->builtInBookName;
      my $normalized_name = $self->normalize_name($book_name);
      my ($book_file) = grep {
          my $normalized_file = $self->normalize_name($_->basename('.yaml'));
          $normalized_file eq $normalized_name;
      } $book_dir->children;

      if (defined($book_file) && $book_file->is_file()) {
        $logger->debug(
          sprintf('found book file %s for book %s', $book_file, $book_name));
        my $data   = $book_file->slurp_utf8;
        my $object = $ypp->load_string($data);
        my $book =
          Game::EvonyTKR::Model::Book::Builtin->from_hash($object, $logger);
        unless (Scalar::Util::blessed($book) eq
          'Game::EvonyTKR::Model::Book::Builtin') {
          $self->worker_croak('failed to import book ' . $book_name);
          return;
        }
        $logger->debug('imported book "%s"', $book->name);
        $general->set_builtInBook($book);

      }
    }

    method load_ascending_attributes ($general) {
      unless ($general->ascending) {
        $logger->info(
          sprintf('general "%s" cannot be ascended.', $general->name));
        return;
      }
      my $ascending_attr_dir = $collection_dir->child('ascending attributes');
      my $name               = $general->name;
      my $normalized_name = $self->normalize_name($name);

      my ($aa_file) = grep {
          my $normalized_file = $self->normalize_name($_->basename('.yaml'));
          $normalized_file eq $normalized_name;
      } $ascending_attr_dir->children;

      if (defined($aa_file) && $aa_file->is_file()) {
        my $data   = $aa_file->slurp_utf8;
        my $object = $ypp->load_string($data);
        my $aa =
          Game::EvonyTKR::Model::AscendingAttributes->from_hash($object,
          $logger);
      }
      else {
        $self->worker_croak(sprintf(
          'general "%s" with ascending set "%s" '
            . 'has no ascending attribute file.',
          $general->name, $general->ascending ? 'true' : 'false'
        ));
        return;
      }
    }

    method load_best_skill_books($general, $targetType, $activationType) {
      my $key = $activationType eq 'PvM' ? 'PvM' : 'default';

      my @books;
      my $generic_dir       = $collection_dir->child('generic books');
      my @sorted_book_names = sort {
        $BestSkillBooks->{$targetType}->{$key}->{$a}
          <=> $BestSkillBooks->{$targetType}->{$key}->{$b}
      } keys %{ $BestSkillBooks->{$targetType}->{$key} };

      foreach my $book_name (@sorted_book_names) {
        my ($book_file) = $generic_dir->children(qr/\Q$book_name\E\.yaml/i);
        if (defined($book_file) && $book_file->is_file()) {
          my $data   = $book_file->slurp_utf8;
          my $object = $ypp->load_string($data);
          my $book =
            Game::EvonyTKR::Model::Book::SkillBook->from_hash($object, $logger);

          $logger->info(sprintf(
            'picked book %s for general %s',
            $book->name, $general->name
          ));
          push @books, $book;
          last if (scalar @books >= 10);

        }
        else {
          $logger->warn("cannot find file for $book_name");
          next;
        }
      }
      # certain books need to be there or the buff summarizer will
      # spew errors.
      foreach my $attr ('Attack', 'Defense', 'HP') {
        foreach my $tt ('Mounted Troop', 'Ranged Troop', 'Ground Troop',
          'Siege Machine') {
          unless (List::AllUtils::any { $_->name =~ /$tt $attr/ } @books) {
            my ($book_file) =
              $generic_dir->children(qr/\QLevel 4 $tt $attr\E\.yaml$/i);
            if ($book_file && $book_file->is_file()) {
              my $data   = $book_file->slurp_utf8;
              my $object = $ypp->load_string($data);
              my $book =
                Game::EvonyTKR::Model::Book::SkillBook->from_hash($object,
                $logger);

              $logger->debug(sprintf(
                'picked book %s for general %s',
                $book->name, $general->name
              ));
              push @books, $book;
            }
          }
        }
      }
      return \@books;
    }

    method normalize_name ($name) {
      my $dn = is_utf8($name) ? $name : decode_utf8($name);
      my $nn = fc(NFKD($dn));
      $nn =~ s/[’''‛`´]/'/g;
      $nn =~ s/[""‟]/"/g;    # Quotes
      return $nn;
    }
  }
}
1;

__END__

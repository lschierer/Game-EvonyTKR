package Game::EvonyTKR::External::General::ComputeBuffCache;
use v5.42.0;
use utf8::all;
use Mojo::Base 'Game::EvonyTKR::External::General::Summarizer', -signatures;

BEGIN {
  # tell the parent it is a stub.
  Game::EvonyTKR::External::General::Summarizer::parentIsStub();
}

sub task_name {'compute_general_buff_cache'}

sub register ($taskClass, $app, $conf = {}) {
  $taskClass->SUPER::register($app, $conf);
  $app->minion->add_task($taskClass->task_name => __PACKAGE__);
  return 1;
}

sub run ($job, @args) {
  # Wait for covenants to be loaded before computing buffs
  return if $job->are_prereqs_outstanding($job->minion, ['load_all_covenants']);

  $job->log_info(
    sprintf('Computing buff cache for general: "%s"', $job->generalName));

  $job->SUPER::run(@args);

  unless ($job->general) {
    return $job->fail(
      sprintf('cannot retrieve general for "%s"', $job->generalName));
  }
  $job->note(general => $job->general->name);

  # Define all possible configurations to cache

  $job->general->populateAscendingAttributes;
  $job->general->populateBuiltinBook;
  $job->general->populateSpecialties;
  my @ascending_levels;
  if ($job->general->ascending) {
    my $isRed = $job->general->stars =~ /red/i;
    push @ascending_levels, $job->AscendingAttributeLevelValues($isRed);
  }
  else {
    push @ascending_levels, 'none';
  }

  my $cached_count = 0;

  foreach my $target_type ($job->GeneralKeys->@*) {
    $job->log_debug(sprintf(
      'computing buffs for "%s" target_type "%s"',
      $job->general->name, $target_type
    ));
    $job->targetType($target_type);
    $job->params->{targetType} = $target_type;
    foreach my $activation_type ($job->AllowedBuffActivationValues->@*) {
      $job->log_debug(sprintf(
        'computing buffs for "%s" activation_type "%s"',
        $job->general->name, $activation_type
      ));

      $job->activationType($activation_type);
      $job->params->{activationType} = $activation_type;

      foreach my $specialty_index (1 .. 4) {
        foreach my $specialty_level ($job->SpecialtyLevelValues->@*) {
          $job->log_debug(sprintf(
            'computing buffs for "%s" specialty_index %s specialty_level "%s"',
            $job->general->name, $specialty_index, $specialty_level
          ));
          # Special logic for 4th specialty
          if ($specialty_index == 4) {
            my $first_three_gold =
              (    $job->specialty1 eq 'gold'
                && $job->specialty2 eq 'gold'
                && $job->specialty3 eq 'gold');

            if ($first_three_gold) {
              # 4th specialty cannot be 'none' when first 3 are gold
              next if $specialty_level eq 'none';
            }
            else {
              # 4th specialty must be 'none' unless first 3 are gold
              next unless $specialty_level eq 'none';
            }
          }

          my $specialty_accessor = "specialty${specialty_index}";
          $job->$specialty_accessor($specialty_level);
          $job->params->{$specialty_accessor} = $specialty_level;

          foreach my $covenant_level ($job->CovenantCategoryValues->@*) {
            $job->log_debug(sprintf(
              'computing buffs for "%s" covenant_level "%s"',
              $job->general->name, $covenant_level
            ));
            $job->covenantLevel($covenant_level);
            $job->params->{covenantLevel} = $covenant_level;

            foreach my $ascending_level (@ascending_levels) {
              $job->log_debug(sprintf(
                'computing buffs for "%s" ascending_level "%s"',
                $job->general->name, $ascending_level
              ));
              if ($job->isPrimary && $job->general->ascending) {
                $job->ascendingLevel($ascending_level);
                $job->params->{ascendingLevel} = $ascending_level;
              }
              else {
                $job->ascendingLevel('none');
                $job->params->{ascendingLevel} = 'none';
              }
              my $tt = $job->targetType     // $job->general->type->[0];
              my $at = $job->activationType // 'default';
              $job->books([
                $job->load_best_skill_books($job->general, $tt, $at)->@*,
                $job->load_mandatory_skill_books()->@*,
              ]);

              $job->log_debug(
                sprintf(
                  'computing buffs for "%s" with params %s',
                  $job->general->name,
                  Data::Printer::np($job->params, multiline => 0)
                )
              );
              $job->summarize();

              # Store in persistence with predictable key
              my $cache_key   = $job->_generate_cache_key();
              my $buff_values = $job->summarizer->buffValues;
              $job->log_debug(sprintf(
                'found buffs for "%s" with cache key "%s": %s',
                $job->general->name, $cache_key,
                Data::Printer::np($buff_values)
              ));
              # Convert to plain hash to avoid reference issues

              $job->persistence->store_data('general_buff_cache', $cache_key,
                $buff_values);
              $cached_count++;
            }
          }
        }
      }
    }
  }

  my $message = sprintf('Cached %s buff configurations for "%s"',
    $cached_count, $job->generalName);
  $job->log_info($message);
  return $job->finish($message);
}

sub _generate_cache_key($job) {
  return sprintf(
    '%s:%s:%s:%s:%s:%s:%s:%s:%s:%s',
    $job->generalName,    $job->isPrimary,      $job->targetType,
    $job->activationType, $job->ascendingLevel, $job->covenantLevel,
    $job->specialty1,     $job->specialty2,     $job->specialty3,
    $job->specialty4
  );
}

1;

__END__

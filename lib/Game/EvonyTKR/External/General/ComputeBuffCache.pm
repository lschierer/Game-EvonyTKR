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
  $app->minion->add_task($taskClass->task_name => __PACKAGE__, { limit => 1 });
  return 1;
}

# Common configurations to cache (reduces from ~50k to ~500 per general)
sub common_activation_types ($job) {
  my $general_types = $job->general->type;

  # Wall generals use Wall and Defense
  if (grep {/wall/i} @$general_types) {
    return ['Defense', 'Wall'];
  }
  elsif (grep {/mayor/i} @$general_types) {
    return ['Defense', 'Mayor'];
  }elsif(grep { /(?:mounted|ranged)/i }@$general_types ){
    return ['Reinforcing','Attacking', 'PvM'];
  }elsif(grep { /ground/i }@$general_types ){
    return ['Reinforcing','Attacking', 'PvM'];
  }elsif(grep { /siege/i }@$general_types ){
    return ['Wall','Attacking','Defense'];
  }

  # Most generals use Attacking and PvM (Monster hunting)
  return ['Attacking', 'PvM'];
}

sub common_specialty_configs ($job) {
  # Returns arrayref of [s1, s2, s3, s4] configurations
  return [
    ['none',   'none',   'none',   'none'],    # No specialties
    ['purple', 'purple', 'purple', 'none'],    # 3 purple
    ['orange', 'orange', 'orange', 'none'],    # 3 orange
    ['gold',   'gold',   'gold',   'gold'],    # 4 gold (max)
  ];
}

sub common_ascending_levels ($job, $isRed) {
  if ($isRed) {
    return ['none', 'red3', 'red4', 'red5'];
  }
  else {
    return ['none', 'purple3', 'purple4', 'purple5'];
  }
  return;
}

sub common_covenant_levels ($job) {
  # Skip 'peace' as it's less common in practice
  return ['none', 'cooperation', 'faith', 'honor', 'civilization'];
}

sub run ($job, @args) {
  # Wait for covenants to be loaded before computing buffs
  return if $job->are_prereqs_outstanding($job->minion, ['load_all_covenants']);
  my $extraDelay = $job->info->{notes}->{delay} // 0;
  return $job->retry({ delay => $job->standard_delay + $extraDelay })
    unless my $guard =
    $job->minion->guard($job->task_name, 600, { limit => 1 });

  $job->SUPER::run(@args);

  $job->log_info(
    sprintf('Computing buff cache for general: "%s"', $job->generalName));

  unless ($job->general) {
    return $job->fail(
      sprintf('cannot retrieve general for "%s"', $job->generalName));
  }
  $job->note(general => $job->general->name);

  # Define common configurations to cache (not all possible permutations)

  $job->general->populateAscendingAttributes;
  $job->general->populateBuiltinBook;
  $job->general->populateSpecialties;

  my $isRed = $job->general->stars =~ /red/i;
  my $ascending_levels =
      $job->general->ascending
    ? $job->common_ascending_levels($isRed)
    : ['none'];

  my $cached_count = 0;

  foreach my $target_type ($job->general->type->@*) {
    unless(length($target_type)){
      $job->log_error(sprintf('general "%s" has invalid target type with no length.', $job->general->name));
      next;
    }
    $job->log_debug(sprintf(
      'computing buffs for "%s" target_type "%s"',
      $job->general->name, $target_type
    ));

    my $tt;
    if($target_type =~ /wall/i or $target_type =~ /mayor/i){
      $tt = $target_type;
    }else {
      $tt = $job->string_to_trooptype($target_type);
    }

    unless(length($tt)){
      $job->log_error(sprintf(
        'general type to troop type conversion failed for general "%s" with type "%s"',
        $job->general->name, $target_type));
      next;
    }
    $job->targetType($tt);
    $job->params->{targetType} = $tt;

    foreach my $activation_type ($job->common_activation_types->@*) {
      $job->log_debug(sprintf(
        'computing buffs for "%s" activation_type "%s"',
        $job->general->name, $activation_type
      ));

      $job->activationType($activation_type);
      $job->params->{activationType} = $activation_type;

      # Iterate through common specialty configurations
      foreach my $specialty_config ($job->common_specialty_configs->@*) {
        my ($s1, $s2, $s3, $s4) = @$specialty_config;

        $job->specialty1($s1);
        $job->specialty2($s2);
        $job->specialty3($s3);
        $job->specialty4($s4);
        $job->params->{specialty1} = $s1;
        $job->params->{specialty2} = $s2;
        $job->params->{specialty3} = $s3;
        $job->params->{specialty4} = $s4;

        $job->log_debug(sprintf(
          'computing buffs for "%s" with specialties %s/%s/%s/%s',
          $job->general->name, $s1, $s2, $s3, $s4
        ));

        foreach my $covenant_level ($job->common_covenant_levels->@*) {
          $job->log_debug(sprintf(
            'computing buffs for "%s" covenant_level "%s"',
            $job->general->name, $covenant_level
          ));
          $job->covenantLevel($covenant_level);
          $job->params->{covenantLevel} = $covenant_level;

          foreach my $ascending_level (@$ascending_levels) {
            $job->log_debug(sprintf(
              'computing buffs for "%s" ascending_level "%s"',
              $job->general->name, $ascending_level
            ));
            if ($job->general->ascending) {
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

            $job->log_debug(sprintf(
              'computing buffs for "%s" with params %s',
              $job->general->name,
              Data::Printer::np($job->params, multiline => 0)
            ));

            # as primary
            $job->isPrimary(1);
            $job->summarize();
            # Store in persistence with predictable key
            my $cache_key = $job->generate_buff_cache_key(
              $job->generalName,    $job->isPrimary,
              $job->targetType,     $job->activationType,
              $job->ascendingLevel, $job->covenantLevel,
              $job->specialty1,     $job->specialty2,
              $job->specialty3,     $job->specialty4
            );
            my $buff_values = $job->summarizer->buffValues;
            $job->log_debug(sprintf(
              'found buffs for "%s" with cache key "%s": %s',
              $job->general->name, $cache_key,
              Data::Printer::np($buff_values)
            ));
            $job->store_buff_cache($cache_key, $buff_values);
            $cached_count++;
            $job->note(cached_count             => $cached_count);
            $job->note(last_primary_buff_values => $buff_values);

            # as secondary
            $job->isPrimary(0);
            $job->summarize();
            # Store in persistence with predictable key
            $cache_key = $job->generate_buff_cache_key(
              $job->generalName,    $job->isPrimary,
              $job->targetType,     $job->activationType,
              $job->ascendingLevel, $job->covenantLevel,
              $job->specialty1,     $job->specialty2,
              $job->specialty3,     $job->specialty4
            );
            $buff_values = $job->summarizer->buffValues;
            $job->log_debug(sprintf(
              'found buffs for "%s" with cache key "%s": %s',
              $job->general->name, $cache_key,
              Data::Printer::np($buff_values)
            ));
            $job->store_buff_cache($cache_key, $buff_values);
            $cached_count++;
            $job->note(cached_count => $cached_count);
          }    # end ascending_level loop
        }    # end covenant_level loop
      }    # end specialty_config loop
    }    # end activation_type loop
  }    # end target_type loop

  my $message = sprintf('Cached %s buff configurations for "%s"',
    $cached_count, $job->generalName);
  $job->log_info($message);
  return $job->finish($message);
}

1;

__END__

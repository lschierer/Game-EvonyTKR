use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::BuildIndexes {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;

  sub task_name {'build_general_indexes'}

  sub register ($taskClass, $app, $conf = {}) {
    return 1 unless $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    return 1;
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }
    $job->log_debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    # Wait for all general loading jobs to complete
    return
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_generals',    'load_all_builtin_books',
        'load_all_specialties', 'load_all_ascending_attributes',
      ]
      ));

    $job->log_info('Starting build_general_indexes job');

    my $app           = $job->app;
    my $general_names = $job->list_generals();

    unless ($general_names && @$general_names) {
      my $errmessage = 'No generals found to index';
      $job->log_error($errmessage);
      return $job->fail($errmessage);
    }

    $job->log_info(
      sprintf('Building indexes for %d generals', scalar @$general_names));

    # Build type -> [keys] mapping
    my %by_type;
    my $indexed_count = 0;

    for my $name (@$general_names) {
      my $general = $job->get_general($name);
      unless ($general) {
        $job->log_warn(
          sprintf('Could not fetch general "%s" from cache', $name));
        next;
      }

      my $key = lc($job->normalize($name));
      $key =~ s/ /_/g;

      # general->type can be arrayref or scalar
      my @types =
        ref($general->type) eq 'ARRAY'
        ? @{ $general->type }
        : ($general->type);

      for my $type (@types) {
        next unless defined($type) && length($type);
        push @{ $by_type{$type} }, $key;
      }

      $indexed_count++;
    }

    $job->log_info(sprintf(
      'Indexed %d generals into %d type categories',
      $indexed_count, scalar(keys %by_type)
    ));

    # Store each type's list in metadata
    for my $type (keys %by_type) {
      my $type_key  = "general_index:by_type:$type";
      my $keys_list = $by_type{$type};

      eval {
        require Mojo::JSON;
        my $json = Mojo::JSON::encode_json($keys_list);
        $job->persistence->set_metadata($type_key, $json);
        $job->log_debug(sprintf(
          'Stored index for type "%s" with %d generals',
          $type, scalar(@$keys_list)
        ));
      };
      if ($@) {
        $job->log_error(sprintf(
          'Failed to store index for type "%s": %s', $type, $@));
      }
    }

    # Also store the list of available types
    my @type_list = sort keys %by_type;
    eval {
      require Mojo::JSON;
      my $json = Mojo::JSON::encode_json(\@type_list);
      $job->persistence->set_metadata('general_index:available_types', $json);
    };
    if ($@) {
      $job->log_error("Failed to store available_types: $@");
    }

    my $msg = sprintf(
      'build_general_indexes completed: %d generals indexed into %d types (%s)',
      $indexed_count, scalar(@type_list), join(', ', @type_list));
    $job->log_info($msg);
    $job->note(
      indexed_count    => $indexed_count,
      types            => \@type_list,
      generals_by_type => \%by_type,
    );

    # Mark job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);

    return $job->finish($msg);
  }
}

1;
__END__

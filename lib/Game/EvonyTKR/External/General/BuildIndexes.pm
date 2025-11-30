use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::General::BuildIndexes {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase',          -signatures;

  sub task_name {'build_general_indexes'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);
    my $signal = __PACKAGE__ =~ s/::/_/gr;
    $app->plugins->emit($signal => 1);
  }

  sub run ($job, @args) {
    if (not defined($job)) {
      say 'job not defined in run for ' . __PACKAGE__;
      return;
    }
    $job->SUPER::run(@args);
    unless (defined($job->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }
    $job->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($job->logger->level())
    ));

    # Wait for all general loading jobs to complete
    return
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_ascending_attributes', 'load_all_builtin_books',
        'load_all_specialties',          'load_ascending_attributes',
        'load_book',                     'load_specialty',
        'load_all_generals',             'load_general',
      ]
      ));

    $job->logger->info('Starting build_general_indexes job');

    my $app           = $job->app;
    my $general_names = $job->list_generals();

    unless ($general_names && @$general_names) {
      my $errmessage = 'No generals found to index';
      $job->logger->error($errmessage);
      return $job->fail($errmessage);
    }

    $job->logger->info(
      sprintf('Building indexes for %d generals', scalar @$general_names));

    # Build type -> [keys] mapping
    my %by_type;
    my $indexed_count = 0;

    for my $name (@$general_names) {
      my $general = $job->get_general($name);
      unless ($general) {
        $job->logger->warn(
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

    $job->logger->info(sprintf(
      'Indexed %d generals into %d type categories',
      $indexed_count, scalar(keys %by_type)
    ));

    # Store each type's list in cache
    for my $type (keys %by_type) {
      my $type_key  = "by_type:$type";
      my $keys_list = $by_type{$type};

      unless ($job->general_cache->set($type_key, $keys_list)) {
        $job->logger->error(sprintf(
          'Failed to store index for type "%s" with %d generals',
          $type, scalar(@$keys_list)
        ));
      }
      else {
        $job->logger->debug(sprintf(
          'Stored index for type "%s" with %d generals',
          $type, scalar(@$keys_list)
        ));
      }
    }

    # Also store the list of available types
    my @type_list = sort keys %by_type;
    $job->general_cache->set('available_types', \@type_list);

    my $msg = sprintf(
      'build_general_indexes completed: %d generals indexed into %d types (%s)',
      $indexed_count, scalar(@type_list), join(', ', @type_list));
    $job->logger->info($msg);
    $job->note(
      indexed_count    => $indexed_count,
      types            => \@type_list,
      generals_by_type => \%by_type,
    );
    return $job->finish($msg);
  }
}

1;
__END__

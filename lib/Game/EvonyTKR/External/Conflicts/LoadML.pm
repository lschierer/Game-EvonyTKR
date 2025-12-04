use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Conflicts::LoadML {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::JSON qw(decode_json);
  use Mojo::File;
  use List::AllUtils qw(any);

  sub task_name {'load_ml_conflicts'}

  sub register ($taskClass, $app, $conf = {}) {
    $taskClass->SUPER::register($app, $conf);
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $taskClass->logger->error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $taskClass->logger->error($errmessage);
      say $errmessage;
      return;
    }
    $taskClass->logger->debug('Registering ML Conflicts Loader task');
    $app->minion->add_task($taskClass->task_name => __PACKAGE__);

    $taskClass->logger->info(sprintf('emitting signal for %s', __PACKAGE__));
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

    return
      if ($job->are_prereqs_outstanding(
      $job->minion,
      [
        'load_all_generals',    'load_all_builtin_books',
        'load_all_specialties', 'load_all_ascending_attributes',
      ]
      ));

    # Find conflicts.json file
    my $json_path = Mojo::File->new('conflicts.json');
    unless (-f $json_path) {
      my $errmsg = "conflicts.json not found in current directory";
      $job->logger->error($errmsg);
      return $job->fail($errmsg);
    }

    $job->logger->info('Loading ML conflict predictions from conflicts.json');

    # Load JSON
    my $json_text = $json_path->slurp;
    my $raw_data  = decode_json($json_text);

    # Get all generals for type checking (normalize names to match JSON keys)
    my %generals = map { $job->normalize($_->name) => $_ } $job->get_generals()->@* ;

    # Filter out cross-type pairs
    my %filtered_conflicts;
    my $total_pairs    = 0;
    my $filtered_pairs = 0;
    my $kept_pairs     = 0;

    for my $g1_name (keys %$raw_data) {
      my $g1 = $generals{$g1_name};
      next unless $g1;

      for my $g2_name (keys %{ $raw_data->{$g1_name} }) {
        my $g2 = $generals{$g2_name};
        next unless $g2;

        $total_pairs++;

        # Check for troop type overlap
        unless ($job->_has_troop_overlap($g1, $g2)) {
          $filtered_pairs++;
          next;
        }

        # Keep this entry
        $filtered_conflicts{$g1_name}{$g2_name} =
          $raw_data->{$g1_name}{$g2_name};
        $kept_pairs++;
      }
    }

    $job->logger->info(sprintf(
      'Loaded ML conflicts: %d total, %d filtered (no troop overlap), %d kept',
      $total_pairs, $filtered_pairs, $kept_pairs
    ));
    $job->note(
      total_pairs     => $total_pairs,
      filtered_pairs  => $filtered_pairs,
      kept_pairs      => $kept_pairs,
    );

    # Store in SQLite for persistence across restarts
    my $stored_count = 0;
    for my $g1_name (keys %filtered_conflicts) {
      for my $g2_name (keys %{ $filtered_conflicts{$g1_name} }) {
        my $prediction = $filtered_conflicts{$g1_name}{$g2_name};
        my $conflicts = $prediction->{conflict} ? 1 : 0;
        $job->persistence->store_conflict($g1_name, $g2_name, $conflicts);
        $stored_count++;
      }
    }

    $job->logger->info("Stored $stored_count ML predictions in SQLite persistence");

    # Mark this job as completed in persistence
    $job->persistence->mark_job_completed($job->task_name);
  }

  sub _has_troop_overlap ($self, $g1, $g2) {
    my @g1_types = @{ $g1->type // [] };
    my @g2_types = @{ $g2->type // [] };

    for my $t1 (@g1_types) {
      return 1 if any { $_ eq $t1 } @g2_types;
    }

    return 0;
  }
}

1;
__END__

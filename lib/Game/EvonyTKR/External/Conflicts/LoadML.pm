use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Conflicts::LoadML {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::File;
  use List::AllUtils qw(any);

  sub task_name {'load_ml_conflicts'}

  sub register ($taskClass, $app, $conf = {}) {
    return 1 unless $taskClass->SUPER::register($app, $conf);
    if (not defined($app)) {
      my $errmessage = 'app not defined in register for ' . __PACKAGE__;
      say $errmessage;
      $taskClass->log_error($errmessage);
      return;
    }
    unless (defined($app->minion)) {
      my $errmessage = sprintf('minion undefined in job for %s', __PACKAGE__);
      $taskClass->log_error($errmessage);
      say $errmessage;
      return;
    }
    $taskClass->log_debug('Registering ML Conflicts Loader task');
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
      $job->log_info(
        'conflicts.json not found - generating ML model and predictions');

      # Run training pipeline
      my $rc = system(
        'perl',            'bin/extract_conflict_features.pl',
        '--mode=training', '--output=training_data.csv'
      );
      if ($rc != 0) {
        my $errmsg = "Failed to extract training features: exit code $rc";
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }

      $rc = system(
        'python',                       'bin/train_conflict_model.py',
        '--training=training_data.csv', '--model=conflict_model.pkl',
        '--importance=feature_importance.csv'
      );
      if ($rc != 0) {
        my $errmsg = "Failed to train ML model: exit code $rc";
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }

      $rc = system(
        'perl',           'bin/extract_conflict_features.pl',
        '--mode=predict', '--output=all_pairs.csv'
      );
      if ($rc != 0) {
        my $errmsg = "Failed to extract prediction features: exit code $rc";
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }

      $rc = system(
        'python',                     'bin/predict_conflicts.py',
        '--model=conflict_model.pkl', '--pairs=all_pairs.csv',
        '--output=conflicts.json'
      );
      if ($rc != 0) {
        my $errmsg = "Failed to generate predictions: exit code $rc";
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }

      $job->log_info('ML model training and prediction complete');

      # Verify the file was created
      unless (-f $json_path) {
        my $errmsg = "conflicts.json still not found after training";
        $job->log_error($errmsg);
        return $job->fail($errmsg);
      }
    }

    $job->log_info('Loading ML conflict predictions from conflicts.json');

    # Load JSON
    my $json_text = $json_path->slurp;
    my $raw_data  = $job->decode($json_text);

    # Get all generals for type checking (normalize names to match JSON keys)
    my %generals =
      map { $job->normalize($_->name) => $_ } $job->get_generals()->@*;

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

    $job->log_info(sprintf(
      'Loaded ML conflicts: %d total, %d filtered (no troop overlap), %d kept',
      $total_pairs, $filtered_pairs, $kept_pairs
    ));
    $job->note(
      total_pairs    => $total_pairs,
      filtered_pairs => $filtered_pairs,
      kept_pairs     => $kept_pairs,
    );

    # Store in SQLite for persistence across restarts
    my $stored_count = 0;
    for my $g1_name (keys %filtered_conflicts) {
      for my $g2_name (keys %{ $filtered_conflicts{$g1_name} }) {
        my $prediction = $filtered_conflicts{$g1_name}{$g2_name};
        my $conflicts  = $prediction->{conflict} ? 1 : 0;
        $job->persistence->store_conflict($g1_name, $g2_name, $conflicts);
        $stored_count++;
      }
    }

    $job->log_info("Stored $stored_count ML predictions in SQLite persistence");

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

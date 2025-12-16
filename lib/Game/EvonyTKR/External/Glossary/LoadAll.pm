use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::External::Glossary::LoadAll {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use Mojo::File;
  use Mojo::Home;
  use YAML::PP;

  sub task_name {'load_all_glossary_terms'}

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
    $taskClass->log_debug('Registering Glossary LoadAll task');
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

    $job->log_info('Starting load_all_glossary_terms job');

    # Find glossary YAML files
    my $mh =
      Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
    my $glossary_dir = $mh->child('share/collections/Glossary');

    unless (-d $glossary_dir) {
      my $errmsg = "Glossary directory not found: $glossary_dir";
      $job->log_error($errmsg);
      return $job->fail($errmsg);
    }

    my @yaml_files = $glossary_dir->list->grep(sub {
      $_ =~ /\.ya?ml$/ && -f -r $_ && $_ !~ /schema/;
    })->each;

    $job->log_info(
      sprintf('Found %d glossary YAML files', scalar(@yaml_files)));

    my $yp          = YAML::PP->new();
    my $total_terms = 0;

    foreach my $file (@yaml_files) {
      $job->log_debug("Processing $file");

      my $data = eval { $yp->load_file($file->to_string) };
      if ($@) {
        $job->log_error("Failed to parse $file: $@");
        next;
      }

      next unless $data->{glossary};

      foreach my $term_data (@{ $data->{glossary} }) {
        require Game::EvonyTKR::Model::Glossary;

        my $term = Game::EvonyTKR::Model::Glossary->new(
          term          => $term_data->{term},
          definition    => $term_data->{definition}    // '',
          synonyms      => $term_data->{synonyms}      // [],
          related_terms => $term_data->{related_terms} // [],
          examples      => $term_data->{examples}      // [],
          owner         => $term_data->{owner}         // '',
          status        => $term_data->{status}        // 'approved',
        );

        $job->add_glossary_term($term);
        $total_terms++;
      }
    }

    $job->log_info("Loaded $total_terms glossary terms");
    $job->note(total_terms => $total_terms);

    # Mark work unit as complete
    require Game::EvonyTKR::WorkUnit::Tracker;
    my $tracker = Game::EvonyTKR::WorkUnit::Tracker->new(persistence => $job->persistence);
    $tracker->mark_complete('load_all_glossary_terms');

    $job->app->log->info("Marked work unit 'load_all_glossary_terms' as complete");
  }
}

1;
__END__

=head1 NAME

Game::EvonyTKR::External::Glossary::LoadAll - Load all glossary terms from YAML files

=head1 DESCRIPTION

Reads glossary YAML files and stores terms in SQLite persistence.

=cut

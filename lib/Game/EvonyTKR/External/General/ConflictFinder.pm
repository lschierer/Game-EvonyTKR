use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require YAML::PP;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;

class Game::EvonyTKR::External::General::ConflictFinder :
  isa(Game::EvonyTKR::Shared::Constants) {
  use Unicode::Normalize;
  use Unicode::CaseFold qw(fc);
  use Encode            qw(is_utf8 decode_utf8 encode_utf8);
  use Carp;

  field $app : param;

  field $conflicts : param = {};
  field $pairs_by_type : param : reader = {};

  field $limit_length = 300;
  field $concurrent   = 3;

  field $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
  field $generals = {};
  field $builderJobs = [];
  field $conflictDetector =
    Game::EvonyTKR::Model::General::Conflict::Book->new(
    build_index      => 1,
    asst_has_dragon  => 1,
    asst_has_spirit  => 1,
    allow_wall_buffs => 1,
    );

  method get_tasks {
    return {
      detect_all_conflicts => sub ($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
        return $job->finish('only one conflict builder kickoff')
          unless my $guard =
          $job->app->minion->lock('detect_all_conflicts', 20 * $limit_length);
        my $cf = Game::EvonyTKR::External::General::ConflictFinder->new(
          app => $job->app,);
        return $cf->detect_all_conflicts($job, $args);
      },
      detect_conflicts_for_general => sub ($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
        unless (
          my $taskLimit = $job->minion->guard(
            'build_pairs_for_primary',
            $limit_length,
            {
              limit => $concurrent
            }
          )
        ) {
          $logger->INFO('Concurrency limit hit for build_pairs_for_primary');
# delay a random amount up to the limit length to allow for jobs not taking the full time
          return $job->retry({ delay => rand($limit_length) });
        }

        my $general_name = $args->{general_name};
        unless (length($general_name)) {
          $logger->ERR('general_name not provided to build_pairs_for_primary');
          return $job->finish(
            'general_name not provided to build_pairs_for_primary');
        }

        return $job->finish(
          sprintf('build_pairs_for_primary for %s already launched',
            $general_name)
          )
          unless my $bppGuard =
          $app->minion->guard("build_pairs_for_primary_${general_name}",
          2 * $limit_length);
        my $cf = Game::EvonyTKR::External::General::ConflictFinder->new(
          app => $job->app,);
        $self->detect_conflicts_for_general($job, $args);
      },
    };
  }

  method load_generals {
    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $bookDir     = $collectionDir->child('skill books');
    my $generalsDir = $collectionDir->child('generals');

    my @files = $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;
    my $expectedTotal = scalar(@files);

    @files = sort @files;

    foreach my $index (0 .. $#files) {
      my $generalFile = $files[$index];
      $self->logger->DEBUG("processing $generalFile, $index of $expectedTotal");
      my $data = $generalFile->slurp('UTF-8');
      my $ho   = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $g = Game::EvonyTKR::Model::General->from_hash($ho, $app->log);
      unless ($g) {
        $self->logger->ERR(sprintf(
          'failed to build general from %s', $generalFile));
        return undef;
      }

      my ($bookFile) = grep {
        my $nf = $self->normalize($_->basename('.yaml'));
        my $nn = $self->normalize($g->builtInBookName);
        $nf eq $nn;
      } $bookDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;
      unless (defined($bookFile)) {
        $self->worker_croak(
          sprintf('no yaml file found for "%s"', $g->builtInBookName));
        next;
      }
      my $bd  = $bookFile->slurp('UTF-8');
      my $bho = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($bd);
      my $book = Game::EvonyTKR::Model::Book::Builtin->from_hash($bho);

      unless ($book
        && Scalar::Util::blessed($book) eq
        'Game::EvonyTKR::Model::Book::Builtin') {
        $self->logger->ERR(sprintf(
          'failed to import book for file "%s", necessary for general "%s"',
          $bookFile, $g->name
        ));
        next;
      }
      $g->set_builtInBook($book);

      $generals->{ $self->normalize($g->name) } = $g;
      $self->logger->DEBUG(sprintf(
        'imported %s, general %s of %s',
        $g->name, scalar keys $generals->%*,
        $expectedTotal
      ));
    }
    $self->logger->INFO(sprintf(
      'imported %s of %s generals',
      scalar keys $generals->%*,
      $expectedTotal
    ));
  }

  method detect_all_conflicts($parent, $args) {
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalsDir = $collectionDir->child('generals');
    my @files    = $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;
    my $expected = scalar(@files);
    $parent->note(job_count => $expected);

    foreach my $fileName (@files) {
      $fileName = Mojo::File->new($fileName);
      my $general_name = $fileName->basename('.yaml');
      $general_name = $self->normalize($general_name);
      $self->logger->DEBUG(
        "normalized general_name $general_name for generalFile $fileName");
      $app->minion->enqueue(
        detect_conflicts_for_general => [{
          general_name            => $general_name,
          pairs_by_type           => $pairs_by_type,
          by_general              => ($conflictDetector->by_general // {}),
          groups_by_conflict_type =>
            ($conflictDetector->groups_by_conflict_type // {}),
        }] => {
          priority => 5,
          attempts => 5,
          delay    => rand($limit_length),
          expire   => 10 * $limit_length,
        }
      );
    }

  }

  method detect_conflicts_for_general ($parent, $args) {
    $self->load_generals();

    my $bookManager = Game::EvonyTKR::Model::Book::Manager->new();
    my $dist_dir    = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
    my $collectionDir = $dist_dir->child("collections/data");
    $bookManager->importAll($collectionDir->child('skill books'));

    foreach my $general (values $generals->%*) {
      unless ($general) {
        $self->logger->error(
          'ConflictWorkerLogic: undefined general in general manager!!');
        next;
      }
      $general->populateBuiltInBook($bookManager);
    }

    $conflictDetector->by_general = ($args->{by_general} // {});
    $conflictDetector->groups_by_conflict_type =
      ($args->{groups_by_conflict_type} // {});
    $pairs_by_type = $args->{groups_by_conflict_type} // {};
    my $general_name = $args->{general_name} // '';
    $general_name = $self->normalize($general_name);

    unless (length($general_name)) {
      $self->logger->ERR(
        'General Name is required for detect_conflicts_for_general.');
      return $parent->finish(
        'General Name is required for detect_conflicts_for_general.');
    }

    my $general = $generals->{$general_name};
    unless ($general) {
      $self->logger->ERR(sprintf(
        'General not found for %s in job %s',
        $general_name, $parent->info->{id}
      ));
      return $parent->finish(sprintf(
        'General not found for %s in job %s',
        $general_name, $parent->info->{id}
      ));
    }

    $conflictDetector->process_single_general($general, $generals);
    $self->merge_results($parent);
    $parent->note(
      groups_by_conflict_type => $conflictDetector->groups_by_conflict_type);
    $parent->note(by_general => $conflictDetector->by_general);
    $parent->note(complete   => 1);
    $parent->finish(sprintf('conflicts detected for %s', $general_name));
  }

  method merge_results ($toMergeFrom) {
    unless ($toMergeFrom) {
      $self->logger->ERR('Cannot merge results without a job to merge from.');
      return;
    }
    my $by_general = $toMergeFrom->info->notes->{by_general};
    foreach my $general (keys $by_general->%*) {
      $self->logger->DEBUG("conflicts in by_general for $general");
      foreach my $og ($by_general->{$general}->%*) {
        $self->logger->DEBUG(sprintf(
          'by_general reports conflict between %s <-> %s',
          $general, $og
        ));
        $conflictDetector->by_general->{$general}->{$og} = 1;
      }
    }
    my $groups_by_conflict_type =
      $toMergeFrom->info->notes->{groups_by_conflict_type};
    foreach my $group (keys $groups_by_conflict_type->%*) {
      $self->logger->DEBUG(sprintf(
        'conflicts in groups_by_conflict_type for %s: %s',
        $group,
        Data::Printer::np($conflicts->{groups_by_conflict_type}->{$group})
      ));
      my @all;
      if (exists $groups_by_conflict_type->{$group}
        && defined $groups_by_conflict_type->{$group}) {
        push @all, @{ $groups_by_conflict_type->{$group} };
      }
      push @all, $conflictDetector->groups_by_conflict_type->{$group}->@*;
      @{ $conflictDetector->groups_by_conflict_type->{$group} } =
        List::AllUtils::uniq @all;
    }
  }
}
1;

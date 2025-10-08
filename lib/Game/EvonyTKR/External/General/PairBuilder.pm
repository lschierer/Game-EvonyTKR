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

class Game::EvonyTKR::External::General::PairBuilder :
  isa(Game::EvonyTKR::Shared::Constants) {
  use Unicode::Normalize;
  use Unicode::CaseFold qw(fc);
  use Encode            qw(is_utf8 decode_utf8 encode_utf8);
  use Carp;

  field $app : param;

  field $conflicts : param = {};

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

  field $pairs_by_type : param : reader = {};

  method get_tasks {
    return {
      build_all_pairs => sub($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
        return $job->finish('only one pair builder kickoff')
          unless my $guard = $job->app->minion->guard('build_all_pairs', 360);
        my $collectionDir =
          Mojo::File->new($app->config('distDir'))->child('collections/data/');
        my $generalsDir = $collectionDir->child('generals');
        my @files = $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;

        foreach my $generalFile (sort @files) {
          $generalFile = Mojo::File->new($generalFile);
          my $general_name = $generalFile->basename('.yaml');
          $logger->DEBUG(
            "general_name $general_name for generalFile $generalFile");
          $general_name = $self->normalize_name($general_name);
          my $child = $job->app->minion->enqueue(
            build_pairs_for_primary => [{
              general_name => $general_name
            }] => {
              priority => 1,
              attempts => 5,
              delay    => 1 + rand(0.5),
              expire   => 3600,
            }
          );
          push @$builderJobs, $child;
        }
        $job->note(builderJobs => $builderJobs);
        return $job->finish('all builder jobs started');
      },
      build_pairs_for_primary => sub ($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
        # about 5 minutes
        my $limit_length = 7200;
        unless(my $taskLimit = $job->minion->guard('build_pairs_for_primary', $limit_length, {
        limit => 3 })) {
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
          $app->minion->guard("build_pairs_for_primary_${general_name}", 360);
        my $pb =
          Game::EvonyTKR::External::General::PairBuilder->new(app => $job->app,
          );
        $pb->load_generals();
        return $pb->build_pairs_for_primary($job, $general_name);
      },
      monitor_pair_builders => sub ($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);

        # Retrieve existing 'pairs_by_type' state from job notes
        my $pbt = $job->info->{notes}->{pairs_by_type} // {};

        return $job->finish('monitor_pair_builders already launched')
          unless $job->app->minion->guard('monitor_pair_builders', 300);

        my $pb = Game::EvonyTKR::External::General::PairBuilder->new(
          app           => $job->app,
          pairs_by_type => $pbt         # Use existing state
        );

        return $pb->monitor_pair_builders($job);
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
        my $nf = $self->normalize_name($_->basename('.yaml'));
        my $nn = $self->normalize_name($g->builtInBookName);
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

      $generals->{ $self->normalize_name($g->name) } = $g;
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

  method monitor_pair_builders ($monitor_job) {
    my $jobs = $app->minion->jobs({ tasks => ['build_pairs_for_primary'] });
    $self->logger->INFO(sprintf(
      'starting monitor_pair_builders %s for %s jobs',
      $monitor_job->info->{id},
      $jobs->total
    ));
    my $something_incomplete = 0;
    my $something_failed     = 0;
    $jobs->each(sub {
      my $info = $_;
      $self->logger->DEBUG(sprintf('inspecting job %s', $info->{id}));
      if ($info->{state} eq 'failed') {
        $self->logger->ERR(sprintf(
'monitor_pair_builders found pair builder JID %s failed with result "%s"',
          $info->{id}, $info->{result}
        ));
        $something_failed++;
        return;
      }
      if ($info->{state} eq 'finished') {
        $self->logger->DEBUG(sprintf(
'monitor_pair_builders found pair builder JID %s finished with result "%s"',
          $info->{id}, $info->{result}
        ));
        my $ngp = $info->{notes}->{pairs_by_type};
        $self->logger->INFO(sprintf(
          'monitor_pair_builders results from jid %s: %s',
          $info->{id}, Data::Printer::np($ngp, multiline => 0)
        ));
        $self->merge_new_pairs($ngp);
        return;
      }

      # at least one job is in progress, retry later.
      # storing the interum results for progressive progress
      $monitor_job->note(pairs_by_type => $pairs_by_type);
      $self->logger->DEBUG(sprintf(
        'monitor_pair_builders found job %s is incomplete, triggering retry',
        $info->{id}));
      $something_incomplete++;
      return $monitor_job->retry({ delay => 10 });
    });

    $monitor_job->note(pairs_by_type => $pairs_by_type);
    if ($something_failed) {
      return $monitor_job->finish(
        "monitor_pair_builders found $something_failed jobs failed");
    }
    if ($something_incomplete) {
      return $monitor_job->retry({
        delay  => 10,
        result =>
          "monitor_pair_builders found $something_incomplete jobs incomplete"
      });
    }
    return $monitor_job->finish('all pair builders complete');
  }

  method merge_new_pairs ($npbt) {
    foreach my $type (sort keys %$npbt) {
      my @all;
      push @all, $npbt->{$type}->@*;
      if (exists $pairs_by_type->{$type}) {
        push @all, $pairs_by_type->{$type}->@*;
      }
      my @unique =
        List::UtilsBy::uniq_by { $_->{primary} . '/' . $_->{secondary} }
      @all;
      $pairs_by_type->{$type} = \@unique;
      $self->logger->DEBUG(sprintf(
        'there are %s pairs of type %s after merge.',
        scalar(@{ $pairs_by_type->{$type} }), $type
      ));
    }
  }

  method build_all_pairs {
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
      $generalFile = Mojo::File->new($generalFile);
      my $general_name = $self->normalize_name($generalFile->basename('.yaml'));
      $self->logger->DEBUG("Found name $general_name from file $generalFile");
      my $jid = $app->minion->enqueue(
        build_pairs_for_primary => [{
          general_name => $self->normalize_name($general_name),
        }],
        {
          priority => -1,
          attempts =>  5,
          expire   => 7200,
        }
      );
      push @$builderJobs, $jid;
      $self->logger->DEBUG(sprintf(
        'pair builder kicked off for general "%s" with jid %s',
        $general_name, $jid
      ));
    }
    return $builderJobs;
  }

  method build_pairs_for_primary ($job, $general_name) {
    my $primary = $generals->{ $self->normalize_name($general_name) };
    unless ($primary) {
      $self->logger->ERR("general for $general_name not found!");
      $self->logger->DEBUG(sprintf(
        'available generals are %s',
        join ', ', sort keys $generals->%*
      ));
      return $job->finish("general for $general_name not found!");
    }

    my %initial_counts;
    foreach my $type (keys %{$pairs_by_type}) {
      my $tc = scalar @{ $pairs_by_type->{$type} } // 0;
      $initial_counts{$type} = $tc;
    }

    foreach my $secondary (sort { $a->name cmp $b->name } values %{$generals}) {
      next if $primary->name eq $secondary->name;
      $self->logger->DEBUG(sprintf(
        'testing if %s and %s conflict.',
        $primary->name, $secondary->name
      ));
      next
        unless $conflictDetector->are_generals_compatible($primary, $secondary);

      $self->logger->DEBUG(sprintf(
        'no conflict, testing %s and %s for common type.',
        $primary->name, $secondary->name
      ));
      my $primary_types     = $primary->type   // [];
      my $secondary_types   = $secondary->type // [];
      my %primary_types_map = map  { $_ => 1 } @$primary_types;
      my @common            = grep { $primary_types_map{$_} } @$secondary_types;
      @common = sort @common;
      next unless (scalar(@common) > 0);

      my $pair = {
        primary   => $primary->name,
        secondary => $secondary->name,
      };

      for my $t (@common) {
        $self->logger->DEBUG(sprintf(
          '%s <-> %s as %s', $pair->{primary}, $pair->{secondary}, $t));
        push @{ $pairs_by_type->{$t} }, $pair;
      }
    }

    my $total_added = 0;
    foreach my $type (keys %{$pairs_by_type}) {
      my $tc    = scalar @{ $pairs_by_type->{$type} } // 0;
      my $delta = $tc - ($initial_counts{$type} // 0);
      $total_added += $delta;
      $self->logger->DEBUG(sprintf(
        'general %s has %s pairs for type %s',
        $primary->name, $delta, $type
      ));
    }
    $self->logger->INFO(
      sprintf('there are %s pairs for %s', $total_added, $primary->name));
    $job->note(pairs_by_type => $pairs_by_type);
    return $job->finish({ pairs_by_type => $pairs_by_type });
  }

  method normalize_name ($name) {
    my $dn = is_utf8($name) ? $name : decode_utf8($name);
    my $nn = fc(NFKD($dn));
    $nn =~ s/[’''‛`´]/'/g;
    $nn =~ s/[""‟]/"/g;      # Quotes
    return $nn;
  }
}
1;
__END__

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

  field $app           : param;

  field $conflicts     : param = {};

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

  field $pairs_by_type : reader = {};

  method get_tasks {
    return {
      build_all_pairs => sub($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
        return $job->finish('only one pair builder kickoff') unless my $guard = $job->app->minion->guard('build_all_pairs', 360);
        my $collectionDir = Mojo::File->new($app->config('distDir'))
          ->child('collections/data/');
        my $generalsDir   = $collectionDir->child('generals');
        my @files = $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;

        foreach my $generalFile (sort @files) {
          $generalFile = Mojo::File->new($generalFile);
          my $general_name = $generalFile->basename('.yaml');
          $logger->DEBUG("general_name $general_name for generalFile $generalFile");
          $general_name = $self->normalize_name($general_name);
          my $child = $job->app->minion->enqueue(build_pairs_for_primary => [{
            general_name => $general_name}] => {
            priority  => 1,
            attempts  => 5,
            delay     => 1 + rand(0.5),
            expire    => 3600,
          });
          push @$builderJobs, $child;
        }
        return $builderJobs;
      },
      build_pairs_for_primary => sub ($job, $args) {
        my $general_name = $args->{general_name};
        unless (length($general_name)) {
          $self->logger->ERR(
            'general_name not provided to build_pairs_for_primary');
          return $job->finish(
            'general_name not provided to build_pairs_for_primary');
        }
        return $job->finish(
          sprintf('build_pairs_for_primary for %s already launched',
            $general_name)
          )
          unless my $bppGuard =
          $app->minion->guard("build_pairs_for_primary_${general_name}", 360);
        return $self->build_pairs_for_primary($job, $general_name);
      },
      monitor_pair_builders => sub ($job, $args) {
        my $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
        return $job->finish('monitor_pair_builders already launched')
          unless $app->minion->guard('monitor_pair_builders', 360);
        my $pb = $args->{pairBuilder};
        if (length(Scalar::Util::blessed($pb)) && Scalar::Util::blessed($pb) eq 'Game::EvonyTKR::External::General::PairBuilder') {
          return $pb->monitor_pair_builders($job);
        }elsif(!length(Scalar::Util::blessed($pb))){
          $logger->ERR(sprintf('$pb has no value with scalar util blessed.: %s', Data::Printer::np($pb)));
        }
      },
    };
  }

  method load_generals {
    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    my $collectionDir = Mojo::File->new($app->config('distDir'))
      ->child('collections/data/');
    my $bookDir       = $collectionDir->child('skill books');
    my $generalsDir   = $collectionDir->child('generals');

    my @files = $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;
    my $expectedTotal = scalar(@files);

    @files = sort @files;

    foreach my $index ( 0 .. $#files ){
      my $generalFile = $files[$index];
      $self->logger->DEBUG("processing $generalFile, $index of $expectedTotal");
      eval {
        my $data = $generalFile->slurp('UTF-8');
        my $ho   = YAML::PP->new(
          schema       => [qw/ + Perl /],
          yaml_version => ['1.2', '1.1'],
        )->load_string($data);
        my $g =
          Game::EvonyTKR::Model::General->from_hash($ho, $app->log);
        unless ($g) {
          $self->logger->ERR(sprintf(
            'failed to build general from %s', $generalFile));
          return undef;
        }

        my ($bookFile) = grep {
          my $nf = $self->normalize_name($_->basename('.yaml'));
          my $nn = $self->normalize_name($g->builtInBookName);
          $nf eq $nn;
        } $bookDir->children;
        unless (defined($bookFile) && $bookFile->is_file()) {
          $self->worker_croak(
            sprintf('no yaml file found for "%s"', $g->builtInBookName));
          next;
        }
        my $bd   = $bookFile->slurp_utf8;
        my $bho  = $ypp->load_string($bd);
        my $book = Game::EvonyTKR::Model::Book::Builtin->from_hash($bho);

        unless($book && Scalar::Util::blessed($book) eq 'Game::EvonyTKR::Model::Book::Builtin') {
          $self->logger->ERR(sprintf('failed to import book for file "%s", necessary for general "%s"', $bookFile, $g->name));
          next;
        }
        $g->set_builtInBook($book);

        $generals->{$self->normalize_name($g->name)} = $g;

      }
    }
  }

  method monitor_pair_building {
    my $jid = $app->minion->enqueue(
      monitor_pair_builders => [{ pairBuilder => $self }],
      {
        priority => 10,
        attempts => 5,
        expire   => 7200,
      }
    );
    return $jid;
  }

  method monitor_pair_builders ($monitor_job) {

    my $jobs = $app->minion->jobs({ tasks => ['build_pairs_for_primary'] });
    while (my $info = $jobs->next) {
      my $pbJid = $info->{id};
      my $pbJob = $app->minion->jobs($pbJid);
      unless ($pbJob) {
        $self->logger->ERR(
          "pair builder JID $pbJid does not have a valid job associated.");
        next;
      }
      if ($info->{state} eq 'failed') {
        $self->logger->ERR(sprintf(
          'pair builder JID %s failed with result "%s"',
          $pbJid, $info->{result}
        ));
        next;
      }
      if ($info->{state} eq 'finished') {
        $self->logger->DEBUG(sprintf(
          'pair builder JID %s finished with result "%s"',
          $pbJid, $info->{result}
        ));
        my $ngp = $info->notes->{pairs_by_type};
        $self->merge_new_pairs($ngp);
        next;
      }
      # at least one job is in progress, retry later.
      return $monitor_job->retry({ delay => 10 });
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
        List::UtilsBy::uniq_by { $_->primary->name . '/' . $_->secondary->name }
      @all;
      $pairs_by_type->{$type} = \@unique;
      $self->logger->DEBUG(sprintf(
        'there are %s pairs of type %s after merge.',
        scalar(@{ $pairs_by_type->{$type} }), $type
      ));
    }
  }

  method build_all_pairs {
    $self->load_generals();
    foreach my $general (sort { $a->name cmp $b->name } values $generals->%*) {
      my $jid = $app->minion->enqueue(
        build_pairs_for_primary => [{
          general_name => $self->normalize_name($general->name),
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
        $general->name, $jid
      ));
    }
    return $builderJobs;
  }

  method build_pairs_for_primary ($job, $general_name) {
    my $primary = $generals->{$self->normalize_name($general_name)};
    unless ($primary) {
      $self->logger->ERR("general for $general_name not found!");
      $self->logger->DEBUG(sprintf('available generals are %s', join ', ', sort keys $generals->%*));
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

use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require Log::Log4perl;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;

package Game::EvonyTKR::External::General::PairBuilder {
  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  sub register ($self, $app, $conf = {}) {
    $app->minion->add_task(
      build_pairs_for_primary => sub ($job, $args) {
        my $worker = PairBuilderWorkerLogic->new();
        my $result = $worker->execute($args);
        $job->note(pairs_by_type => $result);
        $job->finish("Pairs Created for " . $args->{general_name});
      }
    );
  }

  class PairBuilderWorkerLogic : isa(Game::EvonyTKR::Shared::Constants) {
    use Log::Log4perl qw(:levels);
    use Unicode::Normalize;
    use Unicode::CaseFold qw(fc);
    use Encode            qw(is_utf8 decode_utf8 encode_utf8);
    use Carp;

    field $pairs_by_type = {};    # e.g., { Mounted => [ [genA, genB], ... ] }

    field $generalManager = Game::EvonyTKR::Model::General::Manager->new();
    field $bookManager    = Game::EvonyTKR::Model::Book::Manager->new();
    field $conflictDetector =
      Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
      );

    method execute($args) {

      my $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
      my $collectionDir = $dist_dir->child("collections/data");
      $generalManager->importAll($collectionDir->child("generals"));
      $bookManager->importAll($collectionDir->child('skill books'));
      $bookManager->importAll($collectionDir->child('generic books'));
      # Create conflict detector

      foreach my $general (values $generalManager->get_all_generals()->%*) {
        unless ($general) {
          $self->logger->error(
            'ConflictWorkerLogic: undefined general in general manager!!');
          next;
        }
        $general->populateBuiltInBook($bookManager);
      }

      my $conflicts = $args->{conflicts};
      if ($conflicts) {
        foreach my $general ($conflicts->{by_general}->%*) {
          foreach my $og ($conflicts->{by_general}->{$general}->%*) {
            $conflictDetector->by_general->{$general}->{$og} = 1;
          }
        }
        foreach my $group (keys $conflicts->{groups_by_conflict_type}->%*) {
          my @all;
          if (exists $conflictDetector->groups_by_conflict_type->{$group}
            && defined $conflictDetector->groups_by_conflict_type->{$group}) {
            push @all,
              @{ $conflictDetector->groups_by_conflict_type->{$group} };
          }
          push @all, @{ $conflicts->{groups_by_conflict_type}->{$group} };
          @{ $conflictDetector->groups_by_conflict_type->{$group} } =
            List::AllUtils::uniq @all;
        }
      }

      my $generalName = $args->{general_name};
      if ($generalName) {
        my $general = $generalManager->getGeneral($generalName);
        if ($general) {
          $self->build_pairs($general);
        }
      }
      return $pairs_by_type;
    }

    method build_pairs($primary) {
      my $generals = $generalManager->get_all_generals();
      my %initial_counts;
      foreach my $type (keys %{$pairs_by_type}) {
        my $tc = scalar @{ $pairs_by_type->{$type} } // 0;
        $initial_counts{$type} = $tc;
      }

      foreach
        my $secondary (sort { $a->name cmp $b->name } values %{$generals}) {
        next if $primary->name eq $secondary->name;
        $self->logger->debug(sprintf(
          'testing if %s and %s conflict.',
          $primary->name, $secondary->name
        ));
        next
          unless $conflictDetector->are_generals_compatible($primary,
          $secondary);

        $self->logger->debug(sprintf(
          'no conflict, testing %s and %s for common type.',
          $primary->name, $secondary->name
        ));
        my $primary_types     = $primary->type   // [];
        my $secondary_types   = $secondary->type // [];
        my %primary_types_map = map { $_ => 1 } @$primary_types;
        my @common = grep { $primary_types_map{$_} } @$secondary_types;
        @common = sort @common;
        next unless @common;
        $self->logger->debug(sprintf(
          '%s and %s pair based on %s',
          $primary->name, $secondary->name, Data::Printer::np(@common)
        ));
        my $pair = {
          primary   => $primary->name,
          secondary => $secondary->name,
        };

        for my $t (@common) {
          push @{ $pairs_by_type->{$t} }, $pair;
        }
      }

      my $total_added = 0;
      foreach my $type (keys %{$pairs_by_type}) {
        my $tc    = scalar @{ $pairs_by_type->{$type} } // 0;
        my $delta = $tc - ($initial_counts{$type} // 0);
        $total_added += $delta;
        $self->logger->debug(sprintf(
          'general %s has %s pairs for type %s',
          $primary->name, $delta, $type
        ));
      }
      $self->logger->info(
        sprintf('there are %s pairs for %s', $total_added, $primary->name));

    }
  }
}
1;
__END__

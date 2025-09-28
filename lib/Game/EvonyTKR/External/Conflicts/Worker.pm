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

package Game::EvonyTKR::External::Conflicts::Worker {
  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  sub register ($self, $app, $conf = {}) {
    $app->minion->add_task(
      detect_conflicts_for_general => sub ($job, $args) {
        my $worker = ConflictWorkerLogic->new();
        my $result = $worker->process_general($args);
        $job->note(conflicts => $result);
        $job->finish("Conflicts detected for " . $args->{general_name});
      }
    );
  }

  class ConflictWorkerLogic : isa(Game::EvonyTKR::Shared::Constants) {
    use Log::Log4perl qw(:levels);
    use Unicode::Normalize;
    use Unicode::CaseFold qw(fc);
    use Encode            qw(is_utf8 decode_utf8 encode_utf8);
    use Carp;

    method process_general($args) {
      my $general_name = $args->{general_name};

      # Load all generals (like buff worker does)
      my $generalManager = Game::EvonyTKR::Model::General::Manager->new();
      my $bookManager    = Game::EvonyTKR::Model::Book::Manager->new();
      my $dist_dir = Path::Tiny::path(File::Share::dist_dir('Game::EvonyTKR'));
      my $collectionDir = $dist_dir->child("collections/data");
      $generalManager->importAll($collectionDir->child("generals"));
      $bookManager->importAll($collectionDir->child('skill books'));
      $bookManager->importAll($collectionDir->child('generic books'));
      # Create conflict detector

      foreach my $general (values $generalManager->get_all_generals()->%*) {
        unless($general){
          $self->logger->error('ConflictWorkerLogic: undefined general in general manager!!');
          next;
        }
        $general->populateBuiltInBook($bookManager);
      }

      # Process conflicts for this general
      my $general = $generalManager->getGeneral($general_name);
      unless($general){
        $self->logger->error(sprintf('ConflictWorkerLogic: general "%s" is not available in the general manager! Available Generals are: %s',
        $general_name, join ', ', sort map { $_->name } values $generalManager->get_all_generals()->%* ));
        return;
      }
      $general->populateBuiltInBook($bookManager);

      my $conflictDetector =
        Game::EvonyTKR::Model::General::Conflict::Book->new(
        build_index      => 1,
        asst_has_dragon  => 1,
        asst_has_spirit  => 1,
        allow_wall_buffs => 1,
        );

      $conflictDetector->process_single_general($general, $generalManager);

      # Extract and return results
      return {
        by_general              => $conflictDetector->by_general,
        groups_by_conflict_type => $conflictDetector->groups_by_conflict_type,
        processed_general       => $general_name,
      };
    }
  }
}
1;
__END__

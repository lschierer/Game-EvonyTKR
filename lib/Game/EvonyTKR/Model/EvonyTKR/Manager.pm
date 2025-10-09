use v5.42.0;
use experimental qw(class);
use utf8::all;
require Path::Tiny;
require Game::EvonyTKR::Model::Glossary::Manager;
require Game::EvonyTKR::Model::General::Conflict::Book;
use namespace::clean;

class Game::EvonyTKR::Model::EvonyTKR::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {

  field $SourceDir                  : reader : param;
  field $glossaryManager            : reader;

  # computed types
  field $conflictDetector : reader : writer;

  ADJUST {
    # first the import types

    $SourceDir = Path::Tiny::path($SourceDir);

    $glossaryManager =
      Game::EvonyTKR::Model::Glossary::Manager->new(SourceDir => $SourceDir,);

    $conflictDetector = Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 0,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
    );

  }

  method rootImport () {
    # first the imported types

    my $collectionDir = $SourceDir->child("collections/data");
    $self->logger->INFO("starting root import");

    $glossaryManager->importAll($SourceDir->child("collections/Glossary"));

    $self->logger->INFO("root import complete");

  }

}

use v5.40.0;
use experimental qw(class);
use utf8::all;
require Path::Tiny;
require Game::EvonyTKR::Model::Book::Manager;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::ConflictGroup::Manager;
require Game::EvonyTKR::Model::General::Pair::Manager;
use namespace::clean;

class Game::EvonyTKR::Model::EvonyTKR::Manager :isa(Game::EvonyTKR::Model::Data) {

  field $SourceDir : reader : param;
  field $generalManager : reader;
  field $generalConflictGroupManager : reader ;
  field $bookManager: reader;

  # computed types
  field $generalPairManager :reader;

  ADJUST {
    # first the import types
    $generalManager = Game::EvonyTKR::Model::General::Manager->new();
    $generalConflictGroupManager =
      Game::EvonyTKR::Model::General::ConflictGroup::Manager->new();
    $bookManager = Game::EvonyTKR::Model::Book::Manager->new();
    $SourceDir = Path::Tiny::path($SourceDir);

    $generalPairManager = Game::EvonyTKR::Model::General::Pair::Manager->new(
      generalManager        => $generalManager,
      conflictGroupManager  => $generalConflictGroupManager
    );

  }

  method rootImport () {
    # first the imported types

    my $collectionDir = $SourceDir->child("collections");
    $self->logger->info("starting root import");

    $self->logger->info("starting import of generals.");
    $generalManager->importAll($collectionDir->child("generals"));
    $self->logger->info("import of generals complete.");

    $self->logger->info(" starting import of conflict groups.");
    $generalConflictGroupManager->importAll(
      $collectionDir->child('general conflict groups'));
    $self->logger->info("import of conflict groups complete");

    $self->logger->info(" starting import of books.");
    $bookManager->importAll(
      $collectionDir->child('skill books'));
    $self->logger->info("import of books complete");

    $self->logger->info("root import complete");

    # then the computed types
    $self->logger->info("starting computed types");
    $self->logger->info("starting build pairs");
    $generalPairManager->build_pairs();
    $self->logger->info("build pairs complete");
    $self->logger->info("computed types complete");
  }

}

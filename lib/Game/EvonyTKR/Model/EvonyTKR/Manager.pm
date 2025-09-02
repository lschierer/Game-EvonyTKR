use v5.42.0;
use experimental qw(class);
use utf8::all;
require Path::Tiny;
require Game::EvonyTKR::Model::Book::Manager;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Game::EvonyTKR::Model::Specialty::Manager;
require Game::EvonyTKR::Model::AscendingAttributes::Manager;
require Game::EvonyTKR::Model::Covenant::Manager;
require Game::EvonyTKR::Model::Glossary::Manager;
use namespace::clean;

class Game::EvonyTKR::Model::EvonyTKR::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {

  field $SourceDir                   : reader : param;
  field $generalManager              : reader;
  field $bookManager                 : reader;
  field $specialtyManager            : reader;
  field $ascendingAttributesManager  : reader;
  field $covenantManager             : reader;
  field $glossaryManager             : reader;

  # computed types
  field $generalPairManager : reader;

  ADJUST {
    # first the import types
    $generalManager = Game::EvonyTKR::Model::General::Manager->new();
    $bookManager      = Game::EvonyTKR::Model::Book::Manager->new();
    $specialtyManager = Game::EvonyTKR::Model::Specialty::Manager->new();
    $ascendingAttributesManager =
      Game::EvonyTKR::Model::AscendingAttributes::Manager->new();
    $covenantManager =
      Game::EvonyTKR::Model::Covenant::Manager->new(rootManager => $self,);

    $SourceDir = Path::Tiny::path($SourceDir);

    $glossaryManager =
      Game::EvonyTKR::Model::Glossary::Manager->new(SourceDir => $SourceDir,);

    $generalPairManager = Game::EvonyTKR::Model::General::Pair::Manager->new(
      rootManager    => $self,
      generalManager => $generalManager,
    );

  }

  method rootImport () {
    # first the imported types

    my $collectionDir = $SourceDir->child("collections/data");
    $self->logger->info("starting root import");

    $self->logger->info("starting import of generals.");
    $generalManager->importAll($collectionDir->child("generals"));
    $self->logger->info("import of generals complete.");

    $self->logger->info(" starting import of books.");
    $bookManager->importAll($collectionDir->child('skill books'));
    $bookManager->importAll($collectionDir->child('generic books'));
    $self->logger->info("import of books complete");

    $self->logger->info(" starting import of specialties.");
    $specialtyManager->importAll($collectionDir->child('specialties'));
    $self->logger->info("import of specialties complete");

    $self->logger->info(" starting import of ascending attributes.");
    $ascendingAttributesManager->importAll(
      $collectionDir->child('ascending attributes'));
    $self->logger->info("import of ascending attributes complete");

    $self->logger->info(" starting import of covenants.");
    $covenantManager->importAll($collectionDir->child('covenants'));
    $self->logger->info("import of covenants complete");

    $glossaryManager->importAll($SourceDir->child("collections/Glossary"));

    $self->logger->info("root import complete");

    # then the computed types
    $self->logger->info("starting computed types");
    $self->logger->info("starting build pairs");
    $generalPairManager->build_pairs();
    $self->logger->info("build pairs complete");
    $self->logger->info("computed types complete");
  }

}

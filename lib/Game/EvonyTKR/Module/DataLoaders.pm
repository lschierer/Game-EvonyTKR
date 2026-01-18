package Game::EvonyTKR::Module::DataLoaders;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;

extends 'Thunderhorse::Module';

with 'WebFramework::Role::Logger';

use Game::EvonyTKR::Loader::Specialties;
use Game::EvonyTKR::Loader::Books;
use Game::EvonyTKR::Loader::AscendingAttributes;
use Game::EvonyTKR::Loader::Generals;
use Game::EvonyTKR::Loader::Covenants;
use Game::EvonyTKR::Loader::Conflicts;
use Game::EvonyTKR::Loader::Pairs;

# Build method runs at app startup
sub build ($self) {
  $self->logger->info("Loading game data...");

  # Load specialties synchronously at startup
  my $specialty_loader = Game::EvonyTKR::Loader::Specialties->new(
    data_dir => 'share/collections/data/specialties');

  $self->logger->info("Loading specialties...");
  my $specialty_count = $specialty_loader->load_all();
  $self->logger->info("Loaded $specialty_count specialties");

  # Register as helper so controllers can access it
  # Controllers can call $self->specialty_loader()
  $self->add_method(
    controller => specialty_loader => sub ($controller) {
      return $specialty_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{specialty_loader} = $specialty_loader;

  # Load books synchronously at startup
  my $books_loader =
    Game::EvonyTKR::Loader::Books->new(data_dir => 'share/collections/data');

  $self->logger->info("Loading books...");
  my $books_count = $books_loader->load_all();
  $self->logger->info("Loaded $books_count books");

  # Register as helper so controllers can access it
  # Controllers can call $self->books_loader()
  $self->add_method(
    controller => books_loader => sub ($controller) {
      return $books_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{books_loader} = $books_loader;

  # Load ascending attributes synchronously at startup
  my $aa_loader = Game::EvonyTKR::Loader::AscendingAttributes->new(
    data_dir => 'share/collections/data/ascending attributes');

  $self->logger->info("Loading ascending attributes...");
  my $aa_count = $aa_loader->load_all();
  $self->logger->info("Loaded $aa_count ascending attributes");

  # Register as helper so controllers can access it
  # Controllers can call $self->ascending_attributes_loader()
  $self->add_method(
    controller => ascending_attributes_loader => sub ($controller) {
      return $aa_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{ascending_attributes_loader} = $aa_loader;

  # Load generals synchronously at startup
  my $generals_loader = Game::EvonyTKR::Loader::Generals->new(
    data_dir => 'share/collections/data/generals');

  $self->logger->info("Loading generals...");
  my $generals_count = $generals_loader->load_all();
  $self->logger->info("Loaded $generals_count generals");

  # Register as helper so controllers can access it
  # Controllers can call $self->generals_loader()
  $self->add_method(
    controller => generals_loader => sub ($controller) {
      return $generals_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{generals_loader} = $generals_loader;

  # Load covenants synchronously at startup
  # Note: Covenants require generals to be loaded first
  my $covenants_loader = Game::EvonyTKR::Loader::Covenants->new(
    data_dir        => 'share/collections/data/covenants',
    generals_loader => $generals_loader,
  );

  $self->logger->info("Loading covenants...");
  my $covenants_count = $covenants_loader->load_all();
  $self->logger->info("Loaded $covenants_count covenants");

  # Register as helper so controllers can access it
  # Controllers can call $self->covenants_loader()
  $self->add_method(
    controller => covenants_loader => sub ($controller) {
      return $covenants_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{covenants_loader} = $covenants_loader;

  # Load conflicts (ML predictions) if available
  # Note: Conflicts loader uses generals_loader for validation
  my $conflicts_loader = Game::EvonyTKR::Loader::Conflicts->new(
    data_file       => 'conflicts.json',
    generals_loader => $generals_loader,
  );

  $self->logger->info("Loading conflicts...");
  my $conflicts_count = $conflicts_loader->load();
  if ($conflicts_count > 0) {
    $self->logger->info(sprintf(
      "Loaded %d conflict pairs (%d conflicts, %d compatible)",
      $conflicts_loader->total_pairs,
      $conflicts_loader->conflict_count,
      $conflicts_loader->stats->{compatible}
    ));
  }
  else {
    $self->logger->warn(
      "No conflicts loaded - pairs will use heuristic conflict detection");
  }

  # Register as helper so controllers can access it
  # Controllers can call $self->conflicts_loader()
  $self->add_method(
    controller => conflicts_loader => sub ($controller) {
      return $conflicts_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{conflicts_loader} = $conflicts_loader;

  # Load pairs (requires generals and conflicts loaders)
  # Pairs are generated at startup by combining generals and filtering conflicts
  my $pairs_loader = Game::EvonyTKR::Loader::Pairs->new(
    generals_loader  => $generals_loader,
    conflicts_loader => $conflicts_loader,
  );

  $self->logger->info("Generating pairs...");
  my $pairs_count = $pairs_loader->load_all();
  $self->logger->info(sprintf(
    "Generated %d pairs (%d conflicts filtered)",
    $pairs_count,
    $pairs_loader->stats->{conflicts_found}
  ));

  # Register as helper so controllers can access it
  # Controllers can call $self->pairs_loader()
  $self->add_method(
    controller => pairs_loader => sub ($controller) {
      return $pairs_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{pairs_loader} = $pairs_loader;
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Module::DataLoaders - Load game data at startup

=head1 DESCRIPTION

Thunderhorse module that loads all game data at application startup.

Currently loaded:
- Specialties
- Books (Skill and Generic)
- Ascending Attributes
- Generals
- Covenants
- Conflicts (ML predictions from conflicts.json)
- Pairs (generated from generals, filtered by conflicts)

Controllers can access loaders via helper methods:
- $self->specialty_loader()
- $self->books_loader()
- $self->ascending_attributes_loader()
- $self->generals_loader()
- $self->covenants_loader()
- $self->conflicts_loader()
- $self->pairs_loader()

=cut

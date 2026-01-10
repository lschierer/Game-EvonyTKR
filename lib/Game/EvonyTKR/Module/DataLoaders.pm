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

# Build method runs at app startup
sub build ($self) {
  $self->logger->info("Loading game data...");

  # Load specialties synchronously at startup
  my $specialty_loader = Game::EvonyTKR::Loader::Specialties->new(
    data_dir => 'share/collections/data/specialties'
  );

  $self->logger->info("Loading specialties...");
  my $specialty_count = $specialty_loader->load_all();
  $self->logger->info("Loaded $specialty_count specialties");

  # Register as helper so controllers can access it
  # Controllers can call $self->specialty_loader()
  $self->register(
    controller => specialty_loader => sub ($controller) {
      return $specialty_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{specialty_loader} = $specialty_loader;

  # Load books synchronously at startup
  my $books_loader = Game::EvonyTKR::Loader::Books->new(
    data_dir => 'share/collections/data'
  );

  $self->logger->info("Loading books...");
  my $books_count = $books_loader->load_all();
  $self->logger->info("Loaded $books_count books");

  # Register as helper so controllers can access it
  # Controllers can call $self->books_loader()
  $self->register(
    controller => books_loader => sub ($controller) {
      return $books_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{books_loader} = $books_loader;

  # Load ascending attributes synchronously at startup
  my $aa_loader = Game::EvonyTKR::Loader::AscendingAttributes->new(
    data_dir => 'share/collections/data/ascending attributes'
  );

  $self->logger->info("Loading ascending attributes...");
  my $aa_count = $aa_loader->load_all();
  $self->logger->info("Loaded $aa_count ascending attributes");

  # Register as helper so controllers can access it
  # Controllers can call $self->ascending_attributes_loader()
  $self->register(
    controller => ascending_attributes_loader => sub ($controller) {
      return $aa_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{ascending_attributes_loader} = $aa_loader;

  # Load generals synchronously at startup
  my $generals_loader = Game::EvonyTKR::Loader::Generals->new(
    data_dir => 'share/collections/data/generals'
  );

  $self->logger->info("Loading generals...");
  my $generals_count = $generals_loader->load_all();
  $self->logger->info("Loaded $generals_count generals");

  # Register as helper so controllers can access it
  # Controllers can call $self->generals_loader()
  $self->register(
    controller => generals_loader => sub ($controller) {
      return $generals_loader;
    }
  );

  # Store in app stash so it's accessible elsewhere
  $self->app->{generals_loader} = $generals_loader;
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

Future iterations will add:
- Covenants
- etc.

=cut

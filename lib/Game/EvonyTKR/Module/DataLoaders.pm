package Game::EvonyTKR::Module::DataLoaders;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;

extends 'Thunderhorse::Module';

with 'WebFramework::Role::Logger';

use Game::EvonyTKR::Loader::Specialties;

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
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Module::DataLoaders - Load game data at startup

=head1 DESCRIPTION

Thunderhorse module that loads all game data at application startup.

For the MVP, this loads:
- Specialties

Future iterations will add:
- Books
- Generals
- Ascending Attributes
- Covenants
- etc.

=cut

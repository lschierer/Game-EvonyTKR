use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Specialties {
  use Mojo::Base -role,                          -signatures;
  use Carp;

  has 'specialty_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'specialties:');
  };

  sub add_specialty ($self, $specialty) {
    my $key = lc($self->normalize($specialty->name));
    $key =~ s/ /_/g;
    return $self->specialty_cache->set($key, $specialty->to_wire_hash());
  }

  sub get_specialty ($self, $name) {
    state $specialties = {};

    $self->logger->debug("get_specialty called for: $name");

    my $normalized_name = lc($self->normalize($name));
    $normalized_name =~ s/ /_/g;
    if (exists $specialties->{$normalized_name}) {
      $self->logger->debug("Returning specialty $name from local cache");
      return $specialties->{$normalized_name};
    }

    $self->logger->debug("Looking for cache key: $normalized_name");

    my $wire_data = $self->specialty_cache->get($normalized_name);
    unless (defined($wire_data)) {
      $self->logger->warn("No wire_data found for key: $normalized_name");
      return;
    }

    $self->logger->debug("Found wire_data, attempting to build specialty");
    my $specialty =
      Game::EvonyTKR::Model::Factory->build_from_wire('Specialty', $wire_data);

    unless (defined($specialty)) {
      $self->logger->error("Factory failed to build specialty from wire_data");
      return;
    }

    $self->logger->debug("Successfully built specialty: " . $specialty->name);
    $specialties->{$normalized_name} = $specialty;
    return $specialty;
  }

  sub list_specialties ($self, $app) {
    unless (defined($app)) {
      $self->logger->logcroak('$app must be defined');
    }
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $specialtyDir = $collectionDir->child('specialties');
    my @suffixlist   = ('.yaml', '.yml');
    my @files = $specialtyDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
    return \@returnlist;
  }
}

1;

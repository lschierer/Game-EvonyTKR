package Game::EvonyTKR::Role::Persistence::Specialties;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;

has 'specialty_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'specialties:');
};

sub add_specialty ($self, $specialty) {
  my $name = $specialty->name;
  $self->persistence->store_specialty($name, $specialty->to_wire_hash());
  my $key = lc($self->normalize($name));
  $key =~ s/ /_/g;
  $self->specialty_cache->set($key, $specialty->to_wire_hash());
  return 1;
}

sub get_specialty ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $specialties = {};

  my $normalized_name = lc($self->normalize($name));
  $normalized_name =~ s/ /_/g;

  if (exists $specialties->{$normalized_name}) {
    $self->logger->debug("Returning specialty $name from state cache");
    return $specialties->{$normalized_name};
  }

  my $wire_data = $self->specialty_cache->get($normalized_name);

  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_specialty($name);

    if (defined($wire_data)) {
      $self->specialty_cache->set($normalized_name, $wire_data);
    }
  }

  return unless defined($wire_data);

  my $specialty =
    Game::EvonyTKR::Model::Factory->build_from_wire('Specialty', $wire_data);
  $specialties->{$normalized_name} = $specialty if defined($specialty);
  return $specialty;
}

sub list_specialties ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $sdir          = $collectionDir->child('specialties');
  my @suffixlist    = ('.yaml', '.yml');
  my @files         = $sdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Specialties - Specialty persistence operations

=cut

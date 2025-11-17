use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Covenants {
  use Mojo::Base -role, -signatures;
  use Carp;

  has 'covenant_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'covenants__');
  };

  sub add_covenant ($self, $ascendingAttribute) {
    my $key = lc($self->normalize($ascendingAttribute->general));
    $key =~ s/ /_/g;
    return $self->covenant_cache->set($key,
      $ascendingAttribute->to_wire_hash());
  }

  sub get_covenant ($self, $name) {
    state $Covenants = {};

    $self->logger->debug("get_covenant called for: $name");

    my $normalized_name = lc($self->normalize($name));
    $normalized_name =~ s/ /_/g;
    if (exists $Covenants->{$normalized_name}) {
      $self->logger->debug("Returning Covenant $name from local cache");
      return $Covenants->{$normalized_name};
    }

    $self->logger->debug("Looking for cache key: $normalized_name");

    my $wire_data = $self->covenant_cache->get($normalized_name);
    unless (defined($wire_data)) {
      $self->logger->warn("No wire_data found for key: $normalized_name");
      return;
    }

    $self->logger->debug("Found wire_data, attempting to build Covenant");
    my $covenant =
      Game::EvonyTKR::Model::Factory->build_from_wire('Covenant', $wire_data);

    unless (defined($covenant)) {
      $self->logger->error("Factory failed to build Covenant from wire_data");
      return;
    }

    $self->logger->debug(
      sprintf('Successfully built Covenant: "%s"', $covenant->primary->name));
    $Covenants->{$normalized_name} = $covenant;
    return $covenant;
  }

  sub list_covenants ($self, $app) {
    unless (defined($app)) {
      $self->logger->logcroak('$app must be defined');
    }
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $covenantsDir = $collectionDir->child('covenants');
    my @suffixlist   = ('.yaml', '.yml');
    my @files =
      $covenantsDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
    return \@returnlist;
  }
}

1;

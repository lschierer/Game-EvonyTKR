use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::AscendingAttributes {
  use Mojo::Base -role, -signatures;
  use Carp;

  has 'ascending_attribute_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(
      namespace => 'ascending_attributes__');
  };

  sub add_ascending_attribute ($self, $ascendingAttribute) {
    my $key = lc($self->normalize($ascendingAttribute->general));
    $key =~ s/ /_/g;
    return $self->ascending_attribute_cache->set($key,
      $ascendingAttribute->to_wire_hash());
  }

  sub get_ascending_attributes ($self, $name) {
    state $AscendingAttributes = {};

    $self->logger->debug("get_ascending_attributes called for: $name");

    my $normalized_name = lc($self->normalize($name));
    $normalized_name =~ s/ /_/g;
    if (exists $AscendingAttributes->{$normalized_name}) {
      $self->logger->debug(
        "Returning Ascending Attributes $name from local cache");
      return $AscendingAttributes->{$normalized_name};
    }

    $self->logger->debug("Looking for cache key: $normalized_name");

    my $wire_data = $self->ascending_attribute_cache->get($normalized_name);
    unless (defined($wire_data)) {
      $self->logger->warn("No wire_data found for key: $normalized_name");
      return;
    }

    $self->logger->debug(
      "Found wire_data, attempting to build AscendingAttributes");
    my $ascendingAttribute =
      Game::EvonyTKR::Model::Factory->build_from_wire('AscendingAttributes',
      $wire_data);

    unless (defined($ascendingAttribute)) {
      $self->logger->error(
        "Factory failed to build AscendingAttributes from wire_data");
      return;
    }

    $self->logger->debug("Successfully built ascendingAttributes: "
        . $ascendingAttribute->general);
    $AscendingAttributes->{$normalized_name} = $ascendingAttribute;
    return $ascendingAttribute;
  }

  sub list_ascending_attributes ($self, $app = undef) {
    my $collectionDir;
    unless (defined($app)) {
      use Cwd;
      $collectionDir = Mojo::File->new(cwd())->child('share/collections/data/');
      $self->logger->warn(
        sprintf(
          'collectionDir "%s" infered from cwd "%s"',
          $collectionDir, cwd()
        )
      );
    }
    else {
      $collectionDir =
        Mojo::File->new($app->config('distDir'))->child('collections/data/');
    }

    my $AscendingAttributesDir = $collectionDir->child('ascending attributes');
    my @suffixlist             = ('.yaml', '.yml');
    my @files =
      $AscendingAttributesDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
    return \@returnlist;
  }
}

1;

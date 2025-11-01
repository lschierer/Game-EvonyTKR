use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Generals {
  use Mojo::Base -role,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role;
  use List::AllUtils qw(uniq);
  use List::UtilsBy;
  use Carp;

  has 'general_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'generals:');
  };

  sub add_general ($self, $general) {
    my $key = lc($self->normalize($general->name));
    $key =~ s/ /_/g;
    return $self->general_cache->set($key, $general->to_wire_hash());
  }

  sub get_general ($self, $name) {
    state $generals = {};
    return if(not length($name));

    $self->logger->debug("get_general called for: $name");

    my $normalized_name = lc($self->normalize($name)) // '';
    return unless(length($normalized_name));
    $self->logger->debug(sprintf('key "%s" for name "%s"',
    $normalized_name, $name));
    $normalized_name =~ s/ /_/g;
    $self->logger->debug(sprintf('key after spaces removed: "%s", for name "%s"',
    $normalized_name, $name));

    if (exists $generals->{$normalized_name}) {
      $self->logger->debug("Returning general $name from local cache");
      return $generals->{$normalized_name};
    }

    $self->logger->debug("Looking for cache key: $normalized_name");

    my $wire_data = $self->general_cache->get($normalized_name);
    unless (defined($wire_data)) {
      $self->logger->warn("No wire_data found for key: $normalized_name");
      return;
    }

    $self->logger->debug("Found wire_data, attempting to build general");
    my $general =
      Game::EvonyTKR::Model::Factory->build_from_wire('General', $wire_data);

    unless (defined($general)) {
      $self->logger->error("Factory failed to build general from wire_data");
      return;
    }

    $self->logger->debug("Successfully built general: " . $general->name);
    $generals->{$normalized_name} = $general;
    return $general;
  }

  sub list_generals ($self, $app) {
    unless (defined($app)) {
      $self->logger->logcroak('$app must be defined');
    }
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $generalDir = $collectionDir->child('generals');
    my @suffixlist = ('.yaml', '.yml');
    my @files = $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
    return \@returnlist;
  }

  sub get_generals ($self) {
    # Get all generals from cache
    my $general_list = $self->list_generals($self->app);
    my %generals;

    foreach my $name (@$general_list) {
      my $general = $self->get_general($name);
      if ($general) {
        $generals{ $self->normalize($name) } = $general;
      }
    }

    return \%generals;
  }
}

1;
__END__

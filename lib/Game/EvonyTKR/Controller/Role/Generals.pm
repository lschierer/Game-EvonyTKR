use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Generals {
  use Mojo::Base -role, -signatures;
  use List::AllUtils qw(uniq);
  use List::UtilsBy;
  use Log::Any;
  use Carp;

  my $logger = Log::Any->get_logger(category => __PACKAGE__);

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
    return if (not length($name));

    $logger->debug("get_general called for: $name");

    my $normalized_name = lc($self->normalize($name)) // '';
    $normalized_name =~ s/ /_/g;
    return unless (length($normalized_name));
    $logger->debug(sprintf('key "%s" for name "%s"', $normalized_name, $name));

    if (exists $generals->{$normalized_name}) {
      $logger->debug("Returning general $name from local cache");
      return $generals->{$normalized_name};
    }

    $logger->debug("Looking for cache key: $normalized_name");

    my $wire_data = $self->general_cache->get($normalized_name);
    unless (defined($wire_data)) {
      $logger->warn("No wire_data found for key: $normalized_name");
      return;
    }

    $logger->debug("Found wire_data, attempting to build general");
    my $general =
      Game::EvonyTKR::Model::Factory->build_from_wire('General', $wire_data);

    unless (defined($general)) {
      $logger->error("Factory failed to build general from wire_data");
      return;
    }

    $logger->debug("Successfully built general: " . $general->name);
    $generals->{$normalized_name} = $general;
    return $general;
  }

  sub list_generals ($self,) {
    my $mh =
      Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
    my $collectionDir = $mh->child('share/collections/data');

    my $generalDir = $collectionDir->child('generals');
    my @suffixlist = ('.yaml', '.yml');
    my @files = $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
    return \@returnlist;
  }

  sub get_generals ($self) {
    # Get all generals from cache
    my $general_list = $self->list_generals();
    my %generals;

    foreach my $name (@$general_list) {
      my $general = $self->get_general($name);
      if ($general) {
        my $key = lc($self->normalize($name));
        $key =~ s/ /_/g;
        $generals{$key} = $general;
      }
    }

    return \%generals;
  }

  sub get_generals_by_type ($self, $type) {
    $logger->debug("get_generals_by_type called for: $type");

    # Fetch the pre-built index for this type
    my $type_key  = "by_type:$type";
    my $keys_list = $self->general_cache->get($type_key);

    unless ($keys_list && @$keys_list) {
      $logger->warn("No generals found for type: $type");
      return [];
    }

    $logger->debug(sprintf(
      'Found %d general keys for type "%s"',
      scalar(@$keys_list), $type
    ));

    # Hydrate each general from the keys list
    my @generals;
    for my $key (@$keys_list) {
      my $general = $self->get_general($key);
      if ($general) {
        push @generals, $general;
      }
      else {
        $logger->warn("Could not hydrate general for key: $key");
      }
    }

    $logger->debug(sprintf(
      'Returning %d hydrated generals for type "%s"',
      scalar(@generals), $type
    ));

    return \@generals;
  }

  sub get_available_types ($self) {
    my $types = $self->general_cache->get('available_types');
    return $types // [];
  }
}

1;
__END__

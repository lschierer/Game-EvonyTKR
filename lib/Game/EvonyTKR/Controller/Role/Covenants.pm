use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;
require List::UtilsBy;

package Game::EvonyTKR::Controller::Role::Covenants {
  use Mojo::Base -role, -signatures;
  use List::UtilsBy qw(uniq_by);
  use List::AllUtils qw(uniq none all);
  use Carp;

  has 'covenant_cache' => sub ($self) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'covenants__');
  };

  sub add_covenant ($self, $covenant) {
    my $key = lc($self->normalize($covenant->primary->name));
    $key =~ s/ /_/g;
    my $cache = $self->covenant_cache;

    # 1. Store the individual covenant data
    unless ($cache->set($key, $covenant->to_wire_hash())) {
      $self->logger->error(sprintf('failed to set key "%s"', $key));
      return;
    }

    # 2. Atomically update the list of keys using gets/cas loop
    my $result;
    my $max_attempts = 5;    # Prevents infinite loops under extreme contention

    for (my $i = 0; $i < $max_attempts; $i++) {
      my $cas_data = $cache->gets('all_covenants');
      my ($cas_id, $list_str) = (defined $cas_data) ? @$cas_data : (undef, '');

      # Use a regex to check if key is already present as a distinct entry
      if ($list_str =~ /(?:^|;)\Q$key\E(?:$|;)/) {
        $result = 1;    # Key is already in the list, we are done.
        last;
      }

      # Append the new key safely, handling the initial empty string case
      $list_str .= (length($list_str) > 0 ? ';' : '') . $key;

      if (defined $cas_id) {
        # Attempt an atomic "Compare And Swap" update
        $result = $cache->cas('all_covenants', $cas_id, $list_str);
      }
      else {
  # Key 'all_covenants' didn't exist yet, use 'add' to set it for the first time
  # 'add' is an atomic operation that only succeeds if the key doesn't exist.
        $result = $cache->add('all_covenants', $list_str);
      }

      if ($result) {
        last;    # Success, exit the loop
      }
# If result is false, another process updated the key before us.
# The loop will automatically retry by calling 'gets' again to get the new value and CAS token.
    }

    unless ($result) {
      $self->logger->error(sprintf(
        'failed to update "all_covenants" list after %d attempts for key "%s"',
        $max_attempts, $key
      ));
    }

    return $result;
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

  sub list_covenants ($self, $app //= undef) {
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

    my $covenantsDir = $collectionDir->child('covenants');
    my @suffixlist   = ('.yaml', '.yml');
    my @files =
      $covenantsDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist = uniq map { lc($self->normalize($_)) } @files;
    return \@returnlist;
  }
}

1;

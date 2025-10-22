use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Role::Common;

package Game::EvonyTKR::Role::Cache {
  use Mojo::Base -role,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role;
  use Cache::Memcached::Fast;
  use List::AllUtils qw(uniq);
  use Carp;

  our %default_options = (
    servers            => ['127.0.0.1:11211'],
    hash_namespace     => 1,
    compress_threshold => 10_000,
    compress_ratio     => 0.9,
    compress_methods   =>
      [\&IO::Compress::Gzip::gzip, \&IO::Uncompress::Gunzip::gunzip],
    utf8   => 1,
    nowait => 1,
  );

  sub default_client($self) {
    state $client = Cache::Memcached::Fast->new({
      namespace => 'EvonyTKR:',
      %default_options,
    });
    return $client;
  }

  sub create_cache($self, $options = {}) {
    unless (exists $options->{namespace}) {
      $self->logger->logcroak(
        'create_cache requires a namespace as part of its options');
      return;
    }

    return Cache::Memcached::Fast->new({ %default_options, %$options });
  }

  sub add_item($self, $key, $data, $client = undef) {
    $client //= $self->default_client;
    $client->set($key, $data);
    $client->append('_all_keys', ",$key");
  }

  sub get_all_items($self, $client = undef) {
    $client //= $self->default_client;
    my @keys = uniq split ',', ($client->get('_all_keys') // '');
    @keys = grep {length} @keys;
    return $client->get_multi(@keys);
  }

  sub set_value($self, $key, $value, $client = undef, $expiration = 0) {
    $client //= $self->default_client;
    $client->set($key, $value, $expiration);
  }

  sub get_value($self, $key, $client = undef) {
    $client //= $self->default_client;
    return $client->get($key);
  }

  sub delete_value($self, $key, $client = undef) {
    $client //= $self->default_client;
    $client->delete($key);
    my @keys = uniq split ',', ($client->get('_all_keys') // '');
    @keys = sort grep { length($_) && $_ ne $key } @keys;
    $client->set('_all_keys', join(',', @keys), 0);
  }
}
1;
__END__

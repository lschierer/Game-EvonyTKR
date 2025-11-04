use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Service::Cache {
  use Mojo::Base -base,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Cache::Memcached::Fast;
  use Sereal::Encoder;
  use Sereal::Decoder;
  use Carp;

  has 'namespace' => '';

  our $decoder = Sereal::Decoder->new();
  our $encoder = Sereal::Encoder->new({
    canonical          => 1,
    no_shared_hashkeys => 1,
    refuse_objects     => 1
  });

  sub instance ($self) {
    state $instances = {};
    my $ns_key = $self->namespace || 'default';

    unless (exists $instances->{$ns_key}) {
      $instances->{$ns_key} = Cache::Memcached::Fast->new({
        servers           => [{ address => '127.0.0.1:11211' }],
        namespace         => 'evonytkr:' . $self->namespace,
        utf8              => 1,
        serialize_methods => [
          sub { $encoder->encode(@_) },    # Custom serialization sub
          sub { $decoder->decode(@_) }     # Custom deserialization sub
        ]
      });
    }
    return $instances->{$ns_key};
  }

  sub add ($self, $key, $data) {
    my $encoded = $encoder->encode($data);
    return $self->instance->add($key, $encoded);
  }

  sub set ($self, $key, $data) {
    my $encoded = $encoder->encode($data);
    return $self->instance->set($key, $encoded);
  }

  sub get ($self, $key) {
    my $encoded = $self->instance->get($key);
    return unless defined($encoded) && length($encoded);

    $decoder->decode($encoded, my $data);
    return $data;
  }

  sub delete ($self, $key) {
    return $self->instance->delete($key);
  }
}

1;
__END__

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Service::Cache {
  use Mojo::Base -base,                           -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
  use Cache::Memcached::Fast;
  use Sereal::Encoder;
  use Sereal::Decoder;
  use Carp;

  has 'namespace' => '';

  our $decoder = Sereal::Decoder->new();
  our $encoder = Sereal::Encoder->new({
    canonical          => 1,
    no_shared_hashkeys => 1,
    freeze_callbacks   => 1,
    refuse_objects     => 1
  });

  sub instance ($self) {
    state $instances = {};
    my $ns_key = $self->namespace || 'default';

    unless (exists $instances->{$ns_key}) {
      $instances->{$ns_key} = Cache::Memcached::Fast->new({
        servers           => [{ address => '127.0.0.1:11211' }],
        namespace         => 'evonytkr:' . $self->namespace,
        utf8              => 1,    # Encode keys as UTF-8 for unicode support
        serialize_methods =>
          [sub { $encoder->encode($_[0]) }, sub { $decoder->decode($_[0]) }]
      });
    }
    return $instances->{$ns_key};
  }

  sub add ($self, $key, $data) {
    return $self->instance->add($key, $data);
  }

  sub set ($self, $key, $data) {
    return $self->instance->set($key, $data);
  }

  sub get ($self, $key) {
    return $self->instance->get($key);
  }

  sub gets ($self, $key) {
    return $self->instance->gets($key);
  }

  sub cas ($self, $key, $cas, $value) {
    return $self->instance->cas($key, $cas, $value);
  }

  sub delete ($self, $key) {
    return $self->instance->delete($key);
  }
}

1;
__END__

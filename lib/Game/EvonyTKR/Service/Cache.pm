use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

# lib/Game/EvonyTKR/Service/Cache.pm
package Game::EvonyTKR::Service::Cache {
  use Cache::Memcached::Fast;

  our $encoder = Sereal::Encoder->new({ refuse_objects => 1 });
  our $decoder = Sereal::Decoder->new();

  sub instance {
    state $memd = Cache::Memcached::Fast->new({
      servers           => [ { address => '127.0.0.1:11211' } ],
      namespace         => 'evonytkr:',
      utf8              => 1,
      serialize_methods => [
              sub { $encoder->encode(@_) }, # Custom serialization sub
              sub { $decoder->decode(@_) }  # Custom deserialization sub
          ]
    });
  }
}
1;
__END__

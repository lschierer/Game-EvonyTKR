use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

# lib/Game/EvonyTKR/Repo/Books.pm
package Game::EvonyTKR::Repo::Books::Generic {
  use Mojo::Base -base;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
  use Sereal::Encoder;
  use Sereal::Decoder;
  use Game::EvonyTKR::Model::Factory ();
  use Game::EvonyTKR::Service::Cache ();

  sub new {
    my ($class) = @_;
    my $self = bless {
      cache => Game::EvonyTKR::Service::Cache::instance(),
      enc   =>
        Sereal::Encoder->new({ freeze_callbacks => 1, refuse_objects => 1 }),
      dec => Sereal::Decoder->new,
    }, $class;
    return $self;
  }

  sub cache_key_for ($self, $name, $level) {"book__${name}_${level}"}

  sub get ($self, $name, $level) {
    my $bytes = $self->{cache}->get($self->cache_key_for($name, $level))
      or return;
    my $wire = $self->{dec}->decode($bytes);
    return Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire);
  }

  sub put ($self, $book, $ttl = 7200) {
    my $wire  = $book->to_wire_hash;
    my $bytes = $self->{enc}->encode($wire);
    my $pr    = $self->{cache}
      ->set($self->cache_key_for($book->name, $book->level) => $bytes, $ttl);
    $self->logger->debug(sprintf(
      'put result for %s was %s',
      sprintf('Level %s %s', $book->level, $book->name),
      defined($pr) ? $pr : 'undefined'
    ));
    return 1;
  }
}
1;
__END__

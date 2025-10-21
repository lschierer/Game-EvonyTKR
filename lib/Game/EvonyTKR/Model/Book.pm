use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
require Game::EvonyTKR::Util::Book;
use namespace::clean;

package Game::EvonyTKR::Model::Book {
  use Mojo::Base -base,                        -signatures;
  use Mojo::Base 'Game::EvonyTKR::Util::Book', -role;
  use Log::Any qw($log);
  use Carp;
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  has ['name', 'text'] => '';
  has 'buffs'          => sub { [] };

  my $logger = $log;

  # for backwards compatibility
  sub buff ($self) {
    return [$self->buffs->@*];
  }

  sub to_hash ($self) {
    return {
      name  => $self->name,
      buffs => $self->buffs,
    };
  }

  sub TO_JSON {
    my $self = shift;
    return JSON::PP->new->utf8(1)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
  }

  sub as_string ($self) {
    return sprintf('"%s: %s"', $self->name, $self->text);
  }

  sub concat ($self, $other, $swap = 0) {
    my $one = $swap ? $other : $self;
    my $two = $swap ? $self  : $other;
    return "$one" . "$two";
  }

  sub _isTrue ($self) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa('Game::EvonyTKR::Model::Book');
  }
}
1;
__END__

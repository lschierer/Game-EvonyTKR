package Game::EvonyTKR::Model::Base;
use v5.42.0;
use utf8::all;
require JSON::PP;
use File::FindLib 'lib';
use Mojo::Base -base,                          -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Common', -role;
use Mojo::Base 'Game::EvonyTKR::Role::Logging',  -role;
use overload
  '""'       => \&as_string,
  '.'        => \&concat,
  'bool'     => \&_isTrue,
  'fallback' => 0;

sub FREEZE ($self, $serializer) {
  return $self->to_wire_hash();
}

sub THAW ($class, $serializer, $data) {
  return $class->from_wire_hash($data);
}

sub to_hash {
  return {};
}

sub TO_JSON ($self) {
  return $self->to_hash();
}

sub as_string ($self, @args) {
  my $json =
    JSON::PP->new->utf8->pretty->allow_blessed(1)
    ->convert_blessed(1)
    ->encode($self->to_hash());
  return $json;
}

sub concat($self, $other, $swap) {
  if ($swap) {
    return $other . $self->as_string();
  }
  else {
    return $self->as_string() . $other;
  }
}

sub _isTrue ($self, $other = undef, $swap = undef) {
  return
       defined($self)
    && ref($self)
    && blessed($self)
    && $self->isa(__PACKAGE__);
}
1;
__END__

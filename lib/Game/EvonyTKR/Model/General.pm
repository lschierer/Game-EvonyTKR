use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require UUID;

package Game::EvonyTKR::Model::General {
  use Mojo::Base -base,                           -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::General', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',  -role;
  use JSON::PP;
  use UUID qw(uuid5);
  use File::FindLib 'lib';
  use Carp;
  use overload
    '""'       => \&as_string,
    'eq'       => \&equality,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  our $VERSION = 'v0.40.0';

  has 'id' => sub ($self) {
    my $general_type =
      ref($self->type) eq 'ARRAY' ? $self->type->[0] : $self->type;
    if (exists $self->UUID5_Generals->{$general_type}) {
      return uuid5($self->UUID5_Generals->{$general_type}, $self->name);
    }
    return $self->name;
  };

  has ['name', 'type', 'ascendingAttribute', 'builtInBookName',
    'builtInBook'] => undef;

  has ['specialtyNames', 'specialties'] => sub { [] };
  has 'ascending'                       => 0;
  has 'stars'                           => 'none';
  has 'basicAttributes' =>
    sub { return Game::EvonyTKR::Model::BasicAttributes->new() };

  sub to_hash ($self) {
    return {
      id              => $self->id,
      name            => $self->name,
      type            => $self->type,
      basicAttributes => $self->basicAttributes,
      ascending       => $self->ascending,
      builtInBookName => $self->builtInBookName,
      specialtyNames  => $self->specialtyNames,
    };
  }

  sub TO_JSON ($self) {
    return $self->to_hash();
  }

  sub as_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub equality($self, $other, $swap = 0) {
    my $one = $swap ? $other : $self;
    my $two = $swap ? $self  : $other;
    my $on  = '';
    my $tn  = '';
    if (ref($one) && $one->isa('Game::EvonyTKR::Model::General')) {
      $on = $one->name;
    }
    else {
      $on = "$one";
    }
    if (ref($two) && $two->isa('Game::EvonyTKR::Model::General')) {
      $tn = $two->name;
    }
    else {
      $tn = "$two";
    }
    return $on eq $tn;
  }

  sub _isTrue ($self, @args) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && blessed($self) eq __PACKAGE__;
  }

}

1;

__END__

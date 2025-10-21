use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Model::Book::Builtin {
  use Mojo::Base -base, -signatures;

  has 'name'  => '';
  has 'buffs' => sub { [] };

  sub to_hash ($self) {
    return {
      name  => $self->name,
      buffs => $self->buffs
    };
  }
}
1;

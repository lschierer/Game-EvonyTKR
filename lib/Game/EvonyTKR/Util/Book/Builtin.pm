use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Util::Book::Builtin {
  use Mojo::Base -role, -signatures;

  sub from_hash ($class, $object) {
    require Game::EvonyTKR::Model::Book::Builtin;
    return Game::EvonyTKR::Model::Book::Builtin->new(
      name  => $object->{name}  // '',
      buffs => $object->{buffs} // []
    );
  }
}
1;

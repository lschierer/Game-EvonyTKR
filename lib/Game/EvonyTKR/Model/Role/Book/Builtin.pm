use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Mojo::Base;
use namespace::autoclean;

package Game::EvonyTKR::Model::Role::Book::Builtin {
  use Mojo::Base -role, -signatures;
  use Carp;

  sub is_builtin {
    return 1;
  }
}
1;

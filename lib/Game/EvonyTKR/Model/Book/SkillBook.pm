use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Model::Book::SkillBook {
  use Mojo::Base -base, -signatures;

  has 'level' => 1;

  sub to_hash ($self) {
    return { level => $self->level };
  }
}
1;

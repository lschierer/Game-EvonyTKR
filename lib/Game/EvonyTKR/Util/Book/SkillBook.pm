use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Util::Book::SkillBook {
  use Mojo::Base -role, -signatures;

  sub from_hash ($class, $object) {
    require Game::EvonyTKR::Model::Book::SkillBook;
    return Game::EvonyTKR::Model::Book::SkillBook->new(level => $object->{level}
        // 1);
  }
}
1;

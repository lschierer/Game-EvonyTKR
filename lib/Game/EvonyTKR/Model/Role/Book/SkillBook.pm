use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Model::Role::Book::SkillBook {
  use Mojo::Base -role, -signatures;
  use Carp;

  has 'level' => 1;

  sub is_builtin {
    return 0;
  }

  sub validate_level ($self) {
    if ($self->level < 1 || $self->level > 4) {
      $self->logerror(sprintf(
        'invalid level %s, must be between 1 and 4 inclusive.',
        $self->level));
      return 0;
    }
    return 1;
  }

  sub from_hash ($class, $object) {
    require Game::EvonyTKR::Model::Book::SkillBook;
    return Game::EvonyTKR::Model::Book::SkillBook->new(level => $object->{level}
        // 1);
  }
}
1;

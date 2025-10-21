use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
use namespace::autoclean;

package Game::EvonyTKR::Model::Book::SkillBook {
  use Mojo::Base 'Game::EvonyTKR::Model::Book',           -base;
  use Mojo::Base 'Game::EvonyTKR::Util::Book::SkillBook', -role;
  use List::AllUtils qw( any none );
  use Carp;
  use File::FindLib 'lib';
  use Log::Any qw($log);
  use overload
    '""'       => \&TO_JSON,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  has 'level' = 1;

  sub to_hash ($self) {
    my $hashRef = $self->SUPER::to_hash;
    $hashRef->{level} = $self->level;
    return $hashRef;
  }
}
1;

__END__

#ABSTRACT: Model for non-builtin Skill Books that can be equipped to Generals

=pod

=head1 DESCRIPTION

SkillBooks are books that can be equipped to Generals. Unlike Builtins,
these have levels and can be upgraded.

=cut

=cut

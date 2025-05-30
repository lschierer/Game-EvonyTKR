use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;

class Game::EvonyTKR::Model::Book::SkillBook : isa(Game::EvonyTKR::Model::Book) {
# PODNAME: Game::EvonyTKR::Model::Book::SkillBook
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&TO_JSON,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $level : reader : param;

  ADJUST {
    # Validate level is between 1 and 5
    if ($level < 1 || $level > 5) {
      $self->logger()
        ->logcroak("SkillBook level must be between 1 and 5, got $level");
    }
  }

  method toHashRef {
    my $hashRef = {
      name  => $name,
      text  => $text,
      buff  => $buff,
      level => $level,
    };
    return $hashRef;
  }

  method TO_JSON {
    return $self->toHashRef();
  }

}
1;

__END__

#ABSTRACT: Model for non-builtin Skill Books that can be equipped to Generals

=pod

=head1 DESCRIPTION

SkillBooks are books that can be equipped to Generals. Unlike BuiltinBooks, 
these have levels and can be upgraded.

=cut

=cut

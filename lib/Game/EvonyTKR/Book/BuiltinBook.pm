use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Buff;

class Game::EvonyTKR::Book::Builtin : isa(Game::EvonyTKR::Book) {
# PODNAME: Game::EvonyTKR::Book::Builtin
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'        => \&TO_JSON,
    'fallback'  => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;


  method toHashRef {
    return {
      name            => $name,
      text            => $text,
      buff            => $buff,
    };
  }

  method TO_JSON {
    return $self->toHashRef();
  }

}
1;

__END__

#ABSTRACT: Model of the Books that come built-in with Generals

=pod

=head1 DESCRIPTION

Generals have books built in.  This describes/models the books.

=cut

=cut

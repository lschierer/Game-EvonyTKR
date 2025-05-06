use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Model::Book::Builtin : isa(Game::EvonyTKR::Model::Book) {
# PODNAME: Game::EvonyTKR::Model::Book
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""' => \&to_String;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $GeneralRef : reader;

}
1;

__END__

#ABSTRACT: Model of the Books that come built-in with Generals

=pod

=head1 DESCRIPTION

Generals have books built in.  This describes/models the books.

=cut

=cut

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Book;
use namespace::autoclean;

package Game::EvonyTKR::Model::Book::Builtin {
  use Mojo::Base 'Game::EvonyTKR::Model::Book',         -base;
  use Mojo::Base 'Game::EvonyTKR::Util::Book::Builtin', -role;
  use Carp;
  use File::FindLib 'lib';
  use Log::Any qw($log);
  use overload
    '""'       => \&TO_JSON,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';

  my $logger = $log;

}
1;

__END__

#ABSTRACT: Model of the Books that come built-in with Generals

=pod

=head1 DESCRIPTION

Generals have books built in.  This describes/models the books.

=cut

=cut

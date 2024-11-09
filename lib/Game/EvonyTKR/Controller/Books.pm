use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Mojo::Home;
require Data::Printer;
require Game::EvonyTKR::Data;
require Game::EvonyTKR::Model::Book;
require JSON::MaybeXS;
require YAML::PP;

package Game::EvonyTKR::Controller::Books {
  use Mojo::Base 'Mojolicious::Controller', -role, -strict, -signatures;
  use UUID qw(uuid5);
  our $VERSION = 'v0.30.0';

  my $generals = {};
  my $data = Game::EvonyTKR::Data->new();

  sub getAllBooks($self) {

  }

}
1;

__END__
#ABSTRACT: Controller in a Model-View-Controller framework for Books

=pod

=head1 DESCRIPTION

This handles both Builtin and Standard Books - the differences between them are
dwarfed by the similarities.

=cut

=cut

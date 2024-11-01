use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;


class Game::EvonyTKR::Model::Book :isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::Book
  use List::AllUtils qw( any none );
  use UUID qw(uuid5);
  use Mojo::JSON qw (encode_json);
  use Types::Common qw( -lexical -all t );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'  => \&to_String;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $name :reader :param;

  field $text :reader :param //= "Original text not available.";

  field $buff :reader :param //= [];

  method validate() {

  }

}
1;

__END__

#ABSTRACT: base class for builtin BuiltIn and Standard Skill Books.

=pod

=head1 DESCRIPTION

Books are one of the fundamental ways in which the game adds Buffs and Debuffs to Generals.

=cut

=cut

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;
use namespace::clean;

package Game::EvonyTKR::Model::Book {
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Util::Book',                          -role;
  use Log::Any qw($log);
  use Carp;
  use List::AllUtils qw( any none );
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  has ['name', 'text'] = '';
  has 'buffs' = [];

  my $logger = $log;

  # for backwards compatibility
  sub buff ($self) {
    return [$self->buffs->@*];
  }

  sub to_hash ($self) {
    return {
      name => $self->name,
      text => $self->text,
      buff => $self->buffs,
    };
  }

  sub TO_JSON {
    my $self = shift;
    return JSON::PP->new->utf8(1)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
  }

  sub as_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(1)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub concat($self, $other, $swap) {
    if ($swap) {
      return $other . $self->as_string();
    }
    else {
      return $self->as_string() . $other;
    }
  }

  sub _isTrue ($self) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa('Game::EvonyTKR::Model::Book');
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
1;

__END__

#ABSTRACT: base class for builtin BuiltIn and Standard Skill Books.

=pod

=head1 DESCRIPTION

Books are one of the fundamental ways in which the game adds Buffs and Debuffs to Generals.

=cut

=cut

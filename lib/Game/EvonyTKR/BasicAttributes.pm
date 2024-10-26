use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::BasicAttributes : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::BasicAttributes
use Croak;
use Types::Common qw( t is_Num is_Str is_Int);
use List::AllUtils qw( any none );
use Data::Printer;
use Game::EvonyTKR::BasicAttribute;
use namespace::autoclean;
# VERSION

use File::FindLib 'lib';
use overload
  '<=>' => \&_comparison,
  '=='  => \&_equality,
  '!='  => \&_inequality,
  '""'  => \&_toString;

  field $attack :reader = Game::EvonyTKR::BasicAttribute->new(
    attribute_name => 'attack',
  );

  field $leadership :reader = Game::EvonyTKR::BasicAttribute->new(
    attribute_name => 'leadershp',
  );

  field $defense :reader = Game::EvonyTKR::BasicAttribute->new(
    attribute_name => 'defense',
  );

  field $politics :reader = Game::EvonyTKR::BasicAttribute->new(
    attribute_name => 'politics',
  );

}
1;
__END__
# ABSTRACT: Stores the collection of what Evony refers to as the Basic Attributes for a Game::EvonyTKR::General

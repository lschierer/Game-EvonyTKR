use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

class Game::EvonyTKR::General::Ranged : isa(Game::EvonyTKR::General) {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
  use Util::Any -all;
  use namespace::autoclean;
  use Game::EvonyTKR::General;
  use overload
    '<=>'      => \&Game::EvonyTKR::General::_comparison,
    'cmp'      => \&Game::EvonyTKR::General::_comparison,
    'eq'       => \&Game::EvonyTKR::General::_equality,
    '=='       => \&Game::EvonyTKR::General::_equality,
    'ne'       => \&Game::EvonyTKR::General::_inequality,
    '!='       => \&Game::EvonyTKR::General::_inequality,
    '""'       => \&Game::EvonyTKR::General::_toString,
    "fallback" => 1;
# PODNAME: Game::EvonyTKR::General::Ranged

# ABSTRACT: Module for processing information about Evony TKR Ground Specialists.

=head1 DESCRIPTION

=for Out of all Game::EvonyTKR::General instances, some are specialize in enhancing Ground Troops.  The in-game notation for which Generals specialize
in which ways is not always entirely reliable, and these modules will, at times, and for particular generals, deviate from it.  Rather, it will rely on the data files provided.

=cut

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $_generalType : reader = 'Ranged Troops';

}

1;

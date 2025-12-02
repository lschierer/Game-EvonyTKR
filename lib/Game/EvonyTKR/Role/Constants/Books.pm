package Game::EvonyTKR::Role::Constants::Books;
use v5.42.0;
use utf8::all;
require Data::Printer;
require Hash::Util;
use List::AllUtils qw( all any none );
use Mojo::Base -role, -signatures;
use Const::Fast;
use Carp;

# a *best* book will always be a level 4 book,
# as level 4 is the highest.
has bestLevel => 4;

has 'BestSkillBooks' => sub {
  my $self      = shift;
  my $bestLevel = $self->bestLevel;
  const my $tmp => {
    ground_specialist => {
      default => {
        "Level $bestLevel Ground Troop Attack"       => 1,
        "Level $bestLevel March Size"                => 2,
        "Level $bestLevel Ground Troop HP"           => 3,
        "Level $bestLevel Ground Troop Defense"      => 4,
        "Level $bestLevel Siege Machine Range Bonus" => 5,
        "Level $bestLevel Ranged Troop Range Bonus"  => 6,
        "Level $bestLevel Ranged Troop Attack"       => 7,
        "Level $bestLevel Ranged Troop HP"           => 8,
        "Level $bestLevel Ranged Troop Defense"      => 9,
        "Level $bestLevel Siege Machine Attack"      => 10,
      },
      PvM => {
        "Level $bestLevel Ground Troop Attack Against Monster"  => 1,
        "Level $bestLevel Ground Troop Attack"                  => 2,
        "Level $bestLevel March Size"                           => 3,
        "Level $bestLevel Ground Troop HP Against Monster"      => 4,
        "Level $bestLevel Ground Troop HP"                      => 5,
        "Level $bestLevel Ground Troop Defense Against Monster" => 6,
        "Level $bestLevel Ground Troop Defense"                 => 7,
        "Level $bestLevel Luck"                                 => 8,
      }
    },
    mounted_specialist => {
      default => {
        "Level $bestLevel Mounted Troop Attack"      => 1,
        "Level $bestLevel March Size"                => 2,
        "Level $bestLevel Mounted Troop HP"          => 3,
        "Level $bestLevel Mounted Troop Defense"     => 4,
        "Level $bestLevel Siege Machine Range Bonus" => 5,
        "Level $bestLevel Ranged Troop Range Bonus"  => 6,
        "Level $bestLevel Ground Troop Attack"       => 7,
        "Level $bestLevel Ground Troop HP"           => 8,
        "Level $bestLevel Ground Troop Defense"      => 9,
        "Level $bestLevel Siege Machine Attack"      => 10,
      },
      PvM => {
        "Level $bestLevel Mounted Troop Attack Against Monster"  => 1,
        "Level $bestLevel Mounted Troop Attack"                  => 2,
        "Level $bestLevel March Size"                            => 3,
        "Level $bestLevel Mounted Troop HP Against Monster"      => 4,
        "Level $bestLevel Mounted Troop HP"                      => 5,
        "Level $bestLevel Mounted Troop Defense Against Monster" => 6,
        "Level $bestLevel Mounted Troop Defense"                 => 7,
        "Level $bestLevel Luck"                                  => 8,
      },
    },
    ranged_specialist => {
      default => {
        "Level $bestLevel Ranged Troop Attack"       => 1,
        "Level $bestLevel March Size"                => 2,
        "Level $bestLevel Ranged Troop HP"           => 3,
        "Level $bestLevel Ranged Troop Defense"      => 4,
        "Level $bestLevel Ranged Troop Range Bonus"  => 5,
        "Level $bestLevel Siege Machine Range Bonus" => 6,
        "Level $bestLevel Mounted Troop Attack"      => 7,
        "Level $bestLevel Mounted Troop HP"          => 8,
        "Level $bestLevel Mounted Troop Defense"     => 9,
        "Level $bestLevel Siege Machine Attack"      => 10,
      },
      PvM => {
        "Level $bestLevel Ranged Troop Attack Against Monster"  => 1,
        "Level $bestLevel Ranged Troop Attack"                  => 2,
        "Level $bestLevel March Size"                           => 3,
        "Level $bestLevel Ranged Troop Range Bonus"             => 4,
        "Level $bestLevel Ranged Troop HP Against Monster"      => 5,
        "Level $bestLevel Ranged Troop HP"                      => 6,
        "Level $bestLevel Ranged Troop Defense Against Monster" => 7,
        "Level $bestLevel Ranged Troop Defense"                 => 8,
        "Level $bestLevel Luck"                                 => 9,
      },
    },
    siege_specialist => {
      default => {
        "Level $bestLevel Siege Machine Attack"      => 1,
        "Level $bestLevel March Size"                => 2,
        "Level $bestLevel Siege Machine HP"          => 3,
        "Level $bestLevel Siege Machine Defense"     => 4,
        "Level $bestLevel Siege Machine Range Bonus" => 5,
        "Level $bestLevel Ranged Troop Range Bonus"  => 6,
        "Level $bestLevel Ranged Troop Attack"       => 7,
        "Level $bestLevel Ranged Troop HP"           => 8,
        "Level $bestLevel Ranged Troop Defense"      => 9,
        "Level $bestLevel Mounted Troop Attack"      => 10,
      },
      PvM => {
        "Level $bestLevel March Size"                => 1,
        "Level $bestLevel Siege Machine Attack"      => 2,
        "Level $bestLevel Siege Machine Range Bonus" => 3,
        "Level $bestLevel Siege Machine HP"          => 4,
        "Level $bestLevel Siege Machine Defense"     => 5,
        "Level $bestLevel Luck"                      => 6,
      }
    },
    #mayor => {
    #  default => {
    #
    #  }
    #},
    wall => {
      default => {
        "Level $bestLevel Siege Machine Attack"      => 1,
        "Level $bestLevel Mounted Troop Attack"      => 2,
        "Level $bestLevel Ranged Troop Attack"       => 3,
        "Level $bestLevel Siege Machine Range Bonus" => 4,
        "Level $bestLevel Ground Troop Attack"       => 5,
        "Level $bestLevel Ranged Troop Range Bonus"  => 6,
        "Level $bestLevel Siege Machine HP"          => 7,
        "Level $bestLevel Mounted Troop HP"          => 8,
        "Level $bestLevel Ranged Troop HP"           => 9,
        "Level $bestLevel Ground Troop HP"           => 10,
      }
    }
  };
  return $tmp;
};

1;
__END__

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

package Game::EvonyTKR::Role::Constants::Specialties {
  use Moo::Role;
  use Const::Fast;
  use Carp;

  const our %SpecialtyLevels => (
    none   => 'None',
    green  => 'Green',
    blue   => 'Blue',
    purple => 'Purple',
    orange => 'Orange',
    gold   => 'Gold',
  );

  sub is_valid_specialty_level ($self, $level) {
    if (exists $SpecialtyLevels{$level}) {
      return 1;
    }
    return 0;
  }

  has SpecialtyLevelValues => (
    is      => 'ro',
    lazy    => 1,
    default => sub {
      const my $tmp => ['none', 'green', 'blue', 'purple', 'orange', 'gold',];
      return $tmp;
    }
  );

  has SpecialtyLevelLabels => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my $labels = [];
      foreach my $key ($self->SpecialtyLevelValues->@*) {
        push @{$labels}, $SpecialtyLevels{$key};
      }
      return $labels;
    }
  );

  has CommonSpecialtyNames => (
    is      => 'ro',
    lazy    => 1,
    default => sub {
      const my $tmp => [
        'Ambush',
        'Annihilation',
        'Bash',
        'Formation',
        'Fortune',
        'Ground Troop Ares',
        'Ground Troop Assault',
        'Ground Troop Defense',
        'Ground Troop Formation',
        'Hunter',
        'Iron Warrior',
        "King's Ambition",
        'Mounted Troop Ares',
        'Mounted Troop Assault',
        'Mounted Troop Defense',
        'Mounted Troop Formation',
        'Pacify',
        'Ranged Troop Ares',
        'Ranged Troop Assault',
        'Ranged Troop Defense',
        'Ranged Troop Formation',
        'Ruler',
        'Sabotage',
        'Siege',
        'Siege Machine Ares',
        'Siege Machine Assault',
        'Siege Machine Defense',
        'Siege Machine Formation',
        'Snipe',
        'Strike',
        'Suppress',
        'Trapping',
        'Unparalleled Leader',
        'War God',
      ];
      return $tmp;
    }
  );
}
1;
__END__

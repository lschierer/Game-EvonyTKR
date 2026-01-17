use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::BuffConstants {
  use Moo::Role;
  use Const::Fast;
  use List::AllUtils qw( first );
  use Carp;

  has TroopTypeValues => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        ground  => "Ground Troops",
        mounted => "Mounted Troops",
        ranged  => "Ranged Troops",
        siege   => "Siege Machines",
      };
      return $hash;
    },
  );

  sub string_to_trooptype ($self, $string) {
    $string =~ s/_/ /g;
    $string =~ s/(\w+)(?: .*)/\L$1/x;
    my $key = first { $_ =~ /^$string/ix } keys %{ $self->TroopTypeValues };
    if (exists $self->TroopTypeValues->{$key}) {
      return $self->TroopTypeValues->{$key};
    }
    else {
      # return '' to indicate $string is not
      # reflective of a troop type
      return '';
    }
  }

  has BuffActivationValues => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        'Overall'     => 1,
        'PvM'         => 1,
        'Attacking'   => 1,
        'Reinforcing' => 1,
        'Defense'     => 1,
        'In City'     => 1,
        'Out City'    => 1,
        'Wall'        => 1,
        'Mayor'       => 1,
        'Officer'     => 1,
      };
      return $hash;
    },
  );

  has AllowedBuffActivationValues => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      return [sort keys $self->BuffActivationValues->%*];
    },
  );

  has attributeValues => (
    is      => 'ro',
    default => sub {
      const my $tmp => {
        'Attack'                     => 1,
        'Construction Speed'         => 1,
        'Death to Soul'              => 1,
        'Death to Survival'          => 1,
        'Death to Wounded'           => 1,
        'Defense'                    => 1,
        'Deserter Capacity'          => 1,
        'Double Items Drop Rate'     => 1,
        'Healing Speed'              => 1,
        'Hospital Capacity'          => 1,
        'HP'                         => 1,
        'Leadership'                 => 1,
        'March Size'                 => 1,
        'Marching Speed'             => 1,
        'Range'                      => 1,
        'Politics'                   => 1,
        'Rally Capacity'             => 1,
        'Resources Production'       => 1,
        'Stamina cost'               => 1,
        'SubCity Construction Speed' => 1,
        'SubCity Death to Survival'  => 1,
        'SubCity Gold Production'    => 1,
        'SubCity Training Capacity'  => 1,
        'SubCity Training Speed'     => 1,
        'SubCity Troop Capacity'     => 1,
        'Training Capacity'          => 1,
        'Trap Triggering Rate'       => 1,
        'Training Speed'             => 1,
        'Wounded to Death'           => 1,
      };
      return $tmp;
    },
  );

  has AttributeValues => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my @av;
      push @av, sort keys $self->attributeValues->%*;
      $self->logger->debug(
        sprintf('there are %s attribute values', scalar @av));
      return \@av;
    },
  );

  sub basicAttributeTypes {
    const my $tmp => {
      'attack'     => 1,
      'defense'    => 1,
      'politics'   => 1,
      'leadership' => 1,
    };
    return $tmp;
  }

  sub BasicAttributeTypes ($self) {
    return sort keys $self->basicAttributeTypes->%*;
  }

  has BuffConditionValues => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        'Against Monsters' => {
          "Overall"     => 0,
          "PvM"         => 1,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'Attacking' => {
          "Overall"     => 0,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 1,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'brings a dragon' => {
          "Overall"     => 1,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 1,
          "Defense"     => 1,
          "In City"     => 1,
          "Out City"    => 1,
          "Wall"        => 1,
          "Mayor"       => 1,
          "Officer"     => 0,
        },
        'brings a sacred dragon' => {
          "Overall"     => 1,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 1,
          "Defense"     => 1,
          "In City"     => 1,
          "Out City"    => 1,
          "Wall"        => 1,
          "Mayor"       => 1,
          "Officer"     => 0,
        },
        'brings a spiritual beast' => {
          "Overall"     => 1,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 1,
          "Defense"     => 1,
          "In City"     => 1,
          "Out City"    => 1,
          "Wall"        => 1,
          "Mayor"       => 1,
          "Officer"     => 0,
        },
        'Defending' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 1,
          "Defense"     => 1,
          "In City"     => 1,
          "Out City"    => 1,
          "Wall"        => 1,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'During SvS' => {
          "Overall"     => 1,
          "PvM"         => 0,
          "Attacking"   => 1,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'In City' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 1,
          "Defense"     => 0,
          "In City"     => 1,
          "Out City"    => 0,
          "Wall"        => 1,
          "Mayor"       => 1,
          "Officer"     => 0,
        },
        'In Main City' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 1,
          "Defense"     => 0,
          "In City"     => 1,
          "Out City"    => 0,
          "Wall"        => 1,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'leading the army' => {
          "Overall"     => 1,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 1,
          "Defense"     => 1,
          "In City"     => 1,
          "Out City"    => 1,
          "Wall"        => 1,
          "Mayor"       => 1,
          "Officer"     => 0,
        },
        'Marching' => {
          "Overall"     => 1,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 1,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 1,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'Reinforcing' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 1,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 1,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'When City Mayor for this SubCity' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 1,
          "Officer"     => 0,
        },
        'When Defending Outside The Main City' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 1,
          "Defense"     => 1,
          "In City"     => 0,
          "Out City"    => 1,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'When Rallying' => {
          "Overall"     => 1,
          "PvM"         => 1,
          "Attacking"   => 1,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'When The Main Defense General' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 1,
          "Out City"    => 0,
          "Wall"        => 1,
          "Mayor"       => 0,
          "Officer"     => 0,
        },
        'you own the General' => {
          "Overall"     => 1,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 1,
        },
        # Officer positions 'When Appointed as Hospital Officer',
        'When Appointed as Prison Officer' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 1,
        },
        'When Appointed as Workshop Officer' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 1,
        },
        'When Appointed as Academy Officer' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 1,
        },
        'When Appointed as Embassy Officer' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 1,
        },
        'When Appointed as Barracks Officer' => {
          "Overall"     => 0,
          "PvM"         => 0,
          "Attacking"   => 0,
          "Reinforcing" => 0,
          "Defense"     => 0,
          "In City"     => 0,
          "Out City"    => 0,
          "Wall"        => 0,
          "Mayor"       => 0,
          "Officer"     => 1,
        },
      };
      return $hash;
    },
  );

  has DebuffConditionValues => (
    is      => 'ro',
    default => sub {
      const my $array => ['Enemy', 'Monsters'];
      return $array;
    },
  );

  has MappedAttributeNames => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        'Attack Range'                   => 'Range',
        'death into wounded rate'        => 'Death to Wounded',
        'death-turning-wounded rate'     => 'Death to Wounded',
        'death into survival rate'       => 'Death to Survival',
        'march size increase'            => 'March Size',
        'march size capacity'            => 'March Size',
        'marching size capacity'         => 'March Size',
        'march size'                     => 'March Size',
        'the march size'                 => 'March Size',
        'march speed'                    => 'Marching Speed',
        'march time reduction'           => 'Marching Speed',
        'training'                       => 'Training Speed',
        'wounded into death rate'        => 'Wounded to Death',
        'wounded into death'             => 'Wounded to Death',
        'mayor training speed'           => 'SubCity Training Speed',
        'mayor death into survival rate' => 'SubCity Death to Survival',
        'the gold production speed'      => 'SubCity Gold Production',
        "traps’ triggering rate"         => 'Trap Triggering Rate',
        'Trap Triggering Chance'         => 'Trap Triggering Rate',
        'subordinate city troops’ death to survival rate' =>
          'SubCity Death to Survival',
        'subordinate city troops’ death to survival' =>
          'SubCity Death to Survival',
        'subordinate city troops’ death into survival rate' =>
          'SubCity Death to Survival',
        'subordinate city troops’ death into survival' =>
          'SubCity Death to Survival',

      };
      return $hash;
    },
  );

  has MappedConditionNames => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        'In-Rally'                 => "When Rallying",
        'in rally'                 => 'When Rallying',
        'Reduces Monster'          => "Monsters",               # debuff version
        'brings any dragon'        => 'brings a dragon',
        'brings any sacred dragon' => 'brings a sacred dragon',
        'brings dragon'            => 'brings a dragon',
        'with dragon'              => 'brings a dragon',
        'with any dragon'          => 'brings a dragon',
        'with any sacred dragon'   => 'brings a sacred dragon',
        'brings any spiritual beast'    => 'brings a spiritual beast',
        'brings spiritual beast'        => 'brings a spiritual beast',
        'to attack Monsters'            => 'Against Monsters',    # buff version
        'to attack'                     => 'Attacking',
        'to reinforce'                  => 'Reinforcing',
        'the Mayor'                     => "When City Mayor for this SubCity",
        'in this subordinate city'      => "When City Mayor for this SubCity",
        'in subordinate city'           => "When City Mayor for this SubCity",
        'launching Alliance War'        => 'When Rallying',
        'attacking Monsters'            => 'Against Monsters',
        'in-city'                       => 'In City',
        'from Monsters'                 => 'Against Monsters',
        'the main city defense general' => 'When The Main Defense General',

      };
      return $hash;
    },
  );
}
1;
__END__

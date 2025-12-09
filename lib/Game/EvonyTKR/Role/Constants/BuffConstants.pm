use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::BuffConstants {
  use Mojo::Base -role, -signatures;
  use Const::Fast;
  use List::AllUtils qw( first );
  use Carp;

  has 'TroopTypeValues' => sub {
    const my $hash => {
      ground  => "Ground Troops",
      mounted => "Mounted Troops",
      ranged  => "Ranged Troops",
      siege   => "Siege Machines",
    };
    return $hash;
  };

  sub string_to_trooptype ($self, $string) {
    $string =~ s/_/ /g;
    $string =~ s/(\w+)(?: .*)/\L$1/x;
    my $key = first { $_ =~ /^$string/x } keys %{ $self->TroopTypeValues };
    if (exists $self->TroopTypeValues->{$key}) {
      return $self->TroopTypeValues->{$key};
    }
    else {
      # return '' to indicate $string is not
      # reflective of a troop type
      return '';
    }
  }

  has 'BuffActivationValues' => sub {
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
  };

  has 'AllowedBuffActivationValues' => sub ($self) {
    return [sort keys $self->BuffActivationValues->%*];
  };

  has 'attributeValues' => sub {
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
  };

  has 'AttributeValues' => sub ($self) {
    my @av;
    push @av, sort keys $self->attributeValues->%*;
    $self->log_debug(sprintf('there are %s attribute values', scalar @av));
    return \@av;
  };

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

  has 'BuffConditionValues' => sub {
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
  };

  has 'DebuffConditionValues' => sub {
    const my $array => ['Enemy', 'Monsters'];
    return $array;
  };
}
1;
__END__

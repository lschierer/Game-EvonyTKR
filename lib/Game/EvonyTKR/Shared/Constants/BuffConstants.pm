use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Shared::Constants::BuffConstants {
  use Mojo::Base -role, -signatures;
  use Const::Fast;
  use Carp;

  const our %TroopTypeValues => (
    ground  => "Ground Troops",
    mounted => "Mounted Troops",
    ranged  => "Ranged Troops",
    siege   => "Siege Machines",
  );

  sub string_to_trooptype ($self, $string) {
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

  const our $BuffActivationValues => (
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
  );

  sub AllowedBuffActivationValues ($self) {
    return sort keys $self->BuffActivationValues->%*;
  }

  const our attributeValues => (
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
  );

  sub AttributeValues ($self) {
    return sort keys $self->attributeValues->%*;
  }

  const our basicAttributeTypes => (
    'attack'     => 1,
    'defense'    => 1,
    'hp'         => 1,
    'leadership' => 1,
  );

  sub BasicAttributeTypes ($self) {
    return sort keys $self->basicAttributeTypes->%*;
  }

  const our $BuffConditionValues = {
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

  const our $DebuffConditionValues = ['Enemy', 'Monsters'];

}
1;
__END__

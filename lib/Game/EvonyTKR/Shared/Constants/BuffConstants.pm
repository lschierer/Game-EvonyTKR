use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Shared::Constants::BuffConstants {
  use Mojo::Base -role, -signatures;
  use Constant::Fast;
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

  const our BuffActivationValues => (
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

}
1;
__END__

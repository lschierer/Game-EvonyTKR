use experimental 'class';
use utf8::all;
use File::FindLib 'lib';

class Game::EvonyTKR::Data
  : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Data
  use Carp;
  use Types::Standard        qw(is_Int Int Str is_Str);
  use Types::Common::Numeric qw(PositiveOrZeroInt);
  use Type::Utils            qw(is enum);
  use File::ShareDir ':ALL';
  use YAML::XS;
  use X500::RDN;
  use X500::DN;
  use UUID qw(uuid5);
  use namespace::autoclean;
# VERSION
  use File::FindLib 'lib';

  field @buffAttributes :reader = (
    "Attack",
    "Attack Speed",
    "Death to Survival",
    "Death to Soul",
    "Death to Wounded",
    "Defense",
    "Deserter Capacity",
    "Double Items Drop Rate",
    "HP",
    "Healing Speed",
    "Hospital Capacity",
    "Leadership",
    "Load",
    "March Size Capacity",
    "March Time",
    "Marching Speed",
    "Marching Speed to Monsters",
    "MonstersAttack",
    "Politics",
    "Rally Capacity",
    "Range",
    "Resources Production",
    "Stamina cost",
    "SubCity Construction Speed",
    "SubCity Gold Production",
    "SubCity Training Speed",
    "SubCity Troop Capacity",
    "Training Capacity",
    "Training Speed",
    "Wounded to Death",
  );

  field @buffConditions : reader = (
    'Against Monsters',
    'Attacking',
    'Defending',
    'During SvS',
    'In City',
    'In Main City',
    'Marching',
    'Reinforcing',
    'When City Mayor',
    'When City Mayor for this SubCity',
    'When Defending Outside The Main City',
    'When Rallying',
    'When The Main Defense General',
    'When an officer',
    'brings a dragon',
    'brings dragon or beast to attack',
    'dragon to the attack',
    'leading the army to attack',
  );

  field @debuffConditions : reader = (
    'Enemy',
    'Enemy In City',
    'Reduces Enemy',
    'Reduces Enemy in Attack',
    'Reduces Enemy with a Dragon',
    'Reduces',
  );

  field @BuffAttributes : reader;

  method set_BuffAttributes {
    my $data_location = dist_file('Game-EvonyTKR', 'buff/attributes.yaml');
    open(my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml;
    close $DATA;
    @BuffAttributes = @{ $data->{'attributes'} };
  }

  method AllConditions() {
    return (@buffConditions, @debuffConditions);
  }

  field @BuffClasses : reader = (
    'Ground Troops',
    'Mounted Troops',
    'Ranged Troops',
    'Siege Machines',
    'All Troops',
    'Monsters',
  );

  field @GeneralKeys : reader = (
    'Ground Troops',
    'Mayor',
    'Mounted Troops',
    'Officer',
    'Ranged Troops',
    'Siege Machines',
    'Wall',
  );

  #keys should come from @GeneralKeys above.
  field %generalClass : reader = (
    'ground_specialist'   => 'Game::EvonyTKR::General::Ground',
    'mounted_specialist'  => 'Game::EvonyTKR::General::Mounted',
    'ranged_specialist'   => 'Game::EvonyTKR::General::Ranged',
    'siege_specialist'    => 'Game::EvonyTKR::General::Siege',
  );

  field $specialityLevels : reader =
    enum [qw( None Green Blue Purple Orange Gold)];

  field $globalDN : reader = new X500::DN(
    new X500::RDN('OU' => 'EvonyTKR'),
    new X500::RDN('OU' => 'Game'),
    new X500::RDN('OU' => 'module'),
    new X500::RDN('dc' => 'Perl'),
    new X500::RDN('dc' => 'org'),
  );

  field $UUID5_base : reader;

  ADJUST {
    my $ns_base = uuid5(dns => 'perl.org');
    $UUID5_base = uuid5($ns_base, $globalDN->getX500String());
  }

}
1;

__END__

# ABSTRACT: Runtime Data values for Game::EvonyTKR

=method new()

instantiate the shared data helper functions.
=cut

=head1 DESCRIPTION

Due to the encapsulation and initialization order requirements, even if the perlclass feature had already implemented the :common attribute, things marked as common would not be initialized in time for other parameters to validate against them.  Thus I need a ::Data class that users can initialize first.

=cut

=method buffConditions

returns those conditions that are only relevant to Buffs.
=cut

=method debuffConditions

returns those conditions that are only relevant to Debuffs.
=cut

=method BuffAttributes

Returns an array of possible attributes such that each buff will have exactly one attribute from this list of possible Attributes.  This list is effectively attempting to replace having an enum.
=cut

=method AllConditions

A buff will have one or more conditions from this list of Conditioons.  This list is effectively attempting to replace having an emum.

This includes both Buff and Debuff conditions. see buffConditions() and debuffConditions().
=cut

=method BuffClasses

A *buff* will affect either exactly one or All classes of troops. This is attempting to replace having an enum.

A *General* can have more than one Class from this list.  That is because General Classes and Buff classes are the same values, but used totally differently.
=cut

=method set_BuffAttributes()

get the possible attributes from the yaml data file in the shared directory and load them into memory.
=cut

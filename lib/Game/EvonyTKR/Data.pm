use experimental 'class';
use utf8::all;
use File::FindLib 'lib';

class Game::EvonyTKR::Data
  : isa(Game::EvonyTKR::Logger) {
  use Carp;
  use Types::Standard        qw(is_Int Int Str is_Str);
  use Types::Common::Numeric qw(PositiveOrZeroInt);
  use Type::Utils "is";
  use File::ShareDir ':ALL';
  use YAML::XS;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Data
# VERSION
  use File::FindLib 'lib';

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

=method set_BuffAttributes()

get the possible attributes from the yaml data file in the shared directory and load them into memory.
=cut

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

  field %generalClass :reader = (
    'Ground'  => 'Game::EvonyTKR::General::Ground',
    'Mounted' => 'Game::EvonyTKR::General::Mounted',
    'Ranged'  => 'Game::EvonyTKR::General::Ranged',
    'Siege'   => 'Game::EvonyTKR::General::Siege',
  );
  

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

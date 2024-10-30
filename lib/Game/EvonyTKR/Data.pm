use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Types::Standard        qw(is_Int Int Str is_Str);
use Types::Common::Numeric qw(PositiveOrZeroInt);
use Type::Utils            qw(is enum);
use File::ShareDir ':ALL';
use YAML::PP;
use X500::RDN;
use X500::DN;
use UUID qw(uuid5);
use namespace::autoclean;

class Game::EvonyTKR::Data : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Data
  use Carp;
  use File::FindLib 'lib';
  our $VERSION = 'v0.30.0';
  my $debug = 0;

  field @buffAttributes : reader = (
    "attack",
    "attack speed",
    "death to survival",
    "death to soul",
    "death to wounded",
    "defense",
    "deserter capacity",
    "double items drop rate",
    "hp",
    "healing speed",
    "hospital capacity",
    "leadership",
    "load",
    "march size capacity",
    "march time",
    "marching speed",
    "marching speed to monsters",
    "monstersattack",
    "politics",
    "rally capacity",
    "range",
    "resources production",
    "stamina cost",
    "subcity construction speed",
    "subcity gold production",
    "subcity training speed",
    "subcity troop capacity",
    "training capacity",
    "training speed",
    "wounded to death",
  );

  field @buffConditions : reader = (
    'against monsters',
    'attacking',
    'defending',
    'during svs',
    'in city',
    'in main city',
    'marching',
    'reinforcing',
    'when city mayor',
    'when city mayor for this subcity',
    'when defending outside the main city',
    'when rallying',
    'when the main defense general',
    'when an officer',
    'brings a dragon',
    'brings dragon or beast to attack',
    'dragon to the attack',
    'leading the army to attack',
  );

  field @debuffConditions : reader = (
    'monster', 'enemy',
    'enemy in city',
    'reduces enemy',
    'reduces enemy in attack',
    'reduces enemy with a dragon', 'reduces',
  );

  field @BuffAttributes : reader = (qw(
    'Attack',
    'Attack Speed',
    'Death to Survival',
    'Death to Soul',
    'Death to Wounded',
    'Defense',
    'Deserter Capacity',
    'Double Items Drop Rate',
    'HP',
    'Healing Speed',
    'Hospital Capacity',
    'Leadership',
    'Load',
    'March Size Capacity',
    'March Time',
    'Marching Speed',
    'Politics',
    'Rally Capacity',
    'Range',
    'Resources Production',
    'Stamina cost',
    'SubCity Construction Speed',
    'SubCity Gold Production',
    'SubCity Training Speed',
    'SubCity Troop Capacity',
    'Training Capacity',
    'Training Speed',
    'Wounded to Death',
  ));

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

  field @GeneralKeys : reader = (qw(
    ground_specialist
    mounted_specialist
    ranged_specialist
    siege_specialist
    mayor
    officer
    wall
  ));

  #keys should come from @GeneralKeys above.
  field %generalClass : reader = (
    'ground_specialist'  => 'Game::EvonyTKR::General::Ground',
    'mounted_specialist' => 'Game::EvonyTKR::General::Mounted',
    'ranged_specialist'  => 'Game::EvonyTKR::General::Ranged',
    'siege_specialist'   => 'Game::EvonyTKR::General::Siege',
  );

  field $specialityLevels : reader =
    enum [qw( none green blue purple orange gold)];

  field $globalDN : reader = new X500::DN(
    new X500::RDN('OU' => 'EvonyTKR'),
    new X500::RDN('OU' => 'Game'),
    new X500::RDN('OU' => 'module'),
    new X500::RDN('dc' => 'Perl'),
    new X500::RDN('dc' => 'org'),
  );

  field $UUID5_base : reader;

  field $UUID5_Generals : reader = {};

  ADJUST {
    $self->logger()->info('final adjust block for Game::EvonyTKR::Data');
    my $ns_base = uuid5(dns => 'perl.org');
    $UUID5_base = uuid5($ns_base, $globalDN->getX500String());
    my $UUID5_Generals_base = uuid5($UUID5_base, 'Generals');
    for my $k ($self->GeneralKeys()) {
      $UUID5_Generals->{$k} = uuid5($UUID5_Generals_base, $k);
      if ($debug) {
        $self->logger()->trace("base for $k is " . $UUID5_Generals->{$k});
      }

    }
  }

}
1;

__END__

# ABSTRACT: Runtime Data values for Game::EvonyTKR

=pod

=head1 DESCRIPTION

Due to the encapsulation and initialization order requirements, even if the perlclass feature had already implemented the :common attribute, things marked as common would not be initialized in time for other parameters to validate against them.  Thus I need a ::Data class that users can initialize first.

=cut

=head1 METHODS

=method new()

instantiate the shared data helper functions.

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

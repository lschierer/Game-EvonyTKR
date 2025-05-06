use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use namespace::autoclean;

class Game::EvonyTKR::Data : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Data
  require Type::Tiny::Enum;
  use Types::Common qw( -lexical -all t);
  use UUID          qw(uuid5);
  use namespace::autoclean;
  use File::FindLib 'lib';
  use X500::DN;
  use X500::RDN;
  our $VERSION = 'v0.30.0';
  my $debug = 0;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }

  }

  field $AttributeNames : reader = Type::Tiny::Enum->new(
    values => [qw(
      attack
      defense
      leadership
      politics
    )]
  );

  field $allowedBuffActivation : reader = Type::Tiny::Enum->new(
    values => [
      "Overall", "PvM",     "Attacking", "Reinforcing",
      "Defense", "In City", "Out City",  "Wall",
      "Mayor",   "Officer",
    ]
  );

  field $buffAttributeValues : reader = Type::Tiny::Enum->new(
    values => [
      "Attack",
      "Death to Soul",
      "Death to Survival",
      "Death to Wounded",
      "Defense",
      "Deserter Capacity",
      "Double Items Drop Rate",
      "HP",
      "Hospital Capacity",
      "March Size Capacity",
      "Marching Speed to Monsters",
      "Marching Speed",
      "Rally Capacity",
      "Resources Production",
      "Stamina cost",
      "SubCity Construction Speed",
      "SubCity Gold Production",
      "SubCity Training Speed",
      "SubCity Troop Capacity",
      "Training Capacity",
      "Training Speed",
      "Wounded to Death",
    ]
  );

  field $buffConditionValues : reader = Type::Tiny::Enum->new(
    values => [
      "Against Monsters",
      "Attacking",
      "brings a dragon",
      "brings dragon or beast to attack",
      "Defending",
      "dragon to the attack",
      "leading the army to attack",
      "Marching",
      "Reinforcing",
      "When City Mayor for this SubCity",
      "When Defending Outside The Main City",
      "When Rallying",
      "In Main City",
      "When the Main Defense General",
    ]

  );

  field $debuffConditionValues : reader = Type::Tiny::Enum->new(
    values => [
      "Enemy",
      "Enemy In City",
      "Reduces",
      "Reduces Enemy",
      "Reduces Enemy in Attack",
      "Reduces Enemy with a Dragon",
      "Reduces Monster",
    ]

  );

  field $bookConditionValues : reader =
    Type::Tiny::Enum->new(values => ["all the time", "when not mine"]);

  method AllConditions() {
    my %seen;
    my $allowedConditions = grep { not $seen{$_}++ } (
      @{ $buffConditionValues->values },
      @{ $debuffConditionValues->values },
      @{ $bookConditionValues->values }
    );
    return $allowedConditions;
  }

  field $targetedTypeValues : reader = Type::Tiny::Enum->new(
    values => [
      'Ground Troops',
      'Mounted Troops',
      'Ranged Troops',
      'Siege Machines',
      'All Troops',
      'Monsters',
    ]
  );

  field $GeneralKeys : reader = Type::Tiny::Enum->new(
    values => [qw(
      ground_specialist
      mounted_specialist
      ranged_specialist
      siege_specialist
      mayor
      officer
      wall
    )]
  );

  field $allowedValueUnits : reader =
    Type::Tiny::Enum->new(values => [qw( flat percentage )]);

  field $specialityLevels : reader =
    Type::Tiny::Enum->new(values => [qw( none green blue purple orange gold)]);

  field $globalDN : reader = X500::DN->new(
    X500::RDN->new('OU' => 'EvonyTKR'),
    X500::RDN->new('OU' => 'Game'),
    X500::RDN->new('OU' => 'module'),
    X500::RDN->new('dc' => 'Perl'),
    X500::RDN->new('dc' => 'org'),
  );

  field $UUID5_base : reader;

  field $UUID5_Generals : reader = {};

  ADJUST {
    my $ns_base = uuid5(dns => 'perl.org');
    $UUID5_base = uuid5($ns_base, $globalDN->getX500String());
    my $UUID5_Generals_base = uuid5($UUID5_base, 'Generals');
    for my $k (@{ $self->GeneralKeys()->values() }) {
      $UUID5_Generals->{$k} = uuid5($UUID5_Generals_base, $k);
      $self->logger()->trace("base for $k is " . $UUID5_Generals->{$k});

    }
  }

  method toHashRef {
    return {};
  }

  method TO_JSON {
      return $self->toHashRef();
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

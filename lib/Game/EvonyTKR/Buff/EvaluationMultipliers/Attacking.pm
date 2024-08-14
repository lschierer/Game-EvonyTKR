use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

class Game::EvonyTKR::Buff::EvaluationMultipliers::Attacking :isa(Game::EvonyTKR::Buff::EvaluationMultipliers) {
  use Carp;
  use Data::Printer;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils qw(is enum);
  use Util::Any -all;
  use Game::EvonyTKR::Buff::Data;
  use namespace::autoclean;

# PODNAME: Game::EvonyTKR::Buff::EvaluationMultipliers::Attacking

# ABSTRACT: Game::EvonyTKR::Buff Evaluation Multipliers for Attacking use cases.

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

  field $AvailableMultipliers = enum [
    'All Troops',
    'Ground Troops',
    'Mounted Troops',
    'Ranged Troops',
    'Siege Machines',
  ];

  field %BuffMultipliers = (    # key by attribute
    'Attack'              => (    # key by General Score Preference then by buff type
      'Ground Troops'     => (
        'All Troops'      => 2.14833,
        'Ground Troops'   => 1.81500,
        'Mounted Troops'  => 0.11111,
        'Ranged Troops'   => 0.11111,
        'Siege Machines'  => 0.11111,
      ),
      'Mounted Troops'    => (
        'All Troops'      => 2.13833,
        'Ground Troops'   => 0.11111,
        'Mounted Troops'  => 1.80500,
        'Ranged Troops'   => 0.11111,
        'Siege Machines'  => 0.11111,
      ),
      'Ranged Troops'     => (
        'All Troops'      => 2.84933,
        'Ground Troops'   => 0.11111,
        'Mounted Troops'  => 0.11111,
        'Ranged Troops'   => 2.51600,
        'Siege Machines'  => 0.11111,
      ),
      'Siege Machines'    => (
        'All Troops'      => 3.20733,
        'Ground Troops'   => 0.11111,
        'Mounted Troops'  => 0.11111,
        'Ranged Troops'   => 0.11111,
        'Siege Machines'  => 2.87400,
      ),
    ),
    # This is the in-battle buff attribute.  Consensus is that the game does not use it.
    'Attack Speed'        => 0, 

    # EvAns doesn't have this, but I think he treats it the same as 2 Wounded.
    'Death to Survival'   => 0.25555, 
    'Death to Soul'       => 0.12555,
    # EvAns actually splits this by condition.  I'm unsure why.
    'Death to Wounded'    => 0.25555,    
    'Defense'             => (    # key by General Score Preference then by buff type
      'Ground Troops'     => (
        'All Troops'      => 1.55391,
        'Ground Troops'   => 0.16667,
        'Mounted Troops'  => 0.16667,
        # this is a persistent anomaly in EvAns ranking
        'Ranged Troops'   => 1.05390,    
        'Siege Machines'  => 0.16667,
      ),
      'Mounted Troops'    => (
        'All Troops'      => 1.36491,
        'Ground Troops'   => 0.16667,
        'Mounted Troops'  => 0.86490,
        'Ranged Troops'   => 0.16667,
        'Siege Machines'  => 0.16667,
      ),
      'Ranged Troops'     => (
        'All Troops'      => 1.25151,
        'Ground Troops'   => 0.16667,
        'Mounted Troops'  => 0.16667,
        'Ranged Troops'   => 0.75150,
        'Siege Machines'  => 0.16667,
      ),
      # I personally think EvAns over rates Siege Defense.  Siege depends on other troops for its Survival. 
      'Siege Machines'    => (
        'All Troops'      => 1.10751,
        'Ground Troops'   => 0.16667,
        'Mounted Troops'  => 0.16667,
        'Ranged Troops'   => 0.16667,
        'Siege Machines'  => 0.60750,
      ),
    ),
    'Deserter Capacity'      => 0,
    'Double Items Drop Rate' => 0,

    'HP' => (    # key by General Score Preference then by buff type
      'Ground Troops'    => (
        'All Troops'     => 1.67101,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        # this is a persistent anomaly in EvAns ranking
        'Ranged Troops'  => 1.17100,
        'Siege Machines' => 0.16667,
      ),
      'Mounted Troops' => (
        'All Troops'     => 1.46101,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.96100,
        'Ranged Troops'  => 0.16667,
        'Siege Machines' => 0.16667,
      ),
      'Ranged Troops' => (
        'All Troops'     => 1.46101,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        'Ranged Troops'  => 0.96100,
        'Siege Machines' => 0.16667,
      ),
      # I personally think EvAns over rates Siege HP.  Siege depends on other troops for its Survival. 
      'Siege Machines'    => (
        'All Troops'      => 1.17501,
        'Ground Troops'   => 0.16667,
        'Mounted Troops'  => 0.16667,
        'Ranged Troops'   => 0.16667,
        'Siege Machines'  => 0.67500,
      ),
    ),
    'Healing Speed'               => 0,
    'Hospital Capacity'           => 0,

    'Leadership'                  => 0,    # EvAns has no direct mapping to this.
    'Load'                        => 0,

    # EvAns is not consistently using this value yet, but has posted it as the planned value
    'March Size Capacity'         => 6, 
    'March Time'                  => 0,
    'Marching Speed'              => 0,
    'Marching Speed to Monsters'  => 0,
    'Monster Attack'              => 0,

    'Politics'                    => 0,    # EvAns has no direct mapping to this.

    'Rally Capacity'              => 0.5,
    # EvAns actually gives Ranged Troops a Bonus for Siege Range increase as well. 
    # But not a Ranged bonus to Siege.  
    # I am using the upper value in all cases and ignoring the General's prefered Troop Type for this buff. 
    'Range'                       => (  # key by Buff Type then by value type.
      'Ground Troops'             => (
        'flat'                    => 0,
        'percentage'              => 0,
      ),
      'Mounted Troops'            => (
        'flat'                    => 0,
        'percentage'              => 0,
      ),
      'Ranged Troops'             => (
        'flat'                    => 0.12000,
        'percentage'              => 0.12000,
      ),
      'Siege Machines'            => (
        'flat'                    => 0.06000,
        'percentage'              => 0.5,
      ),
    ),
    'Resources Production' => 0,

    'Stamina cost'               => 0,
    'SubCity Construction Speed' => 0,
    'SubCity Gold Production'    => 0,
    'SubCity Training Speed'     => 0,
    'SubCity Troop Capacity'     => 0,

    'Training Capacity' => 0,
    'Training Speed'    => 0,
    # EvAns actually splits this by condition.  I'm unsure why. ,
    'Wounded to Death'  => 0.25555,    
  );

  my $classData = Game::EvonyTKR::Buff::Data->new();
  my @BuffAttributes;
  my @BuffConditions;
  my @BuffClasses;

  ADJUST {
    if(scalar @BuffAttributes == 0) {
      $classData->set_BuffAttributes();
      @BuffAttributes = $classData->BuffAttributes();
    }

    if(scalar @BuffConditions == 0) {
      $classData->set_BuffConditions();
      @BuffConditions = $classData->BuffConditions();
    }

    if(scalar @BuffClasses == 0) {
      $classData->set_BuffClasses();
      @BuffClasses = $classData->BuffClasses();
    }
  }

  method getMultiplierForBuff($attribute, $GeneralBias, $BuffBias = 'All Troops', $unit = 'percentage') {
    if(scalar @BuffAttributes == 0) {
      $self->logger()->logcroak("No attributes loaded");
    }
    if(scalar @BuffClasses == 0) {
      $self->logger()->logcroak("No classes loaded");
    }
    if (any {$_ =~ /$attribute/i} @BuffAttributes) {
      if (any {$_ =~ /$GeneralBias/} @{$AvailableMultipliers->values()}){
        if (any {$_ =~ /$BuffBias/i} @BuffClasses) {
          if ($attribute =~ /Attack/i) {
            return $BuffMultipliers{'Attack'}{$GeneralBias}{$BuffBias};
          }
          elsif ($attribute =~ /Defense/i) {
            return $BuffMultipliers{'Defense'}{$GeneralBias}{$BuffBias};
          }
          elsif ($attribute =~ /HP/i) {
            return $BuffMultipliers{'HP'}{$GeneralBias}{$BuffBias};
          }
          elsif($attribute =~ /Range/i) {
            if($unit =~ /(flat|percentage)/i) {
              return $BuffMultipliers{'Range'}{$BuffBias}{lc $unit};
            }
            else {
              $self->logger()->error("$unit is invalid.");
            }
          }
          else {
            return $BuffMultipliers{$attribute};
          }
        }
        else {
          $self->logger()->error("$BuffBias is invalid.");
        }
      } 
      else {
        $self->logger()->error("$GeneralBias is invalid.");
      }
    }
    else {
      $self->logger()->error("$attribute is invalid");
    }
    return 0;
  }

}
1;

__END__

=head1 DESCRIPTION

See Game::EvonyTKR::Buff::EvaluationMultipliers for a general description.

This provides the base overrides for the Attacking use cases.

=cut

=method new()

autogenerated constructor for this class
=cut

=method getMultiplierForBuff($attribute, $GeneralBias, $BuffBias = 'All Troops', $unit = 'percentage') 

$attribute must be a value from the list of attributes set by Game::EvonyTKR::Buff::Data

$GeneralBias must be one of 
=for :List

* 'All Troops',

* 'Ground Troops',

* 'Mounted Troops',

* 'Ranged Troops',

* 'Siege Machines',

This field represents the type of troop that the General prefers to lead. 

$BuffBias must be one of the classes set by Game::EvonyTKR::Buff:Data

This field represents the type of troop that the Buff affects. 

$unit must be either 'flat' or 'percentage'.

Defaults are shown for the two optional parameters. 

This method is highly specific to PVP Attacking, it makes all sorts of assumptions about the nature of the Buff Multipliers.  It returns the appropriate EvAns multiplier, with some minimal adjustments to fit my own biases. 

=cut

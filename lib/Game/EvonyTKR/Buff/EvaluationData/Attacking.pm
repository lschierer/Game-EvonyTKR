use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::Buff::EvaluationData::Attacking :
  isa(Game::EvonyTKR::Buff::EvaluationData) {
# PODNAME: Game::EvonyTKR::Buff::EvaluationData::Attacking

  use Carp;
  use Data::Printer;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils   qw(is enum);
  use Util::Any -all;
  use Game::EvonyTKR::Data;
  use namespace::autoclean;

# VERSION

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $AvailableMultipliers = enum [
    'All Troops',
    'Ground Troops',
    'Mounted Troops',
    'Ranged Troops',
    'Siege Machines',
  ];

  field @RelevantBuffConditions : reader;

  field @RelevantDebuffConditions : reader;

  field @RelevantClasses : reader;

  ADJUST {

    @RelevantClasses = grep { $_ !~ /Monsters/i } $self->BuffClasses();

    @RelevantBuffConditions = grep { $_ !~ /^(In City|In Main City)/ }
      grep { $_ !~ /(Defending|Defense)/ }
      grep { $_ !~ /Reinforcing/ }
      grep { $_ !~ /officer/ }
      grep { $_ !~ /(Monster)/i }
      grep { $_ !~ /Mayor/i } $self->buffConditions();
    $self->logger()
      ->debug("RelevantBuffConditions are " . np @RelevantBuffConditions);

    @RelevantDebuffConditions = grep { $_ !~ /^(In City|In Main City)/ }
      grep { $_ !~ /(Defending|Defense)/ }
      grep { $_ !~ /Reinforcing/ }
      grep { $_ !~ /officer/ }
      grep { $_ !~ /(Monster)/i }
      grep { $_ !~ /Mayor/i } $self->debuffConditions();
    $self->logger()
      ->debug("RelevantDebuffConditions are " . np @RelevantDebuffConditions);
  }

  field %BuffMultipliers = (    # key by attribute
    'Attack' => {    # key by General Score Preference then by buff type
      'Ground Troops' => {
        'All Troops'     => 2.14833,
        'Ground Troops'  => 1.81500,
        'Mounted Troops' => 0.11111,
        'Ranged Troops'  => 0.11111,
        'Siege Machines' => 0.11111,
      },
      'Mounted Troops' => {
        'All Troops'     => 2.13833,
        'Ground Troops'  => 0.11111,
        'Mounted Troops' => 1.80500,
        'Ranged Troops'  => 0.11111,
        'Siege Machines' => 0.11111,
      },
      'Ranged Troops' => {
        'All Troops'     => 2.84933,
        'Ground Troops'  => 0.11111,
        'Mounted Troops' => 0.11111,
        'Ranged Troops'  => 2.51600,
        'Siege Machines' => 0.11111,
      },
      'Siege Machines' => {
        'All Troops'     => 3.20733,
        'Ground Troops'  => 0.11111,
        'Mounted Troops' => 0.11111,
        'Ranged Troops'  => 0.11111,
        'Siege Machines' => 2.87400,
      },
    },
# This is the in-battle buff attribute.  Consensus is that the game does not use it.
    'Attack Speed' => 0,

    # EvAns doesn't have this, but I think he treats it the same as 2 Wounded.
    'Death to Survival' => 0.25555,
    'Death to Soul'     => 0.12555,
    # EvAns actually splits this by condition.  I'm unsure why.
    'Death to Wounded' => 0.25555,
    'Defense'          => {  # key by General Score Preference then by buff type
      'Ground Troops' => {
        'All Troops'     => 1.55391,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        # this is a persistent anomaly in EvAns ranking
        'Ranged Troops'  => 1.05390,
        'Siege Machines' => 0.16667,
      },
      'Mounted Troops' => {
        'All Troops'     => 1.36491,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.86490,
        'Ranged Troops'  => 0.16667,
        'Siege Machines' => 0.16667,
      },
      'Ranged Troops' => {
        'All Troops'     => 1.25151,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        'Ranged Troops'  => 0.75150,
        'Siege Machines' => 0.16667,
      },
      'Siege Machines' => {
        'All Troops'     => 1.10751,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        'Ranged Troops'  => 0.16667,
        'Siege Machines' => 0.60750,
      },
    },
    'Deserter Capacity'      => 0,
    'Double Items Drop Rate' => 0,

    'HP' => {    # key by General Score Preference then by buff type
      'Ground Troops' => {
        'All Troops'     => 1.67101,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        # this is a persistent anomaly in EvAns ranking
        'Ranged Troops'  => 1.17100,
        'Siege Machines' => 0.16667,
      },
      'Mounted Troops' => {
        'All Troops'     => 1.46101,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.96100,
        'Ranged Troops'  => 0.16667,
        'Siege Machines' => 0.16667,
      },
      'Ranged Troops' => {
        'All Troops'     => 1.46101,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        'Ranged Troops'  => 0.96100,
        'Siege Machines' => 0.16667,
      },
      'Siege Machines' => {
        'All Troops'     => 1.17501,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        'Ranged Troops'  => 0.16667,
        'Siege Machines' => 0.67500,
      },
    },
    'Healing Speed'     => 0,
    'Hospital Capacity' => 0,

    'Leadership' => 0,    # EvAns has no direct mapping to this.
    'Load'       => 0,

# EvAns is not consistently using this value yet, but has posted it as the planned value
    'March Size Capacity'        => 6,
    'March Time'                 => 0,
    'Marching Speed'             => 0,
    'Marching Speed to Monsters' => 0,
    'Monster Attack'             => 0,

    'Politics' => 0,      # EvAns has no direct mapping to this.

    'Rally Capacity' => 0.5,
# EvAns actually gives Ranged Troops a Bonus for Siege Range increase as well.
# But not a Ranged bonus to Siege.
# I am using the upper value in all cases and ignoring the General's prefered Troop Type for this buff.
    'Range' => {          # key by Buff Type then by value type.
      'Ground Troops' => {
        'flat'       => 0,
        'percentage' => 0,
      },
      'Mounted Troops' => {
        'flat'       => 0,
        'percentage' => 0,
      },
      'Ranged Troops' => {
        'flat'       => 0.12000,
        'percentage' => 0.12000,
      },
      'Siege Machines' => {
        'flat'       => 0.06000,
        'percentage' => 0.5,
      },
    },
    'Resources Production' => 0,

    'Stamina cost'               => 0,
    'SubCity Construction Speed' => 0,
    'SubCity Gold Production'    => 0,
    'SubCity Training Speed'     => 0,
    'SubCity Troop Capacity'     => 0,

    'Training Capacity' => 0,
    'Training Speed'    => 0,
    # This only actually exists as a debuff.
    'Wounded to Death' => 0,
  );

  field %DebuffMultipliers = (    # key by attribute
    'Attack' => {    # key by General Score Preference then by buff type
      'Ground Troops' => {
        'All Troops'     => 3.54240,
        'Ground Troops'  => 0.76800,
        'Mounted Troops' => 1.20000,
        'Ranged Troops'  => 1.05600,
        'Siege Machines' => 0.91200,
      },
      'Mounted Troops' => {
        'All Troops'     => 2.17530,
        'Ground Troops'  => 0.63004,
        'Mounted Troops' => 0.53056,
        'Ranged Troops'  => 0.82900,
        'Siege Machines' => 0.72952,
      },
      'Ranged Troops' => {
        'All Troops'     => 1.63836,
        'Ground Troops'  => 0.55500,
        'Mounted Troops' => 0.42180,
        'Ranged Troops'  => 0.35520,
        'Siege Machines' => 0.48840,
      },
      'Siege Machines' => {
        'All Troops'     => 2.06640,
        'Ground Troops'  => 0.61600,
        'Mounted Troops' => 0.7000,
        'Ranged Troops'  => 0.44800,
        'Siege Machines' => 0.53200,
      },
    },
# This is the in-battle buff attribute.  Consensus is that the game does not use it.
    'Attack Speed' => 0,

    # EvAns doesn't have this, but I think he treats it the same as 2 Wounded.
    'Death to Survival' => 0.12555,
    'Death to Soul'     => 0.12555,
    # EvAns actually splits this by condition.  I'm unsure why.
    'Death to Wounded' => 0.12555,
    'Defense'          => {  # key by General Score Preference then by buff type
      'Ground Troops' => {
        'All Troops'     => 2.80800,
        'Ground Troops'  => 0.60000,
        'Mounted Troops' => 0.72000,
        'Ranged Troops'  => 0.96000,
        'Siege Machines' => 0.84000,
      },
      'Mounted Troops' => {
        'All Troops'     => 1.72432,
        'Ground Troops'  => 0.58030,
        'Mounted Troops' => 0.41450,
        'Ranged Troops'  => 0.66320,
        'Siege Machines' => 0.49740,
      },
      'Ranged Troops' => {
        'All Troops'     => 1.31069,
        'Ground Troops'  => 0.44400,
        'Mounted Troops' => 0.33744,
        'Ranged Troops'  => 0.28416,
        'Siege Machines' => 0.39072,
      },
      'Siege Machines' => {
        'All Troops'     => 1.63800,
        'Ground Troops'  => 0.35000,
        'Mounted Troops' => 0.42000,
        'Ranged Troops'  => 0.49000,
        'Siege Machines' => 0.56000,
      },
    },
    'Deserter Capacity'      => 0,
    'Double Items Drop Rate' => 0,

    'HP' => {    # key by General Score Preference then by buff type
      'Ground Troops' => {
        'All Troops'     => 2.80800,
        'Ground Troops'  => 0.60000,
        'Mounted Troops' => 0.72000,
        'Ranged Troops'  => 0.96000,
        'Siege Machines' => 0.84000,
      },
      'Mounted Troops' => {
        'All Troops'     => 1.72432,
        'Ground Troops'  => 0.58030,
        'Mounted Troops' => 0.41450,
        'Ranged Troops'  => 0.66320,
        'Siege Machines' => 0.49740,
      },
      'Ranged Troops' => {
        'All Troops'     => 1.31069,
        'Ground Troops'  => 0.44400,
        'Mounted Troops' => 0.33744,
        'Ranged Troops'  => 0.28416,
        'Siege Machines' => 0.39072,
      },
# I personally think EvAns over rates Siege HP.  Siege depends on other troops for its Survival.
      'Siege Machines' => {
        'All Troops'     => 1.17501,
        'Ground Troops'  => 0.16667,
        'Mounted Troops' => 0.16667,
        'Ranged Troops'  => 0.16667,
        'Siege Machines' => 0.67500,
      },
    },
    'Healing Speed'     => 0,
    'Hospital Capacity' => 0,

    'Leadership' => 0,    # EvAns has no direct mapping to this.
    'Load'       => 0,

    'March Size Capacity'        => 0,
    'March Time'                 => 0,
    'Marching Speed'             => 0,
    'Marching Speed to Monsters' => 0,
    'Monster Attack'             => 0,

    'Politics' => 0,      # EvAns has no direct mapping to this.

    'Rally Capacity' => 0,
    'Range'          =>
      0,   # while it might be interesting, I have never seen a debuff on range.
    'Resources Production' => 0,

    'Stamina cost'               => 0,
    'SubCity Construction Speed' => 0,
    'SubCity Gold Production'    => 0,
    'SubCity Training Speed'     => 0,
    'SubCity Troop Capacity'     => 0,

    'Training Capacity' => 0,
    'Training Speed'    => 0,
    # EvAns actually splits this by condition.  I'm unsure why. ,
    'Wounded to Death' => 0.12555,
  );

  method getMultiplierForBuff(
    $attribute, $GeneralBias,
    $BuffBias = 'All Troops',
    $unit = 'percentage'
  ) {
    my $returnValue = 0;
    if (scalar $self->BuffAttributes() == 0) {
      $self->logger()->logcroak("No attributes loaded");
    }
    if (scalar $self->RelevantClasses() == 0) {
      $self->logger()->logcroak("No classes loaded");
    }
    if (any { $_ =~ /$attribute/i } $self->BuffAttributes()) {
      if (any { $_ =~ /$GeneralBias/ } @{ $AvailableMultipliers->values() }) {
        if (any { $_ =~ /$BuffBias/i } $self->RelevantClasses()) {
          $self->logger()
            ->debug(sprintf('getMultiplierForBuff for %s with %s, %s, %s',
              $attribute, $GeneralBias, $BuffBias, $unit,));
          if ($attribute =~ /Attack/i) {
            if (exists $BuffMultipliers{'Attack'}) {
              if (exists $BuffMultipliers{'Attack'}->{$GeneralBias}) {
                if (
                  exists $BuffMultipliers{'Attack'}->{$GeneralBias}->{$BuffBias}
                ) {
                  $returnValue =
                    $BuffMultipliers{'Attack'}->{$GeneralBias}->{$BuffBias};
                }
                else {
                  $self->logger()
                    ->error(sprintf('BuffMultipliers{Attack}->{%s} does not contain %s',  
                      $GeneralBias, $BuffBias));
                }
              }
              else {
                $self->logger()
                  ->error(
                  "BuffMultipliers{Attack} does not contain $GeneralBias");
              }
            }
            else {
              $self->logger()->error("BuffMultipliers does not contain Attack");
            }
          }
          elsif ($attribute =~ /Defense/i) {
            if (exists $BuffMultipliers{'Defense'}) {
              if (exists $BuffMultipliers{'Defense'}->{$GeneralBias}) {
                if (
                  exists $BuffMultipliers{'Defense'}->{$GeneralBias}
                  ->{$BuffBias}) {
                  $returnValue =
                    $BuffMultipliers{'Defense'}->{$GeneralBias}->{$BuffBias};
                }
                else {
                  $self->logger()
                    ->error(
"BuffMultipliers{Defense}->{$GeneralBias} does not contain $BuffBias"
                    );
                }
              }
              else {
                $self->logger()
                  ->error(
                  "BuffMultipliers{Defense} does not contain $GeneralBias");
              }
            }
            else {
              $self->logger()
                ->error("BuffMultipliers does not contain Defense");
            }
          }
          elsif ($attribute =~ /HP/i) {
            if (exists $BuffMultipliers{'HP'}) {
              if (exists $BuffMultipliers{'HP'}->{$GeneralBias}) {
                if (exists $BuffMultipliers{'HP'}->{$GeneralBias}->{$BuffBias})
                {
                  $returnValue =
                    $BuffMultipliers{'HP'}->{$GeneralBias}->{$BuffBias};
                }
                else {
                  $self->logger()
                    ->error(
"BuffMultipliers{HP}->{$GeneralBias} does not contain $BuffBias"
                    );
                }
              }
              else {
                $self->logger()
                  ->error("BuffMultipliers{HP} does not contain $GeneralBias");
              }
            }
            else {
              $self->logger()->error("BuffMultipliers does not contain HP");
            }
          }
          elsif ($attribute =~ /Range/i) {
            if (exists $BuffMultipliers{'Range'}) {
              if (exists $BuffMultipliers{'Range'}->{$BuffBias}) {
                if ($unit =~ /(flat|percentage)/i) {
                  $unit = lc $unit;
                  if (exists $BuffMultipliers{'Range'}->{$BuffBias}->{$unit}) {
                    $returnValue =
                      $BuffMultipliers{'Range'}->{$BuffBias}->{$unit};
                  }
                  else {
                    $self->logger()
                      ->error(
"BuffMultipliers{Range}->{$BuffBias} does not contain $unit"
                      );
                  }
                }
                else {
                  $self->logger()->error("'$unit' is an invalid value.");
                }
              }
              else {
                $self->logger()
                  ->error("BuffMultipliers{Range} does not contain $BuffBias");
              }
            }
            else {
              $self->logger()->error("BuffMultipliers does not contain Range");
            }
          }
          else {
            $returnValue = $BuffMultipliers{$attribute};
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
    $self->logger()
      ->info(
"getMultiplierForBuff for $attribute with $GeneralBias, $BuffBias, $unit returns '$returnValue'"
      );
    return $returnValue;
  }

  method getMultiplierForDebuff(
    $attribute, $GeneralBias,
    $BuffBias = 'All Troops',
    $unit = 'percentage'
  ) {
    my $returnValue = 0;
    if (scalar $self->BuffAttributes() == 0) {
      $self->logger()->logcroak("No attributes loaded");
    }
    if (scalar $self->RelevantClasses() == 0) {
      $self->logger()->logcroak("No classes loaded");
    }
    if (any { $_ =~ /$attribute/i } $self->BuffAttributes()) {
      if (any { $_ =~ /$GeneralBias/ } @{ $AvailableMultipliers->values() }) {
        if (any { $_ =~ /$BuffBias/i } $self->RelevantClasses()) {
          $self->logger()
            ->debug(
"getMultiplierForDebuff for $attribute with $GeneralBias, $BuffBias, $unit"
            );
          if ($attribute =~ /Attack/i) {
            if (exists $DebuffMultipliers{'Attack'}) {
              if (exists $DebuffMultipliers{'Attack'}->{$GeneralBias}) {
                if (
                  exists $DebuffMultipliers{'Attack'}->{$GeneralBias}
                  ->{$BuffBias}) {
                  $returnValue =
                    $DebuffMultipliers{'Attack'}->{$GeneralBias}->{$BuffBias};
                }
                else {
                  $self->logger()
                    ->error(
"DebuffMultipliers{Attack}->{$GeneralBias} does not contain $BuffBias"
                    );
                }
              }
              else {
                $self->logger()
                  ->error(
                  "DebuffMultipliers{Attack} does not contain $GeneralBias");
              }
            }
            else {
              $self->logger()
                ->error("DebuffMultipliers does not contain Attack");
            }
          }
          elsif ($attribute =~ /Defense/i) {
            if (exists $DebuffMultipliers{'Defense'}) {
              if (exists $DebuffMultipliers{'Defense'}->{$GeneralBias}) {
                if (
                  exists $DebuffMultipliers{'Defense'}->{$GeneralBias}
                  ->{$BuffBias}) {
                  $returnValue =
                    $DebuffMultipliers{'Defense'}->{$GeneralBias}->{$BuffBias};
                }
                else {
                  $self->logger()
                    ->error(
"DebuffMultipliers{Defense}->{$GeneralBias} does not contain $BuffBias"
                    );
                }
              }
              else {
                $self->logger()
                  ->error(
                  "DebuffMultipliers{Defense} does not contain $GeneralBias");
              }
            }
            else {
              $self->logger()
                ->error("DebuffMultipliers does not contain Defense");
            }
          }
          elsif ($attribute =~ /HP/i) {
            if (exists $DebuffMultipliers{'HP'}) {
              if (exists $DebuffMultipliers{'HP'}->{$GeneralBias}) {
                if (
                  exists $DebuffMultipliers{'HP'}->{$GeneralBias}->{$BuffBias})
                {
                  $returnValue =
                    $DebuffMultipliers{'HP'}->{$GeneralBias}->{$BuffBias};
                }
                else {
                  $self->logger()
                    ->error(
"DebuffMultipliers{HP}->{$GeneralBias} does not contain $BuffBias"
                    );
                }
              }
              else {
                $self->logger()
                  ->error(
                  "DebuffMultipliers{HP} does not contain $GeneralBias");
              }
            }
            else {
              $self->logger()->error("DebuffMultipliers does not contain HP");
            }
          }
          else {
            $returnValue = $DebuffMultipliers{$attribute};
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
    $self->logger()
      ->info(
"getMultiplierForDebuff for $attribute with $GeneralBias, $BuffBias, $unit returns '$returnValue'"
      );
    return $returnValue;
  }
}
1;

__END__

# ABSTRACT: Game::EvonyTKR::Buff Evaluation Data for Attacking use cases.

=head1 DESCRIPTION

This was originally based on L<https://www.evonyanswers.com/post/evony-answers-attribute-methodology-explanation>
as of 2024-08-14.  However, while I have I<mostly> used EvAns values, this will act somewhat differently.

EvAns rates the value of each Buff, a Buff like Maximillian I's "reduces enemy ranged troops’ attack by 20% when General is leading the army to attack" is valued as an "Attacking Enemy Attack Debuff" where one that does not have the "leading the army to attack" condition would be valued as a "Reinforcing Enemy Attack Debuff." This would be true regardless of if you are looking at his results for the Attacking pairs or the Reinforcing pairs.

Instead, I am valuing the buffs by *situation.*  If a buff will activate in the sitation, I value it as such. If it will not activate, I value it at 0 - it might as well not be there. 

This class is for the PvP attacking situation.  

=cut

=method new()

autogenerated constructor for this class
=cut

=method getMultiplierForBuff($attribute, $GeneralBias, $BuffBias = 'All Troops', $unit = 'percentage') 

$attribute must be a value from the list of attributes set by Game::EvonyTKR::Data

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

=method getMultiplierForDebuff($attribute, $GeneralBias, $BuffBias = 'All Troops', $unit = 'percentage') 
$attribute must be a value from the list of attributes set by Game::EvonyTKR::Data

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

This is the same as getMultiplierForBuff except it returns Debuff multipliers instead of Buff multipliers.
=cut


=head1 TODO

=head2 better? siege values

I have considered significantly adjusting the Siege values.  I do not understand EvAns' logic for them. 
When Siege attacks, it prioritizes other Siege, which already has *very* low HP.  My priority is to kill everything before their siege kills me.  As I understand it then, Siege cares about buffs as follows:

=for :List

* March Size - Siege depends on massed fire.  March size is critical for everything escept a wall general. 

* Range - if I have more range, I can kill you before you can reach me to kill me.  I think EvAns underrates this on the theory that everyone essentially has the same buffs, and it thus cancels out.  That is probably true when a keep is defending against a siege attack, or any siege versus siege battle.  But if the march comming against my siege force is ground, mounted, or ranged heavy instead, then that is not necessarily true.  That Range buff determines how soon my Siege starts killing their siege, and they only have layers.  If they have further to march, my siege will start on other troop types earlier in the battle.

* Attack buff - I understand everyone debuffs Siege attack, but the more of this you have, you still get half of it at worst. You need as much as possible to kill as much as possible.  Especially since Siege does not have huge individual values and depends on massed fire. 

* Toughness (HP and Defense) Debuffs - Make them more vulnerable to my attack. 

* I<Siege> Attack Debuff - Make sure their Siege can't attack me back. 

* Siege Toughness - If I get to the point where I am being attacked, this will help I<some> but essentially I've lost.  I need to kill them before I need this. 

The reason why I have I<not> already customized the siege values is that EvAns does have fairly customized values for each troop type here.  He has obviously done some work analysing reports and with the available simulators.  I cannot safely modify the values until and unless I have the time to put in that effort. 
=cut
=cut

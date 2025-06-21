use v5.40.0;
use experimental qw(class);
use utf8::all;
require JSON::PP;
require Scalar::Util;

class Game::EvonyTKR::Model::General::Pair : isa(Game::EvonyTKR::Model::Data) {
  require Game::EvonyTKR::Model::General;
  require Game::EvonyTKR::Model::Buff::Summarizer;
  use overload
    '""'       => \&as_string,
    'fallback' => 1;

  field $primary : reader : param;
  field $secondary : reader : param;
  field $rootManager : reader = undef;

  #computed fields;

  field $marchbuff : reader = 0;

  field $attackbuff : reader  = 0;
  field $defensebuff : reader = 0;
  field $hpbuff : reader      = 0;

  field $groundattackdebuff : reader  = 0;
  field $grounddefensedebuff : reader = 0;
  field $groundhpdebuff : reader      = 0;

  field $mountedattackdebuff : reader  = 0;
  field $mounteddefensedebuff : reader = 0;
  field $mountedhpdebuff : reader      = 0;

  field $rangedattackdebuff : reader  = 0;
  field $rangeddefensedebuff : reader = 0;
  field $rangedhpdebuff : reader      = 0;

  field $siegeattackdebuff : reader  = 0;
  field $siegedefensedebuff : reader = 0;
  field $siegehpdebuff : reader      = 0;

  method setRootManager ($nm) {
    if (Scalar::Util::blessed $nm eq 'Game::EvonyTKR::Model::EvonyTKR::Manager')
    {
      $rootManager = $nm;
      return 1;
    }
    return 0;
  }

  method updateBuffs (
    $generalType          = '',
    $primaryAscending     = 'red5',
    $primarySpeciality1   = 'gold',
    $primarySpeciality2   = 'gold',
    $primarySpeciality3   = 'gold',
    $primarySpeciality4   = 'gold',
    $secondarySpeciality1 = 'gold',
    $secondarySpeciality2 = 'gold',
    $secondarySpeciality3 = 'gold',
    $secondarySpeciality4 = 'gold',
    $keepLevel            = 40,
    $primaryLevel         = 45,
    $secondaryLevel       = 45,
  ) {

    my $primarySummarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager    => $rootManager,
      general        => $primary,
      isPrimary      => 1,
      targetType     => $generalType,
      ascendingLevel => $primaryAscending,
      speciality1    => $primarySpeciality1,
      speciality2    => $primarySpeciality2,
      speciality3    => $primarySpeciality3,
      speciality4    => $primarySpeciality4,
      generalLevel   => $primaryLevel,
      keepLevel      => $keepLevel,
    );
    my $secondarySummarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager  => $rootManager,
      general      => $secondary,
      isPrimary    => 0,
      targetType   => $generalType,
      speciality1  => $secondarySpeciality1,
      speciality2  => $secondarySpeciality2,
      speciality3  => $secondarySpeciality3,
      speciality4  => $secondarySpeciality4,
      generalLevel => $secondaryLevel,
      keepLevel    => $keepLevel,
    );

    # Update buffs for both summarizers
    $primarySummarizer->updateBuffs();
    $secondarySummarizer->updateBuffs();

    # Update debuffs for both summarizers
    $primarySummarizer->updateDebuffs();
    $secondarySummarizer->updateDebuffs();

    # Set buff values by summing primary and secondary
    $marchbuff =
      $primarySummarizer->marchIncrease + $secondarySummarizer->marchIncrease;
    $attackbuff =
      $primarySummarizer->attackIncrease + $secondarySummarizer->attackIncrease;
    $defensebuff = $primarySummarizer->defenseIncrease +
      $secondarySummarizer->defenseIncrease;
    $hpbuff = $primarySummarizer->hpIncrease + $secondarySummarizer->hpIncrease;

    # Set ground troop debuff values
    $groundattackdebuff = $primarySummarizer->reducegroundattack +
      $secondarySummarizer->reducegroundattack;
    $grounddefensedebuff = $primarySummarizer->reducegrounddefense +
      $secondarySummarizer->reducegrounddefense;
    $groundhpdebuff =
      $primarySummarizer->reducegroundhp + $secondarySummarizer->reducegroundhp;

    # Set mounted troop debuff values
    $mountedattackdebuff = $primarySummarizer->reducemountedattack +
      $secondarySummarizer->reducemountedattack;
    $mounteddefensedebuff = $primarySummarizer->reducemounteddefense +
      $secondarySummarizer->reducemounteddefense;
    $mountedhpdebuff = $primarySummarizer->reducemountedhp +
      $secondarySummarizer->reducemountedhp;

    # Set ranged troop debuff values
    $rangedattackdebuff = $primarySummarizer->reducerangedattack +
      $secondarySummarizer->reducerangedattack;
    $rangeddefensedebuff = $primarySummarizer->reducerangeddefense +
      $secondarySummarizer->reducerangeddefense;
    $rangedhpdebuff =
      $primarySummarizer->reducerangedhp + $secondarySummarizer->reducerangedhp;

    # Set siege machine debuff values
    $siegeattackdebuff = $primarySummarizer->reducesiegeattack +
      $secondarySummarizer->reducesiegeattack;
    $siegedefensedebuff = $primarySummarizer->reducesiegedefense +
      $secondarySummarizer->reducesiegedefense;
    $siegehpdebuff =
      $primarySummarizer->reducesiegehp + $secondarySummarizer->reducesiegehp;
  }

  # Method to convert to hash
  method to_hash {
    return {
      primary              => $primary,
      secondary            => $secondary,
      marchbuff            => $marchbuff,
      attackbuff           => $attackbuff,
      defensebuff          => $defensebuff,
      hpbuff               => $hpbuff,
      groundattackdebuff   => $groundattackdebuff,
      grounddefensedebuff  => $grounddefensedebuff,
      groundhpdebuff       => $groundhpdebuff,
      mountedattackdebuff  => $mountedattackdebuff,
      mounteddefensedebuff => $mounteddefensedebuff,
      mountedhpdebuff      => $mountedhpdebuff,
      rangedattackdebuff   => $rangedattackdebuff,
      rangeddefensedebuff  => $rangeddefensedebuff,
      rangedhpdebuff       => $rangedhpdebuff,
      siegeattackdebuff    => $siegeattackdebuff,
      siegedefensedebuff   => $siegedefensedebuff,
      siegehpdebuff        => $siegehpdebuff,
      # Add any other useful properties you might need
    };
  }

  # Method for JSON serialization
  method TO_JSON {
    return $self->to_hash();
  }

  # Stringification method using JSON
  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

}
1;

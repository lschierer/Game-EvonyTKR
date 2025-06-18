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

  field $attackbuff : reader = 0;

  method setRootManager ($nm) {
    if (Scalar::Util::blessed $nm eq 'Game::EvonyTKR::Model::EvonyTKR::Manager')
    {
      $rootManager = $nm;
      return 1;
    }
    return 0;
  }

  # some parameters unused currently, but adding them for future growth
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
    $primarySummarizer->updateBuffs();
    $secondarySummarizer->updateBuffs();
    $marchbuff =
      $primarySummarizer->marchIncrease + $secondarySummarizer->marchIncrease;
    $attackbuff =
      $primarySummarizer->attackIncrease + $secondarySummarizer->attackIncrease;
  }

  # Method to convert to hash
  method to_hash {
    return {
      primary   => $primary,
      secondary => $secondary,
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

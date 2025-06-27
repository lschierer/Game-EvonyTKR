use v5.40.0;
use experimental qw(class);
use utf8::all;
require JSON::PP;
require Scalar::Util;

class Game::EvonyTKR::Model::General::Pair : isa(Game::EvonyTKR::Model::Data) {
  require Game::EvonyTKR::Model::General;
  require Game::EvonyTKR::Model::Buff::Summarizer;
  use List::AllUtils qw( all any none );
  use overload
    '""'       => \&as_string,
    'fallback' => 1;

  field $primary : reader : param;
  field $secondary : reader : param;
  field $rootManager : reader = undef;
  field $targetType : reader;

  ADJUST {
    $targetType =
      ref($primary->type) eq 'ARRAY' ? $primary->type->[0] : $primary->type;
  }

  #computed fields;
  field $buffValues : reader = {
    'Ground Troops' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };

  field $debuffValues : reader = {
    'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };

  method setRootManager ($nm) {
    if (Scalar::Util::blessed $nm eq 'Game::EvonyTKR::Model::EvonyTKR::Manager')
    {
      $rootManager = $nm;
      return 1;
    }
    return 0;
  }

  method setTargetType ($tt) {
    if (any { $tt =~ /$_/ } $self->GeneralKeys->values) {
      $targetType = $tt;
    }
  }

  method _compute_total_buffs ($primarySummarizer, $secondarySummarizer) {
    my $primary   = $primarySummarizer->buffValues;
    my $secondary = $secondarySummarizer->buffValues;
    $self->logger->debug("primary is " . Data::Printer::np($primary));
    $self->logger->debug("secondary is " . Data::Printer::np($secondary));

    foreach my $category (keys %$primary) {
      $self->logger->debug("computing buff total for category $category");
      foreach my $type (keys %{ $primary->{$category} }) {
        $self->logger->debug(
          "computing buff total for category $category type $type");
        $buffValues->{$category}->{$type} =
          ($primary->{$category}->{$type}   // 0) +
          ($secondary->{$category}->{$type} // 0);
      }
    }
  }

  method _compute_total_debuffs ($primarySummarizer, $secondarySummarizer) {
    my $primary   = $primarySummarizer->debuffValues;
    my $secondary = $secondarySummarizer->debuffValues;

    foreach my $category (keys %$primary) {
      foreach my $type (keys %{ $primary->{$category} }) {
        $debuffValues->{$category}{$type} =
          ($primary->{$category}{$type}   // 0) +
          ($secondary->{$category}{$type} // 0);
      }
    }
  }

  method updateBuffs (
    $generalType            = '',
    $primaryAscending       = 'red5',
    $primaryCovenantLevel   = 'civilization',
    $primarySpecialties     = ['gold', 'gold', 'gold', 'gold',],
    $secondaryCovenantLevel = 'civilization',
    $secondarySpecialties   = ['gold', 'gold', 'gold', 'gold',],

    $keepLevel      = 40,
    $primaryLevel   = 45,
    $secondaryLevel = 45,
  ) {

    my $primarySummarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager => $rootManager,
      general     => $primary,
      isPrimary   => 1,
      targetType  => $generalType,

      ascendingLevel => $primaryAscending,
      covenantLevel  => $primaryCovenantLevel,
      specialty1     => $primarySpecialties->[0],
      specialty2     => $primarySpecialties->[1],
      specialty3     => $primarySpecialties->[2],
      specialty4     => $primarySpecialties->[3],

      generalLevel => $primaryLevel,
      keepLevel    => $keepLevel,
    );
    my $secondarySummarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager => $rootManager,
      general     => $secondary,
      isPrimary   => 1,
      targetType  => $generalType,

      ascendingLevel => 'none',
      covenantLevel  => $secondaryCovenantLevel,
      specialty1     => $secondarySpecialties->[0],
      specialty2     => $secondarySpecialties->[1],
      specialty3     => $secondarySpecialties->[2],
      specialty4     => $secondarySpecialties->[3],

      generalLevel => $secondaryLevel,
      keepLevel    => $keepLevel,
    );

    # Update buffs for both summarizers
    $primarySummarizer->updateBuffs();
    $secondarySummarizer->updateBuffs();

    # Update debuffs for both summarizers
    $primarySummarizer->updateDebuffs();
    $secondarySummarizer->updateDebuffs();

    $self->_compute_total_buffs($primarySummarizer, $secondarySummarizer);
    $self->_compute_total_debuffs($primarySummarizer, $secondarySummarizer);

  }

  # Method to convert to hash
  method to_hash {
    $self->logger->debug("buffvalues is " . Data::Printer::np($buffValues));
    my $tt = $targetType;
    if ($targetType =~ /(\w+)_(specialist)/) {
      $tt = $targetType =~ s/(\w+)_(specialist)/$1 Troops/r;
      $tt =~ s/^(\w)/\U$1/;
      if ($tt eq 'Siege Troops') {
        $tt = 'Siege Machines';
      }
      if ($tt eq 'Wall Troops') {
        $tt = 'Overall';
      }
      $self->logger->debug("looking for type $tt");
    }
    return {
      primary              => $primary,
      secondary            => $secondary,
      marchbuff            => $buffValues->{$tt}->{'March Size Capacity'},
      attackbuff           => $buffValues->{$tt}->{'Attack'},
      defensebuff          => $buffValues->{$tt}->{'Defense'},
      hpbuff               => $buffValues->{$tt}->{'Defense'},
      groundattackdebuff   => $debuffValues->{'Ground Troops'}->{'Attack'},
      grounddefensedebuff  => $debuffValues->{'Ground Troops'}->{'Defense'},
      groundhpdebuff       => $debuffValues->{'Ground Troops'}->{'HP'},
      mountedattackdebuff  => $debuffValues->{'Mounted Troops'}->{'Attack'},
      mounteddefensedebuff => $debuffValues->{'Mounted Troops'}->{'Defense'},
      mountedhpdebuff      => $debuffValues->{'Mounted Troops'}->{'HP'},
      rangedattackdebuff   => $debuffValues->{'Ranged Troops'}->{'Attack'},
      rangeddefensedebuff  => $debuffValues->{'Ranged Troops'}->{'Defense'},
      rangedhpdebuff       => $debuffValues->{'Ranged Troops'}->{'HP'},
      siegeattackdebuff    => $debuffValues->{'Siege Machines'}->{'Attack'},
      siegedefensedebuff   => $debuffValues->{'Siege Machines'}->{'Defense'},
      siegehpdebuff        => $debuffValues->{'Siege Machines'}->{'HP'},
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

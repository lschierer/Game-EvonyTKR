use v5.42.0;
use experimental qw(class);
use utf8::all;
require JSON::PP;
require Scalar::Util;

class Game::EvonyTKR::Model::General::Pair :
  isa(Game::EvonyTKR::Shared::Constants) {
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
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
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
    $primarySummarizer->updateBuffs();
    $secondarySummarizer->updateBuffs();
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
    $primarySummarizer->updateDebuffs();
    $secondarySummarizer->updateDebuffs();
    my $primary   = $primarySummarizer->debuffValues;
    my $secondary = $secondarySummarizer->debuffValues;

    foreach my $category (keys %$primary) {
      $self->logger->debug("calc debuffs for $category");
      foreach my $type (keys %{ $primary->{$category} }) {
        $self->logger->debug("calc debuffs for $type");
        $debuffValues->{$category}->{$type} =
          $primary->{$category}->{$type} + $secondary->{$category}->{$type};
      }
    }
  }

  method updateBuffsAndDebuffs (
    $generalType            = '',
    $primaryAscending       = 'red5',
    $primaryCovenantLevel   = 'civilization',
    $primarySpecialties     = ['gold', 'gold', 'gold', 'gold',],
    $secondaryCovenantLevel = 'civilization',
    $secondarySpecialties   = ['gold', 'gold', 'gold', 'gold',],
    $buffActivation         = 'Overall',

    $keepLevel      = 40,
    $primaryLevel   = 45,
    $secondaryLevel = 45,
  ) {
    if (!$primary) {
      $self->logger->logcroak("NO PRIMARY DEFINED FOR PAIR");
      return;
    }
    if (!$secondary) {
      $self->logger->logcroak("NO SECONDARY DEFINED FOR PAIR");
      return;
    }

    my $primarySummarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager    => $rootManager,
      general        => $primary,
      isPrimary      => 1,
      targetType     => $generalType,
      activationType => $buffActivation,

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
      rootManager    => $rootManager,
      general        => $secondary,
      isPrimary      => 1,
      targetType     => $generalType,
      activationType => $buffActivation,

      ascendingLevel => 'none',
      covenantLevel  => $secondaryCovenantLevel,
      specialty1     => $secondarySpecialties->[0],
      specialty2     => $secondarySpecialties->[1],
      specialty3     => $secondarySpecialties->[2],
      specialty4     => $secondarySpecialties->[3],

      generalLevel => $secondaryLevel,
      keepLevel    => $keepLevel,
    );

    $self->_compute_total_buffs($primarySummarizer, $secondarySummarizer);
    $self->_compute_total_debuffs($primarySummarizer, $secondarySummarizer);
    $self->logger->debug(sprintf(
      'updated pair %s/%s buffs %s debuffs %s.',
      $primary->name,                 $secondary->name,
      Data::Printer::np($buffValues), Data::Printer::np($debuffValues),
    ));

  }

  # Method to convert to hash
  method to_hash {

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
      marchbuff            => $buffValues->{$tt}->{'March Size'},
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

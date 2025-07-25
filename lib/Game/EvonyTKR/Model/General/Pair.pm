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
  use Readonly;
  use overload
    '""'       => \&as_string,
    'fallback' => 1;

  field $primary : reader : param;
  field $secondary : reader : param;
  field $rootManager : reader = undef;
  field $targetType : reader;

  field %total_computed_buffs_cache;
  field $current_cache_key :reader :writer;

  ADJUST {
    $targetType =
      ref($primary->type) eq 'ARRAY' ? $primary->type->[0] : $primary->type;
  }


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

  method buffValues {
    unless (defined $current_cache_key) {
      return {}
    }
      return $total_computed_buffs_cache{$current_cache_key}->{buffValues} // {};
  }
  method debuffValues {
      unless (defined $current_cache_key) {
        return {}
      }
      return $total_computed_buffs_cache{$current_cache_key}->{debuffValues} // {};
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
        $total_computed_buffs_cache{$current_cache_key}->{buffValues}->{$category}->{$type} =
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
        $total_computed_buffs_cache{$current_cache_key}->{debuffValues}->{$category}->{$type} =
          $primary->{$category}->{$type} + $secondary->{$category}->{$type};
      }
    }
  }

  method updateBuffsAndDebuffs (
    $generalType            = '',  # this was determined by which table was requested
    $primaryAscending       = 'red5',
    $primaryCovenantLevel   = 'civilization',
    $primarySpecialties     = ['gold', 'gold', 'gold', 'gold',],
    $secondaryCovenantLevel = 'civilization',
    $secondarySpecialties   = ['gold', 'gold', 'gold', 'gold',],
    $buffActivation         = 'Overall', # this was determined by which table was requested

    $keepLevel      = 40,    # I have plans for this value but do not use it yet
    $primaryLevel   = 45,    # I have plans for this value but do not use it yet
    $secondaryLevel = 45,    # I have plans for this value but do not use it yet
  ) {
    if (!$primary) {
      $self->logger->logcroak("NO PRIMARY DEFINED FOR PAIR");
      return;
    }
    if (!$secondary) {
      $self->logger->logcroak("NO SECONDARY DEFINED FOR PAIR");
      return;
    }
    $current_cache_key = sprintf('%s-%s-%s-%s-%s-%s-%s',
        $generalType, $buffActivation,
        $primaryAscending, $primaryCovenantLevel, join('-', @$primarySpecialties),
        $secondaryCovenantLevel, join('-', @$secondarySpecialties)
    );

    if (exists $total_computed_buffs_cache{$current_cache_key}) {
      $self->logger->info(sprintf('Cache hit for "%s". There are "%s" cache entries.',
      $current_cache_key, scalar keys %total_computed_buffs_cache));
      return;
    } else{
      $self->logger->info(sprintf('Cache miss for "%s". There are "%s" cache entries.',
      $current_cache_key, scalar keys %total_computed_buffs_cache));
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
      Data::Printer::np($self->buffValues), Data::Printer::np($self->debuffValues),
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
      marchbuff            => $self->buffValues->{$tt}->{'March Size'},
      attackbuff           => $self->buffValues->{$tt}->{'Attack'},
      defensebuff          => $self->buffValues->{$tt}->{'Defense'},
      hpbuff               => $self->buffValues->{$tt}->{'HP'},
      groundattackdebuff   => $self->debuffValues->{'Ground Troops'}->{'Attack'},
      grounddefensedebuff  => $self->debuffValues->{'Ground Troops'}->{'Defense'},
      groundhpdebuff       => $self->debuffValues->{'Ground Troops'}->{'HP'},
      mountedattackdebuff  => $self->debuffValues->{'Mounted Troops'}->{'Attack'},
      mounteddefensedebuff => $self->debuffValues->{'Mounted Troops'}->{'Defense'},
      mountedhpdebuff      => $self->debuffValues->{'Mounted Troops'}->{'HP'},
      rangedattackdebuff   => $self->debuffValues->{'Ranged Troops'}->{'Attack'},
      rangeddefensedebuff  => $self->debuffValues->{'Ranged Troops'}->{'Defense'},
      rangedhpdebuff       => $self->debuffValues->{'Ranged Troops'}->{'HP'},
      siegeattackdebuff    => $self->debuffValues->{'Siege Machines'}->{'Attack'},
      siegedefensedebuff   => $self->debuffValues->{'Siege Machines'}->{'Defense'},
      siegehpdebuff        => $self->debuffValues->{'Siege Machines'}->{'HP'},
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

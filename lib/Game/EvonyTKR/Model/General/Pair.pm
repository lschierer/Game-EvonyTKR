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

  field $primary    : reader : param;
  field $secondary  : reader : param;
  field $targetType : reader;

  field %total_computed_buffs_cache;
  field $current_cache_key : reader : writer;

  ADJUST {
    $targetType =
      ref($primary->type) eq 'ARRAY' ? $primary->type->[0] : $primary->type;
  }

  method setTargetType ($tt) {
    if (any { $tt =~ /$_/ } $self->GeneralKeys->values) {
      $targetType = $tt;
    }
  }

  method buffValues {
    unless (defined $current_cache_key) {
      return {};
    }
    return $total_computed_buffs_cache{$current_cache_key}->{buffValues} // {};
  }

  method debuffValues {
    unless (defined $current_cache_key) {
      return {};
    }
    return $total_computed_buffs_cache{$current_cache_key}->{debuffValues}
      // {};
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
        $total_computed_buffs_cache{$current_cache_key}->{buffValues}
          ->{$category}->{$type} =
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
        $total_computed_buffs_cache{$current_cache_key}->{debuffValues}
          ->{$category}->{$type} =
          $primary->{$category}->{$type} + $secondary->{$category}->{$type};
      }
    }
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
      primary             => $primary,
      secondary           => $secondary,
      marchbuff           => $self->buffValues->{$tt}->{'March Size'},
      attackbuff          => $self->buffValues->{$tt}->{'Attack'},
      defensebuff         => $self->buffValues->{$tt}->{'Defense'},
      hpbuff              => $self->buffValues->{$tt}->{'HP'},
      groundattackdebuff  => $self->debuffValues->{'Ground Troops'}->{'Attack'},
      grounddefensedebuff =>
        $self->debuffValues->{'Ground Troops'}->{'Defense'},
      groundhpdebuff      => $self->debuffValues->{'Ground Troops'}->{'HP'},
      mountedattackdebuff =>
        $self->debuffValues->{'Mounted Troops'}->{'Attack'},
      mounteddefensedebuff =>
        $self->debuffValues->{'Mounted Troops'}->{'Defense'},
      mountedhpdebuff     => $self->debuffValues->{'Mounted Troops'}->{'HP'},
      rangedattackdebuff  => $self->debuffValues->{'Ranged Troops'}->{'Attack'},
      rangeddefensedebuff =>
        $self->debuffValues->{'Ranged Troops'}->{'Defense'},
      rangedhpdebuff     => $self->debuffValues->{'Ranged Troops'}->{'HP'},
      siegeattackdebuff  => $self->debuffValues->{'Siege Machines'}->{'Attack'},
      siegedefensedebuff =>
        $self->debuffValues->{'Siege Machines'}->{'Defense'},
      siegehpdebuff => $self->debuffValues->{'Siege Machines'}->{'HP'},
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

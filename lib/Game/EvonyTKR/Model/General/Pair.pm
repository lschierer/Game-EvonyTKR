use v5.42.0;
use experimental qw(class);
use utf8::all;
require JSON::PP;
require Scalar::Util;

require Game::EvonyTKR::Model::General;

package Game::EvonyTKR::Model::General::Pair {
  use Mojo::Base 'Game::EvonyTKR::Model::Base';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use UUID           qw(uuid5);
  use List::AllUtils qw( any none );
  use File::FindLib 'lib';
  use Carp;
  use overload
    '""'       => \&to_string,
    '<=>'      => \&compare,
    'cmp'      => \&compare,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  has ['primary', 'secondary', 'type'] => undef;

  sub to_key ($self) {
    my $key = sprintf('%s/%s/%s',
      $self->type,
      $self->normalize($self->primary->name),
      $self->normalize($self->secondary->name),
    );
    $key = lc($key);
    $key =~ s/ /_/g;
    return $key;
  }

  sub to_hash ($self) {
    my $h = {
      primary   => $self->primary->to_hash(),
      secondary => $self->secondary->to_hash(),
      type      => $self->type,
    };
    return $h;
  }

  sub to_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub to_wire_hash ($self) {
    my $h = {
      primary   => { name => $self->primary->name },
      secondary => { name => $self->secondary->name },
      type      => $self->type,
    };
    return $h;
  }

  sub from_wire_hash ($class, $h) {
    my $logger = Game::EvonyTKR::Log::Config->get_logger();

    my $primary_name = ref($h->{primary}) eq 'HASH'
      ? $h->{primary}->{name}
      : $h->{primary};
    my $secondary_name = ref($h->{secondary}) eq 'HASH'
      ? $h->{secondary}->{name}
      : $h->{secondary};

    unless (defined($primary_name) && length($primary_name)) {
      $logger->error('hash object must contain a primary with a name in it.');
      return;
    }

    state $general_helper;
    unless ($general_helper) {
      eval {
        $general_helper = Mojo::Base->new->with_roles(
          'Game::EvonyTKR::Log::Config',
          'Game::EvonyTKR::Role::Common',
          'Game::EvonyTKR::Controller::Role::Generals'
        );
      } or do {
        $logger->error(
          sprintf('eval failed; cannot define general helper: "%s"', $@));
        return;
      };
    }
    my $primary = $general_helper->get_general($primary_name);
    unless ($primary) {
      $logger->error(
        sprintf('cannot retrieve general for %s when creating a pair.',
          $primary_name)
      );
      return;
    }
    my $secondary = $general_helper->get_general($secondary_name);
    unless ($secondary) {
      $logger->error(
        sprintf('cannot retrieve general for %s when creating a pair.',
          $secondary_name)
      );
      return;
    }

    unless ($h
      && ref($h)
      && ref($h) eq 'HASH'
      && exists $h->{type}
      && length($h->{type})) {
      $logger->error('hash object must contain a type.');
      return;
    }

    return $class->new(
      primary   => $primary,
      secondary => $secondary,
      type      => $h->{type},
    );
  }

  sub compare ($self, $other, $swapped = undef) {
    my ($a, $b) = $swapped ? ($other, $self) : ($self, $other);
    if ($a
      && Scalar::Util::blessed($a) =~ /Game::EvonyTKR::Model::General::Pair/) {
      if ($b
        && Scalar::Util::blessed($b) =~ /Game::EvonyTKR::Model::General::Pair/)
      {
        return $a->primary->name cmp $b->primary->name
          || $a->secondary->name cmp $b->secondary->name;
      }
      else {
        return $a->primary->name cmp "$b"
          || $a->secondary->name cmp "$b";
      }
    }
    elsif ($b
      && Scalar::Util::blessed($b) =~ /Game::EvonyTKR::Model::General::Pair/) {
      return "$a" cmp $b->primary->name
        || "$a" cmp $b->secondary->name;
    }
    else {
      return "$a" cmp "$b";
    }
  }

  sub _isTrue ($self, $other = undef, $swap = undef) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa(__PACKAGE__);
  }
}
1;
__END__
  use List::AllUtils qw( all any none );
  use Readonly;
  use Carp;
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

use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Math::Round;
require JSON::PP;
use namespace::autoclean;

package Game::EvonyTKR::Model::BasicAttribute {
  use Mojo::Base 'Game::EvonyTKR::Model::Base';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use List::AllUtils qw( any none );
  use Scalar::Util   qw(blessed);
  use Data::Printer;
  use Const::Fast;
  use File::FindLib 'lib';
  use Carp;
  use overload
    '<=>'      => \&_comparison,
    '=='       => \&_equality,
    'eq'       => \&_equality,
    '!='       => \&_inequality,
    '""'       => \&as_string,
    '.'        => \&concat,
    "fallback" => 0;

  has 'attribute_name'      => '';
  has ['base', 'increment'] => 0;
  has 'EvansAdjustment'     => 2.4867;
  has 'BasicAESAdjustment'  => sub {
    const my $hash = {
      'none'    => 0,
      'purple1' => 0,
      'purple2' => 0,
      'purple3' => 0,
      'purple4' => 0,
      'purple5' => 0,
      'red1'    => 10,
      'red2'    => 20,
      'red3'    => 30,
      'red4'    => 40,
      'red5'    => 50,
    };
    return $hash;
  };

  sub validate ($self) {
    my @errors;
    unless (Scalar::Util::looks_like_number($self->base) && $self->base >= 0) {
      push @errors,
        sprintf('base must be a positive number, not "%s"', $self->base);
    }
    unless (Scalar::Util::looks_like_number($self->increment)
      && $self->increment >= 0) {
      push @errors,
        sprintf('increment must be a positive number, not "%s"',
        $self->increment);
    }
    unless ((not Scalar::Util::looks_like_number($self->attribute_name))
      && length($self->attribute_name)) {
      push @errors,
        sprintf('attribute_name is a required string, not "%s"',
        $self->attribute_name);
    }
    unless (any { $_ =~ /$self->attribute_name/i } $self->BasicAttributeTypes) {
      push @errors,
        sprintf('attribute_name must be one of %s, not "%s"',
        join ', ', $self->BasicAttributeTypes);
    }

    if (scalar @errors >= 1) {
      $self->logger->logcroak(join ', ', @errors);
    }
  }

  sub setBase ($self, $newBase = 0) {
    my @errors = ();
    Scalar::Util::looks_like_number($newBase)
      or push @errors => "base must be a number, not $newBase";
    unless ($newBase >= 0) {
      push @errors, "base must be positive, not $newBase";
    }
    if (scalar @errors >= 1) {
      $self->logger->logerror(join(', ', @errors));
      return;
    }
    else {
      $self->base = $newBase;
    }
  }

  sub setIncrement ($self, $newIncrement = 0) {
    my @errors = ();

    Scalar::Util::looks_like_number($newIncrement)
      or push @errors => "increment must be a number, not $newIncrement";
    unless ($newIncrement >= 0) {
      push @errors, "increment must be positive, not $newIncrement";
    }
    if (scalar @errors >= 1) {
      $self->logger->error(join(', ', @errors));
      return;
    }
    else {
      $self->increment = $newIncrement;
    }
  }

  # Calculate total value
  # =ROUND(((900*0.1)+(((L131+(M131*2.4867*44))*1.1+50+520)-900)*0.2)/100,3)
  # https://evonyguidewiki.com/en/general-cultivate-en/
  sub total ($self, $level = 1, $stars = 'none', $name = "GeneralName",) {
    my $AES_adjustment = 0;
    # This cultivation value is the maximum realistic value possible.
    my $cultivation = 520;
    if (exists $self->BasicAESAdjustment->{$stars}) {
      $AES_adjustment = $self->BasicAESAdjustment->{$stars};
    }
    # The EvansAdjustment may be intended to partially account for the variable
    # amount of attribute increase per star a general gets for the first five
    # stars.  This varies per general and does not seem to be tracked by anyone.
    my $sa     = $self->BasicAESAdjustment->{$stars};
    my $result = Math::Round::round((
        (900 * 0.1) + (((
              $self->base + ($self->increment * $self->EvansAdjustment * $level)
            ) * 1.1 + $sa + $cultivation
          ) - 900
        ) * 0.2
      ) / 100,
      3
    );
    $self->logger->debug(sprintf(
      'found total basic %s attribute value of "%s" for "%s"',
      $self->attribute_name, $result, $name
    ));
    return $result;
  }

  sub _comparison ($self, $other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttribute') {
      my $od = Data::Printer::p $other;
      $self->logger->error(sprintf(
        'Game::EvonyTKR::Model::BasicAttribute '
          . 'comparison operator cannot take a %s',
        $od
      ));
      croak(sprintf(
        'Game::EvonyTKR::Model::BasicAttributes '
          . 'comparison operator cannot take a %s',
        $od
      ));
      return;
    }
    else {
      my $mt = $self->total();
      my $ot = $other->total();
      if ($self->attribute_name() cmp $other->attribute_name()) {
        $self->logger->warn(sprintf(
          'you probably did not intend to compare '
            . 'to different attributes: %s %s',
          $self->attribute_name(),
          $other->attribute_name()
        ));
        return $self->attribute_name() cmp $other->attribute_name();
      }
      return $mt <=> $ot;
    }
  }

  sub _equality ($self, $other, $swap = 0) {
    my $otherClass = blessed $other;
    if (defined($otherClass) && length($otherClass) > 0) {
      my @classList = split(/::/, $otherClass);
      if ($classList[2] ne 'BasicAttribute') {
        my $od = Data::Printer::p $other;
        $self->logger->error(sprintf(
          'Game::EvonyTKR::Model::BasicAttribute '
            . 'equality operator cannot take a %s',
          $od
        ));
        croak(sprintf(
          'Game::EvonyTKR::Model::BasicAttributes '
            . 'equality operator cannot take a %s',
          $od
        ));
        return;
      }
      else {
        my $mt = $self->total();
        my $ot = $other->total();
        return (($mt == $ot)
            and ($self->attribute_name() eq $other->attribute_name()));
      }
    }
    else {
      return 0;
    }

  }

  sub _inequality ($self, $other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttribute') {
      my $od = Data::Printer::p $other;
      $self->logger->error(sprintf(
        'Game::EvonyTKR::Model::BasicAttribute '
          . 'inequality operator cannot take a %s',
        $od
      ));
      croak(sprintf(
        'Game::EvonyTKR::Model::BasicAttributes '
          . 'inequality operator cannot take a %s',
        $od
      ));
      return;
    }
    else {
      my $mt = $self->total();
      my $ot = $other->total();
      return (
             ($mt != $ot)
          or ($self->attribute_name() ne $other->attribute_name())
      );
    }
  }

  sub to_hash ($self) {
    return {
      base      => $self->base,
      increment => $self->increment,
    };
  }

  sub to_wire_hash ($self) {
    return {
      _v             => 1,
      attribute_name => $self->attribute_name,
      base           => $self->base,
      increment      => $self->increment,
    };
  }

  sub from_wire_hash ($class, $w) {
    return $class->new(
      attribute_name => $w->{attribute_name},
      base           => $w->{base}      // 0,
      increment      => $w->{increment} // 0,
    );
  }

  # Method for JSON serialization
  sub TO_JSON ($self) {
    return $self->to_wire_hash();
  }

  # Stringification method using JSON
  sub as_string ($self) {
    my $json =
      JSON::PP->new->utf8->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub concat($self, $other, $swap) {
    if ($swap) {
      return $other . $self->as_string();
    }
    else {
      return $self->as_string() . $other;
    }
  }

}
1;
__END__
# ABSTRACT: Stores a single Basic Attribute of a Game::EvonyTKR::Model::General

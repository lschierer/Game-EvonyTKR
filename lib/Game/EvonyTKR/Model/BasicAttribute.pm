use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Math::Round;
require JSON::PP;
require Game::EvonyTKR::Shared::Constants::BuffConstants;
require Game::EvonyTKR::Util::BasicAttribute;
use namespace::autoclean;

package Game::EvonyTKR::Model::BasicAttribute {
  use Mojo::Base -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Util::BasicAttribute',             -role;
  use Carp;
  use List::AllUtils qw( any none );
  use Scalar::Util   qw(blessed);
  use Data::Printer;
  use Const::Fast;
  use File::FindLib 'lib';
  use overload
    '<=>'      => \&_comparison,
    '=='       => \&_equality,
    'eq'       => \&_equality,
    '!='       => \&_inequality,
    '""'       => \&as_string,
    '.'        => \&concat,
    "fallback" => 0;

  my $logger = Game::EvonyTKR::Log::Config->logger();

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
    $logger->debug(sprintf(
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
      $logger->error(sprintf(
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
        $logger->warn(sprintf(
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
        $logger->error(sprintf(
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
      $logger->error(sprintf(
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

  # Method for JSON serialization
  sub TO_JSON ($self) {
    return $self->to_hash();
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

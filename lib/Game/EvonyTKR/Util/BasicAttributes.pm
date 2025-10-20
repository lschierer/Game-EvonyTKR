use v5.42.0;
use experimental qw(class);
use utf8::all;
require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;

use File::FindLib 'lib';
require Game::EvonyTKR::Shared::Constants;
package Game::EvonyTKR::Util::BasicAttributes {

  use Carp;
  use List::AllUtils qw( any none first );
  use Types::Common  qw( t is_Num is_Str);
  use Data::Printer;
  require Game::EvonyTKR::Model::BasicAttribute;
  require JSON::PP;
  use namespace::autoclean;
# VERSION

  use File::FindLib 'lib';
  my constants = Game::EvonyTKR::Shared::Constants->new();

  my $logger = Game::EvonyTKR::Log::Config->logger();

  sub setAttribute($self, $bas, $attributeName, $newAttribute) {
    if (none { $_ =~ $attributeName } @attributeNames) {
      $logger->error(sprintf(
        'attributeName must be one of %s, not %s',
        Data::Printer::np($self->AttributeValues),
        $attributeName,
      ));
      return;
    }

    unless(ref($newAttribute) && $newAttribute->isa('Game::EvonyTKR::Model::BasicAttribute')) {
      $logger->error(sprintf(
        'newAttribute must be a %s not a %s',
        'Game::EvonyTKR::Model::BasicAttribute',
        blessed $newAttribute
      ));
      return;
    }

    unless(ref($bas) && $bas->isa('Game::EvonyTKR::Model::BasicAttributes')){
      $logger->error(sprintf(
        'second paramter to setAttribute must be a "Game::EvonyTKR::Model::BasicAttribute" not %s',
        blessed($bas),
      ));
      return;
    }

    if (not exists $self->attributes()->{$attributeName}) {
      $logger->error(sprintf(
'$self->attributes()->{$attributeName} does not exist for $attributeName %s',
        $attributeName));
      return;
    }

    $self->attributes()->{$attributeName} = $newAttribute;

  }

  method total($level = 1, $stars = 'none', $name = "GeneralName") {
    my $total = $self->attack()->total($level, $stars, $name, 'attack');
    $total += $self->leadership->total($level, $stars, $name, 'leadership');
    $total += $self->defense->total($level, $stars, $name, 'defense');
    $total += $self->politics->total($level, $stars, $name, 'politics');
    return $total;
  }

  method score(
    $level      = 1,
    $stars      = 'none',
    $name       = "GeneralName",
    $multiplier = 0
  ) {
    my $score =
      $self->attack->score($level, $stars, $name, $multiplier, 'attack');
    $score += $self->leadership->score($level, $stars, $name, $multiplier,
      'leadership');
    $score +=
      $self->defense->score($level, $stars, $name, $multiplier, 'defense');
    $score +=
      $self->politics->score($level, $stars, $name, $multiplier, 'politics');
    return $score;
  }

  method _comparison ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttributes') {
      my $od = Data::Printer::p $other;
      $logger->error(sprintf(
        'Game::EvonyTKR::Model::BasicAttributes '
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
      return $mt <=> $ot;
    }
  }

  method _equality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttributes') {
      my $od = Data::Printer::p $other;
      $logger->error(sprintf(
        'Game::EvonyTKR::Model::BasicAttributes '
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
      return $mt == $ot;
    }
  }

  method _inequality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttributes') {
      my $od = Data::Printer::p $other;
      $logger->error(sprintf(
        'Game::EvonyTKR::Model::BasicAttributes '
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
      return $mt != $ot;
    }
  }

  method getReaderForAttribute($attrib) {
    if ($attrib =~ /attack/i) {
      return $self->attack();
    }
    elsif ($attrib =~ /leadership/i) {
      return $self->leadership();
    }
    elsif ($attrib =~ /defense/i) {
      return $self->defense();
    }
    elsif ($attrib =~ /politics/i) {
      return $self->politics();
    }
    else {
      $logger->error('invalid attribute requested');
      croak('invalid attribute requested');
      return;
    }
  }

  method to_hash {
    return {
      attack     => $attributes->{attack},
      defense    => $attributes->{defense},
      leadership => $attributes->{leadership},
      politics   => $attributes->{politics},
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
__END__
# ABSTRACT: Stores the collection of what Evony refers to as the Basic Attributes for a Game::EvonyTKR::Model::General

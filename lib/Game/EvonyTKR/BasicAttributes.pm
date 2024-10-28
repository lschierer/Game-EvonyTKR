use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::BasicAttributes : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::BasicAttributes
  use Croak;
  use Types::Common  qw( t is_Num is_Str is_Int);
  use List::AllUtils qw( any none );
  use Data::Printer;
  use Game::EvonyTKR::BasicAttribute;
  use namespace::autoclean;
# VERSION

  use File::FindLib 'lib';
  use overload
    '<=>' => \&_comparison,
    '=='  => \&_equality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

  field $attack : reader =
    Game::EvonyTKR::BasicAttribute->new(attribute_name => 'attack',);

  field $leadership : reader =
    Game::EvonyTKR::BasicAttribute->new(attribute_name => 'leadershp',);

  field $defense : reader =
    Game::EvonyTKR::BasicAttribute->new(attribute_name => 'defense',);

  field $politics : reader =
    Game::EvonyTKR::BasicAttribute->new(attribute_name => 'politics',);

  method total($level = 1, $stars = 'none', $name = "GeneralName") {
    my $total = $self->attack()
      ->total($level = 1, $stars = 'none', $name = "GeneralName", 'attack');
    $total += $self->leadership->total(
      $level = 1,
      $stars = 'none',
      $name  = "GeneralName", 'leadership'
    );
    $total += $self->defense->total(
      $level = 1,
      $stars = 'none',
      $name  = "GeneralName", 'defense'
    );
    $total += $self->politics->total(
      $level = 1,
      $stars = 'none',
      $name  = "GeneralName", 'politics'
    );
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
      $self->logger()
        ->logcroak(
        "Game::EvonyTKR::BasicAttributes comparison operator cannot take a $od"
        );
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
      $self->logger()
        ->logcroak(
        "Game::EvonyTKR::BasicAttributes equality operator cannot take a $od");
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
      $self->logger()
        ->logcroak(
        "Game::EvonyTKR::BasicAttributes inequality operator cannot take a $od"
        );
    }
    else {
      my $mt = $self->total();
      my $ot = $other->total();
      return $mt != $ot;
    }
  }

  method _toHashRef($verbose = 0) {
    my $returnRef = {
      leadership => $self->leadership()->_toHashRef($verbose),
      attack     => $self->attack()->_toHashRef($verbose),
      defense    => $self->defense()->_toHashRef($verbose),
      politic    => $self->politics()->_toHashRef($verbose),
    } return $returnRef;
  }

  method TO_JSON {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $self->toHashRef();
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

}
1;
__END__
# ABSTRACT: Stores the collection of what Evony refers to as the Basic Attributes for a Game::EvonyTKR::General

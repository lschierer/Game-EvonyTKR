use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::BasicAttributes : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::BasicAttributes
  use Carp;
  use List::AllUtils qw( any none );
  use Types::Common  qw( t is_Num is_Str);
  use Data::Printer;
  require Game::EvonyTKR::BasicAttribute;
  use namespace::autoclean;
# VERSION

  use File::FindLib 'lib';
  use overload
    '<=>' => \&_comparison,
    '=='  => \&_equality,
    '!='  => \&_inequality,
    '""'  => \&TO_JSON;

  field $attributes : reader;

  ADJUST {
    my $an = $self->AttributeNames();
    $attributes->{attack} = Game::EvonyTKR::BasicAttribute->new(
      attribute_name => $an->closest_match('attack'),);
    $attributes->{defense} = Game::EvonyTKR::BasicAttribute->new(
      attribute_name => $an->closest_match('defense'),);
    $attributes->{leadership} = Game::EvonyTKR::BasicAttribute->new(
      attribute_name => $an->closest_match('leadership'),);
    $attributes->{politics} = Game::EvonyTKR::BasicAttribute->new(
      attribute_name => $an->closest_match('politics'),);

  }

  method attack {
    return $self->attributes()->{attack};
  }

  method leadership {
    return $self->attributes()->{leadership};
  }

  method defense {
    return $self->attributes()->{defense};
  }

  method politics {
    return $self->attributes()->{politics};
  }

  field @attributeNames = qw(attack leadership defense politics);

  method setAttribute($attributeName, $newAttribute) {
    if (none { $_ =~ $attributeName } @attributeNames) {
      $self->logger()->error(sprintf(
        'attributeName must be one of %s, not %s',
        Data::Printer::np($self->AttributeNames->values()),
        $attributeName,
      ));
      return;
    }

    my @nac = split(/::/, blessed $newAttribute);
    if ($nac[2] ne 'BasicAttribute') {
      $self->logger()->error(sprintf(
        'newAttribute must be a %s not a %s',
        'Game::EvonyTKR::BasicAttribute',
        blessed $newAttribute
      ));
      return;
    }

    if (not exists $self->attributes()->{$attributeName}) {
      $self->logger()
        ->error(sprintf(
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
      $self->logger()->logcroak("invalid attribute requested");
    }
  }

  method toHashRef {
    return {
      attack     => $attributes->{attack},
      defense    => $attributes->{defense},
      leadership => $attributes->{leadership},
      politics   => $attributes->{politics},
    };
  }

  method TO_JSON {
    return $self->toHashRef();
  }

}
1;
__END__
# ABSTRACT: Stores the collection of what Evony refers to as the Basic Attributes for a Game::EvonyTKR::General

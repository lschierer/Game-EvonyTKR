use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::BasicAttribute : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::BasicAttribute
  use Carp;
  use Types::Common  qw( t is_Num is_Str is_Int);
  use List::AllUtils qw( any none );
  use Data::Printer;
  use Hash::Util;
  use namespace::autoclean;
# VERSION

  use File::FindLib 'lib';
  use overload
    '<=>' => \&_comparison,
    '=='  => \&_equality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

  field $base : reader : param //= 0;

  field $increment : reader : param //= 0;

  field $attribute_name : reader : param;

  field @AttributeNames : reader = qw(
    attack
    defense
    leadership
    politics
  );

  field %BasicAESAdjustment = (
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
  );

  ADJUST {
    Hash::Util::lock_keys(%BasicAESAdjustment);
    my @errors = ();

    my $type = t('PositiveOrZeroNum');
    is_Num($base)
      or push @errors => "base must be a number, not $base";
    $type->check($base)
      or push @errors => "base must be positive, not $base";

    $type = t('PositiveOrZeroNum');
    is_Num($increment)
      or push @errors => "increment must be a number, not $increment";
    $type->check($increment)
      or push @errors => "increment must be positive, not $increment";

    is_Str($attribute_name)
      or push @errors => "attribute name must be a string, not $attribute_name";
    if (none { $_ eq $attribute_name } @AttributeNames) {
      push @errors,
        "attribute name must be one of " . Data::Printer::np @AttributeNames;
    }
    if (scalar @errors >= 1) {
      croak(join(', ' => @errors));
    }
  }

  method setBase($newBase = 0) {
    my @errors = ();
    my $type   = t('PositiveOrZeroNum');
    is_Num($newBase)
      or push @errors => "base must be a number, not $newBase";
    $type->check($newBase)
      or push @errors => "base must be positive, not $newBase";
    if (scalar @errors >= 1) {
      $self->logger()->logerror(join(', ', @errors));
      return;
    }
    else {
      $base = $newBase;
    }
  }

  method setIncrement($newIncrement = 0) {
    my @errors = ();
    my $type   = t('PositiveOrZeroNum');
    is_Num($newIncrement)
      or push @errors => "increment must be a number, not $newIncrement";
    $type->check($newIncrement)
      or push @errors => "increment must be positive, not $newIncrement";
    if (scalar @errors >= 1) {
      $self->logger()->logerror(join(', ', @errors));
      return;
    }
    else {
      $increment = $newIncrement;
    }
  }

  #https://evonyguidewiki.com/en/general-cultivate-en/
  method total(
    $level  = 1,
    $stars  = 'none',
    $name   = "GeneralName",
    $attrib = "Attribute"
  ) {
    my $AES_adjustment = 0;
    if (exists $BasicAESAdjustment{$stars}) {
      $AES_adjustment = $BasicAESAdjustment{$stars};
    }
    my $step = $level * $increment;
    $self->logger()->trace(sprintf(
      'Basic Arribute %s for %s is %d after step 1',
      $attrib, $name, $step
    ));
    $step = $step + 500 + $AES_adjustment;
    $self->logger()->trace(sprintf(
      'Basic Arribute %s for %s is %d after adding 500 and AES %n',
      $attrib, $name, $step, $AES_adjustment
    ));

    if ($step < 900) {
      $step = $step * .1;
    }
    else {
      $step = 90 + ($step - 900) * 0.2;
    }
    $self->logger()->trace(sprintf(
      'Basic Arribute %s for %s is %d after if block',
      $attrib, $name, $step
    ));
    return $step;
  }

  method score(
    $level      = 1,
    $stars      = 'none',
    $name       = "GeneralName",
    $multiplier = 0,
    $attrib     = "Attribute",
  ) {
    my $total = $self->total($level, $stars, $name, $attrib);
    $total = $total * $multiplier;
    return $total;
  }

  method validation {
    my @errors = ();

    my $type = t('PositiveOrZeroNum');
    is_Num($base)
      or push @errors => "base must be a number, not $base";
    $type->check($base)
      or push @errors => "base must be positive, not $base";

    $type = t('PositiveOrZeroNum');
    is_Num($increment)
      or push @errors => "increment must be a number, not $increment";
    $type->check($increment)
      or push @errors => "increment must be positive, not $increment";

    is_Str($attribute_name)
      or push @errors => "attribute name must be a string, not $attribute_name";
    if (none { $_ eq $attribute_name } @AttributeNames) {
      push @errors,
        "attribute name must be one of " . Data::Printer::np @AttributeNames;
    }
    if (scalar @errors >= 1) {
      $self->logger()->logcroak(join(', ' => @errors));
    }

  }

  method _comparison ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttribute') {
      my $od = Data::Printer::p $other;
      $self->logger()
        ->logcroak(
        "Game::EvonyTKR::BasicAttribute comparison operator cannot take a $od");
    }
    else {
      my $mt = $self->total();
      my $ot = $other->total();
      if ($self->attribute_name() cmp $other->attribute_name()) {
        $self->logger()->warn(
          'you probably did not intend to compare to different attributes: %s %s',
          $self->attribute_name(), $other->attribute_name()
        );
        return $self->attribute_name() cmp $other->attribute_name();
      }
      return $mt <=> $ot;
    }
  }

  method _equality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttribute') {
      my $od = Data::Printer::p $other;
      $self->logger()
        ->logcroak(
        "Game::EvonyTKR::BasicAttribute equality operator cannot take a $od");
    }
    else {
      my $mt = $self->total();
      my $ot = $other->total();
      return (($mt == $ot)
        and ($self->attribute_name() eq $other->attribute_name()));
    }
  }

  method _inequality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttribute') {
      my $od = Data::Printer::p $other;
      $self->logger()
        ->logcroak(
        "Game::EvonyTKR::BasicAttribute inequality operator cannot take a $od");
    }
    else {
      my $mt = $self->total();
      my $ot = $other->total();
      return (($mt != $ot)
        or ($self->attribute_name() ne $other->attribute_name()));
    }
  }

  method _toHashRef($verbose = 0) {
    my $returnRef = {};
    $returnRef->{ total } = $self->total();
    if ($verbose) {
      $returnRef->{base}      = $self->base();
      $returnRef->{increment} = $self->increment();
    }
    return $returnRef;
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
# ABSTRACT: Stores a single Basic Attribute of a Game::EvonyTKR::General

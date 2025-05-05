use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';
require Math::Round;

class Game::EvonyTKR::Model::BasicAttribute : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::BasicAttribute
  use Carp;
  use List::AllUtils qw( any none );
  use Data::Printer;
  use Hash::Util;
  use Types::Common qw( -lexical -all t);
  use namespace::autoclean;
# VERSION

  use File::FindLib 'lib';
  use overload
    '<=>' => \&_comparison,
    '=='  => \&_equality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

  ADJUST {
    if (!(t->simple_lookup('PositiveOrZeroNum'))) {
      t->add_types(-Common);
    }
  }

  field $base : reader : param //= 0;

  field $increment : reader : param //= 0;

  field $attribute_name : reader : param;

  field $EvansAdjustment = 2.4867;

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
    my @errors         = ();
    my @AttributeNames = $self->AttributeNames();

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

  # =ROUND(((900*0.1)+(((L131+(M131*2.4867*44))*1.1+50+520)-900)*0.2)/100,3)
  #https://evonyguidewiki.com/en/general-cultivate-en/
  method total(
    $level  = 1,
    $stars  = 'none',
    $name   = "GeneralName",
    $attrib = "Attribute"
  ) {
    my $AES_adjustment = 0;
    my $cultivation    = 520;
    if (exists $BasicAESAdjustment{$stars}) {
      $AES_adjustment = $BasicAESAdjustment{$stars};
    }
    # The EvansAdjustment may be intended to partially account for the variable
    # amount of attribute increase per star a general gets for the first five
    # stars.  This varies per general and does not seem to be tracked by anyone.
    my $sa     = $BasicAESAdjustment{$stars};
    my $result = Math::Round::round((
        (900 * 0.1) + ((
            ($base + ($increment * $EvansAdjustment * $level)) * 1.1 +
              $sa + $cultivation
          ) - 900
        ) * 0.2
      ) / 100,
      3
    );
    $self->logger->debug(
      "found total basic attribute value of $result for $name");
    return $result;
  }

  method validation {
    my @errors         = ();
    my @AttributeNames = $self->AttributeNames();

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
      return (
             ($mt != $ot)
          or ($self->attribute_name() ne $other->attribute_name())
      );
    }
  }

  method _toHashRef($verbose = 0) {
    my $returnRef = {};

    $returnRef->{base}      = $self->base();
    $returnRef->{increment} = $self->increment();
    if ($verbose) {
      $returnRef->{total} = $self->total();
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

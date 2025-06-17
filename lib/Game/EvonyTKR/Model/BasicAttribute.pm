use v5.40.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require Math::Round;

class Game::EvonyTKR::Model::BasicAttribute : isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::BasicAttribute
  use Carp;
  use List::AllUtils qw( any none );
  use Types::Common  qw( t is_Num is_Str);
  use Scalar::Util   qw(blessed);
  use Data::Printer;
  use Hash::Util;
  require JSON::PP;
  use namespace::autoclean;
  use overload
    '""'       => \&TO_JSON,
    "fallback" => 0;
# VERSION

  use File::FindLib 'lib';
  use overload
    '<=>'      => \&_comparison,
    '=='       => \&_equality,
    'eq'       => \&_equality,
    '!='       => \&_inequality,
    '""'       => \&as_string,
    "fallback" => 0;

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
    my @errors = ();

    my $tp = t('PositiveOrZeroNum');
    is_Num($base)
      or push @errors => "base must be a number, not $base";
    $tp->check($base)
      or push @errors => "base must be positive, not $base";

    $tp = t('PositiveOrZeroNum');
    is_Num($increment)
      or push @errors => "increment must be a number, not $increment";
    $tp->check($increment)
      or push @errors => "increment must be positive, not $increment";

    is_Str($attribute_name)
      or push @errors => "attribute name must be a string, not $attribute_name";
    my $re = $self->AttributeNames()->as_regexp;

    if ($attribute_name !~ /$re/i) {
      push @errors,
        "attribute name is $attribute_name, not one of "
        . Data::Printer::np $self->AttributeNames()->values();
    }
    if (scalar @errors >= 1) {
      croak(join(', ' => @errors));
    }
  }

  method setBase($newBase = 0) {
    my @errors = ();
    my $tp     = t('PositiveOrZeroNum');
    is_Num($newBase)
      or push @errors => "base must be a number, not $newBase";
    $tp->check($newBase)
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
    my $tp     = t('PositiveOrZeroNum');
    is_Num($newIncrement)
      or push @errors => "increment must be a number, not $newIncrement";
    $tp->check($newIncrement)
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

    my $tp = t('PositiveOrZeroNum');
    is_Num($base)
      or push @errors => "base must be a number, not $base";
    $tp->check($base)
      or push @errors => "base must be positive, not $base";

    $tp = t('PositiveOrZeroNum');
    is_Num($increment)
      or push @errors => "increment must be a number, not $increment";
    $tp->check($increment)
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
"Game::EvonyTKR::Model::BasicAttribute comparison operator cannot take a $od"
        );
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
    if (defined($otherClass) && length($otherClass) > 0) {
      my @classList = split(/::/, $otherClass);
      if ($classList[2] ne 'BasicAttribute') {
        my $od = Data::Printer::p $other;
        $self->logger()
          ->logcroak(
"Game::EvonyTKR::Model::BasicAttribute equality operator cannot take a $od"
          );
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

  method _inequality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'BasicAttribute') {
      my $od = Data::Printer::p $other;
      $self->logger()
        ->logcroak(
"Game::EvonyTKR::Model::BasicAttribute inequality operator cannot take a $od"
        );
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

  method to_hash {
    return {
      base      => $base,
      increment => $increment,
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
# ABSTRACT: Stores a single Basic Attribute of a Game::EvonyTKR::Model::General

use v5.40.0;
use experimental 'class';

class Game::EvonyTKR::Buff::Value {

  use Types::Standard        qw(is_Int Int Str is_Str);
  use Types::Common::Numeric qw(PositiveOrZeroInt);
  use Type::Utils "is";
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Buff::Value

# ABSTRACT: Values for Evony TKR Buffs

=attr $number

the numeric value as an Integer for Buff.  Defaults to 0.
=cut

=method number()

the auto generated getter for the $number attribute
=cut

  field $number : reader : param //= 0;

  ADJUST {
    my @errors;
    is_Int($number) or push @errors => "number must be an integer, not $number.
    ";
    PositiveOrZeroInt->check($number)
      or push @errors => "number must be positive, not $number";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

=attr $unit

this string value will reject anything other than the litteral values 'percentage' and 'flat'.
It defaults to 'percentage'.
=cut

=method unit()

the autogenerated getter for the $unit attribute
=cut

  field $unit : reader : param //= 'percentage';

  ADJUST {
    my @errors;
    is_Str($unit) or push @errors => "unit must be a string, not $unit.";
    ($unit =~ /^(percentage|flat)$/)
      or push @errors => "unit must match 'percentage' or 'unit' not '$unit'.";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

}

1;

__END__

=head1 DESCRIPTION

A Buff needs a (numeric) Value to be meaningful.  This may be either a flat or percentage based value.

Flat buffs are simply added to the attribute they enhance.  To date, I am unaware of any flat debuffs in the game.

Percentage based buffs are multiplied against the base value for that attribute.  Thus if you have multiple percentage
based buffs, you are not going to experience exponential growth, since each will be multiplied against the same base value
before they are then added together.

=cut

=method new()

autogenerated constructor for this class
=cut

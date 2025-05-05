use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

class Game::EvonyTKR::Model::Buff::Value : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::Buff::Value
  use Types::Common qw( -lexical -all);
  use Mojo::JSON    qw (encode_json);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""' => \&to_String;

  field $number : reader : param //= 0;

  field $unit : reader : param //= 'flat';

  method validate() {
    my @errors;

    if ($unit ne 'flat' and $unit ne 'percentage') {
      push @errors, "unit must be 'flat' or 'percentage' not '$unit'";
    }

    my $type = t('PositiveOrZeroNum');
    $type->check($number)
      or push @errors =>
      "number must be a positive floating point number, not '$number'";

    if (scalar @errors >= 1) {
      $self->logger()->logcroak(join(', ' => @errors));
    }
  }

  ADJUST {
    $self->validate();
  }

  method to_String {
    return {
      number => $number,
      unit   => $unit,
    };
  }

}
1;

__END__

#ABSTRACT: Stores the Value for Buff objects

=pod

=head1 DESCRIPTION

A Buff's value cannot be properly understood, or used, as a simple number.  Rather,
it must be considered in light of its unit.  A Buff/Debuff Value is either a flat
value (which *could* be considered a simple number), or a percentage (which would
need to be divided by 100 and then multiplied against some base value before it
could be treated as a simple number).

This stores both the numeric portion and the string based unit designator for the Value.

=cut

=cut

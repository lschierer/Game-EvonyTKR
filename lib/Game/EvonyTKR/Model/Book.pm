use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;
use namespace::clean;

class Game::EvonyTKR::Model::Book : isa(Game::EvonyTKR::Model::Data) {
  # PODNAME: Game::EvonyTKR::Model::Book
  use Carp;
  use List::AllUtils qw( any none );
  use overload
    '""'       => \&as_string,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  field $name : reader : param;
  field $buff : reader = [];
  field $text : reader : param //= '';

  method get_buffs (
    $attribute,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    my $logger = $self->logger;
    $logger->debug("Calculating buffs for $name, attribute: $attribute");

    my $total = 0;

    foreach my $b (@$buff) {
      if ($b->match_buff(
        $attribute, $targetedType, $conditions, $debuffConditions
      )) {
        my $val = $b->value->number;
        $logger->debug("  ➤ Match found. Adding $val to total.");
        $total += $val;
      }
    }

    $logger->debug("$name: total for attribute '$attribute': $total");
    return $total;
  }

  method addBuff ($newBuff) {
    $self->logger->debug("addBuff called for book '$name'");

    if (!defined $newBuff) {
      $self->logger->warn("addBuff: newBuff is undefined");
      return;
    }

    my $reftype = Scalar::Util::reftype($newBuff);
    my $blessed = Scalar::Util::blessed($newBuff);

    $self->logger->debug(
      "addBuff: newBuff reftype=$reftype, blessed=" . ($blessed // 'undef'));

    if ($reftype eq 'OBJECT') {
      my $classList = $blessed;
      $self->logger->debug("Adding buff of class $classList to book $name");

      my @classStack = split(/::/, $classList);
      $self->logger->debug("Class stack: " . join(", ", @classStack));

      if (scalar @classStack > 3) {
        if ($classStack[3] eq 'Buff') {
          $self->logger->info("adding $newBuff to $name");

          # Check if $buff is defined and is an array reference
          if (!defined $buff) {
            $self->logger->warn("$buff is undefined in book $name");
            $buff = [];
          }
          elsif (ref($buff) ne 'ARRAY') {
            $self->logger->warn(
              "$buff is not an array reference in book $name");
            $buff = [];
          }

          push @{$buff}, $newBuff;
          $self->logger->debug(
            "Book $name now has " . scalar @{$buff} . " buffs");
        }
        else {
          $self->logger->warn("Not adding buff: class stack position 2 is "
              . $classStack[2]
              . " not 'Buff'.");
        }
      }
      else {
        $self->logger->warn(
          "Not adding buff: class stack has fewer than 3 elements");
      }
    }
    else {
      $self->logger->warn("Not adding buff: not an object (reftype=$reftype)");
    }
  }

  method getBuffs {
    if (!defined $buff) {
      $self->logger->warn("getBuffs: buff is undefined in book $name");
      return [];
    }
    return $buff;
  }

  method to_hash {
    return {
      name => $name,
      text => $text,
      buff => $buff,
    };
  }

  method TO_JSON {
    return $self->to_hash();
  }

  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  method _isTrue {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa('Game::EvonyTKR::Model::Book');
  }

  method validate() {
    my @errors;
    if (scalar @{$buff}) {
      for my $b (@{$buff}) {
        my $bc  = blessed $b;
        my @bcl = split(/::/, $bc);
        if (not($bcl[1] eq 'EvonyTKR' and $bcl[2] eq 'Buff')) {
          push @errors,
            sprintf(
            '$buff must contain type Game::EvonyTKR::Model::Buff not %s',
            $bc);
        }
      }
    }
    my $type = t('Str');
    $type->check($name)
      or push @errors => sprintf('$name must contain a string, not %s', $name);
    $type->check($text)
      or push @errors => sprintf('$text must contain a string, not %s', $text);
    if (@errors) {
      $self->logger()->logcroak(join(', ' => @errors));
    }
  }

}
1;

__END__

#ABSTRACT: base class for builtin BuiltIn and Standard Skill Books.

=pod

=head1 DESCRIPTION

Books are one of the fundamental ways in which the game adds Buffs and Debuffs to Generals.

=cut

=cut
1;

__END__

#ABSTRACT: base class for builtin BuiltIn and Standard Skill Books.

=pod

=head1 DESCRIPTION

Books are one of the fundamental ways in which the game adds Buffs and Debuffs to Generals.

=cut

=cut

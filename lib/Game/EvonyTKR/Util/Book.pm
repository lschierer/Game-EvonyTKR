use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::Buff::Matcher;
require JSON::PP;
require Sereal::Encoder;
require Sereal::Decoder;
use namespace::clean;

package Game::EvonyTKR::Util::Book {
  use Mojo::Base 'Game::EvonyTKR::Util::Common', -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::GeneralConstants', -role;
  use Carp;
  use Log::Any       qw($log);
  use List::AllUtils qw( any none );

  my $logger = $log;

  sub get_buffs ($self) (
    $attribute, $matching_type,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    $logger->debug("Calculating buffs for $name, attribute: $attribute");

    my $total = 0;

    # For buff matching, don't pass debuff conditions
    # For debuff matching, don't pass buff conditions
    my ($match_buff_conditions, $match_debuff_conditions);
    if ($matching_type eq 'buff') {
      $match_buff_conditions   = $conditions;
      $match_debuff_conditions = [];
    }
    else {
      $match_buff_conditions   = $conditions;
      $match_debuff_conditions = $debuffConditions;
    }

    foreach my $b ($self->buffs->@*) {
      my $matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $b);
      my $logID   = int(rand(9e12)) + 1e12;
      if ($matcher->match(
        $attribute,             $targetedType,
        $match_buff_conditions, $match_debuff_conditions,
        $logID
      )) {
        my $val = $b->value->number;
        $logger->debug("  ➤ Match found. Adding $val to total.");
        $total += $val;
      }
      else {
        $logger->debug("  ✗ No match found.");
      }
    }

    $logger->debug("$name: total for attribute '$attribute': $total");
    return $total;
  }

  sub addBuff ($self, $newBuff) {
    $logger->debug("addBuff called for book '$name'");

    if (!defined $newBuff) {
      $logger->warn("addBuff: newBuff is undefined");
      return;
    }

    my $reftype = Scalar::Util::reftype($newBuff);
    my $blessed = Scalar::Util::blessed($newBuff);

    $logger->debug(sprintf(
      'addBuff: newBuff reftype="%s", blessed="%s"',
      $reftype, ($blessed // 'undef')
    ));

    if ($reftype eq 'OBJECT') {
      my $classList = $blessed;
      $logger->debug("Adding buff of class $classList to book $name");

      my @classStack = split(/::/, $classList);
      $logger->debug("Class stack: " . join(", ", @classStack));

      if (scalar @classStack > 3) {
        if ($classStack[3] eq 'Buff') {
          $logger->debug("adding $newBuff to $name");

          push @{ $self->buffs }, $newBuff;
          $logger->debug(sprintf(
            'Book "%s" now has "%s" buffs',
            $name, scalar @{ $self->buffs }
          ));
        }
        else {
          $logger->warn(sprintf(
            'Not adding buff: class stack position 2 is "%s" not "Buff"',
            $classStack[2]));
        }
      }
      else {
        $logger->warn("Not adding buff: class stack has fewer than 3 elements");
      }
    }
    else {
      $logger->warn("Not adding buff: not an object (reftype=$reftype)");
    }
  }

  sub validate($self) {
    my @errors;
    if (scalar @{ $self->buffs }) {
      for my $b (@{ $self->buffs }) {
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
    unless (not Scalar::Util::looks_like_number($name)) {
      push @errors => sprintf('$name must contain a string, not %s', $name);
    }
    unless (not Scalar::Util::looks_like_number($text)) {
      push @errors => sprintf('$text must contain a string, not %s', $text);
    }
    if (@errors) {
      $logger->logcroak(join ', ', @errors);
      return;
    }
  }

}
1;

__END__

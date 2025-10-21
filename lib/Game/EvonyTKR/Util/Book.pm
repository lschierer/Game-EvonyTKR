use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
require Scalar::Util;
require Game::EvonyTKR::Model::Buff::Matcher;
require Game::EvonyTKR::Util::Common;
require Game::EvonyTKR::Shared::Constants::BuffConstants;
require Game::EvonyTKR::Shared::Constants::GeneralConstants;
use namespace::autoclean;

package Game::EvonyTKR::Util::Book {
  use Mojo::Base -role,                          -signatures;
  use Mojo::Base 'Game::EvonyTKR::Util::Common', -role;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::GeneralConstants', -role;
  use Log::Any       qw($log);
  use List::AllUtils qw( any none );
  use Carp;

  my $logger = $log;

  sub get_buffs (
    $self, $attribute, $matching_type,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    $logger->debug(
      sprintf(
        'Calculating buffs for "%s", attribute: "%s"',
        $self->name, $attribute
      )
    );

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

    $logger->debug(
      sprintf(
        '"%s": total for attribute "%s": "%s"',
        $self->name, $attribute, $total
      )
    );
    return $total;
  }

  sub addBuff ($self, $newBuff) {
    $logger->debug(sprintf('addBuff called for book "%s"', $self->name));

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
      $logger->debug(
        sprintf(
          'Adding buff of class "%s" to book "%s"',
          $classList, $self->name
        )
      );

      my @classStack = split(/::/, $classList);
      $logger->debug("Class stack: " . join(", ", @classStack));

      if (scalar @classStack > 3) {
        if ($classStack[3] eq 'Buff') {
          $logger->debug(sprintf('adding %s to %s', $newBuff, $self->name));

          push @{ $self->buffs }, $newBuff;
          $logger->debug(sprintf(
            'Book "%s" now has "%s" buffs',
            $self->name, scalar @{ $self->buffs }
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
    unless (not Scalar::Util::looks_like_number($self->name)) {
      push @errors =>
        sprintf('$name must contain a string, not %s', $self->name);
    }
    unless (not Scalar::Util::looks_like_number($self->text)) {
      push @errors =>
        sprintf('$text must contain a string, not %s', $self->text);
    }
    if (@errors) {
      $logger->logcroak(join ', ', @errors);
      return;
    }
  }

}
1;

__END__

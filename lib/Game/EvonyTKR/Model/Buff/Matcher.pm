use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;

class Game::EvonyTKR::Model::Buff::Matcher : isa(Game::EvonyTKR::Model::Data) {
  use List::AllUtils qw( any all none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  our $VERSION = 'v0.1.0';

  field $toTest : param;

  field %general_to_targeted = (
    mounted => 'Mounted Troops',
    ground  => 'Ground Troops',
    ranged  => 'Ranged Troops',
    siege   => 'Siege Machines',
  );

  method matchTargetedType($test_tt, $logID) {
    if (scalar @{ $toTest->targetedTypes }) {
      # test_tt often comes from generals, convert it for use here.
      if ($test_tt =~ /^(\w+)_specialist$/) {
        my $short = $1;

        if (exists $general_to_targeted{$short}) {
          $test_tt = $general_to_targeted{$short};
          $self->logger->debug(
            "$logID. Normalized test_targetedType to '$test_tt'");
        }
        else {
          $self->logger->warn(
            "$logID . Unrecognized general specialist key: $test_tt");
          return 0;
        }
      }

      if (none { $_ =~ /\Q$test_tt\E/i } @{ $toTest->targetedTypes }) {
        $self->logger->debug(
          $logID
            . sprintf(
            '  ✗ Rejected: targetedType "%s" not matched by %s',
            $test_tt, Data::Printer::np($toTest->targetedTypes)
            )
        );
        return 0;
      }
    }
    else {
      $self->logger->debug(
        "$logID the buff to be tested had no targeted types.");
    }
    $self->logger->debug($logID . ' matchTargetedType found no issue');
    return 1;
  }

  method matchDebuffConditions ($testDebuffs, $logID) {
    my $has_debuff_conditions = scalar @{ $toTest->debuffConditions } > 0;
    $self->logger->debug(
      "$logID has_debuff_conditions is $has_debuff_conditions");
    if ($has_debuff_conditions) {
      if (scalar @$testDebuffs == 0) {
        $self->logger->debug(
          "  ✗ Rejected: buff has debuff conditions but none were requested");
        return 0;
      }
      $self->logger->debug(
        "past check for no debuff conditions. " . scalar @$testDebuffs);
      # we have debuff condition values to test against
      foreach my $condition (@{ $toTest->debuffConditions }) {
        if (none { $_ eq $condition } @$testDebuffs) {
          $self->logger->debug(
            "  ✗ Rejected: debuff condition '$condition' not in allowed list");
          return 0;
        }
      }
    }
    elsif (scalar @{$testDebuffs}) {
      $self->logger->debug($logID
          . "  ✗ Rejected: debuff conditions are not present in buff and are required."
      );
      return 0;
    }
    $self->logger->debug($logID . ' matchDebuffConditions found no issue');
    return 1;
  }

  method matchBuffConditions ($testBuffs, $logID) {
    if (scalar @{ $toTest->buffConditions }) {
      if (scalar @{$testBuffs} == 0) {
        $self->logger->debug($logID
            . '  ✗ Rejected: This buff has conditions, and none are allowed.');
        return 0;
      }
      foreach my $condition (@{ $toTest->buffConditions }) {
        if (none { $_ eq $condition } @{$testBuffs}) {
          $self->logger->debug(
            $logID
              . sprintf(
              '  ✗ Rejected: This buff has %s, which is not one of %s.',
              $condition, join(', ', @{$testBuffs}),
              )
          );
          return 0;
        }
      }
    }
    # the buff has no conditions.
    # An unconnditional buff matches all conditions.
    $self->logger->debug('matchBuffConditions found no issue');
    return 1;
  }

  method match ($test_attribute, $test_tt, $testBuffs, $testDebuffs, $logID) {
    $self->logger->debug("$logID === BUFF MATCHER CALLED ===");
    $self->logger->debug(sprintf(
      "$logID Matcher called with: attr=%s, tt=%s, buffs=%s, debuffs=%s",
      $test_attribute,        $test_tt,
      join(',', @$testBuffs), join(',', @$testDebuffs)
    ));

    if ($toTest->attribute ne $test_attribute) {
      $self->logger->debug("$logID Rejecting based on $test_attribute");
      return 0;
    }

    if (scalar @$testBuffs == 0) {
      $testBuffs = [
        "Attacking",
        "brings a dragon",
        "brings dragon or beast to attack",
        "dragon to the attack",
        "leading the army to attack",
        "Marching",
        "When Rallying",
      ];
      $self->logger->debug(
        "$logID Empty buff conditions provided, using defaults instead");
    }
    if (length($test_tt)) {
      if (!$self->matchTargetedType($test_tt, $logID)) {
        $self->logger->debug("$logID Rejecting based on $test_tt");
        return 0;
      }
    }
    if (!$self->matchDebuffConditions($testDebuffs, $logID)) {
      $self->logger->debug(
        "$logID Rejecting based on " . join(', ', @{$testDebuffs}));
      return 0;
    }
    if (!$self->matchBuffConditions($testBuffs, $logID)) {
      $self->logger->debug(
        "$logID Rejecting based on " . join(', ', @{$testBuffs}));
      return 0;
    }
    $self->logger->debug("accepted $logID");
    return 1;
  }
}
1;

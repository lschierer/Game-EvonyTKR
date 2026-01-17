use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;

package Game::EvonyTKR::Model::Buff::Matcher {
  use Moo;
  extends 'Game::EvonyTKR::Model::Base';
  use List::AllUtils qw( any all none );
  use Carp;
  our $VERSION = 'v0.1.0';

  has toTest => (is => 'rw');

  has general_to_targeted => (
    is => 'ro',
    lazy => 1,
    default => sub {
      return {
        mounted => 'Mounted Troops',
        ground  => 'Ground Troops',
        ranged  => 'Ranged Troops',
        siege   => 'Siege Machines',
      };
    }
  );

  # Conditions that are equivalent to having no condition
  has no_op_Conditions => (
    is => 'ro',
    lazy => 1,
    default => sub {
      return [
        "brings a dragon",
        'brings a sacred dragon',
        'brings a spiritual beast',
        'During SvS',
        'leading the army',
        'you own the General',
      ];
    }
  );

  sub matchTargetedType ($self, $test_tt, $logID) {
    if (length $self->toTest->targetedType) {
      # test_tt often comes from generals, convert it for use here.
      if ($test_tt =~ /^(\w+)_specialist$/) {
        my $short = $1;

        if (exists $self->general_to_targeted->{$short}) {
          $test_tt = $self->general_to_targeted->{$short};
          $self->logger->debug(
            "$logID. Normalized test_targetedType to '$test_tt'");
        }
        else {
          $self->logger->warn(
            "$logID . Unrecognized general specialist key: $test_tt");
          return 0;
        }
      }

      if ($self->toTest->targetedType !~ /$test_tt/i) {
        $self->logger->debug(
          $logID
            . sprintf(
            '  ✗ Rejected: targetedType "%s" not matched by %s',
            $test_tt, $self->toTest->targetedType
            )
        );
        return 0;
      }
    }
    else {
      $self->logger->debug("$logID the buff to be tested had no targeted types.");
    }
    $self->logger->debug($logID . ' matchTargetedType found no issue');
    return 1;
  }

  sub matchDebuffConditions ($self, $testDebuffs, $logID) {
    my $has_debuff_conditions = scalar @{ $self->toTest->debuffConditions } > 0;
    $self->logger->debug("$logID has_debuff_conditions is $has_debuff_conditions");
    if ($has_debuff_conditions) {
      if (scalar @$testDebuffs == 0) {
        $self->logger->debug(
          "  ✗ Rejected: buff has debuff conditions but none were requested");
        return 0;
      }
      $self->logger->debug(
        "past check for no debuff conditions. " . scalar @$testDebuffs);
      # we have debuff condition values to test against
      foreach my $condition (@{ $self->toTest->debuffConditions }) {
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

  sub matchBuffConditions ($self, $testBuffs, $logID) {
    if (scalar @{ $self->toTest->buffConditions }) {
      my %allowed_conditions;
      if (scalar @$testBuffs > 0) {
        %allowed_conditions =
          map { $_ => 1 } ($testBuffs->@*, $self->no_op_Conditions->@*);
      }
      else {
        %allowed_conditions = map { $_ => 1 } $self->no_op_Conditions->@*;
      }
      $self->logger->debug(sprintf(
        '%s  Processing: This buff has %s conditions.',
        $logID, scalar @{ $self->toTest->buffConditions }
      ));
      foreach my $condition (@{ $self->toTest->buffConditions }) {
        if (!exists $allowed_conditions{$condition}) {
          $self->logger->debug(
            $logID
              . sprintf(
              '  ✗ Rejected: buff condition "%s" not in allowed list (%s).',
              $condition, join(', ', sort keys %allowed_conditions),
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

  sub match ($self, $test_attribute, $test_tt, $testBuffs, $testDebuffs, $logID)
  {
    $self->logger->debug("$logID === BUFF MATCHER CALLED ===");
    $self->logger->debug(sprintf(
      "$logID Matcher called with: attr=%s, tt=%s, buffs=%s, debuffs=%s",
      $test_attribute,        $test_tt,
      join(',', @$testBuffs), join(',', @$testDebuffs)
    ));

    if ($self->toTest->attribute ne $test_attribute) {
      $self->logger->debug("$logID Rejecting based on $test_attribute");
      return 0;
    }

    if (scalar @$testBuffs == 0 && scalar @$testDebuffs == 0) {
      $testBuffs = [
        # "Attacking",
        # "brings a dragon",
        # "brings dragon or beast to attack",
        # "dragon to the attack",
        # "leading the army to attack",
        # "Marching",
        # "When Rallying",
      ];
      $self->logger->warn(
        "$logID Empty buff conditions provided, using empty defaults instead");
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
        sprintf('%s Rjecting based on: %s.', $logID, join(', ', @{$testBuffs}))
      );
      return 0;
    }
    $self->logger->debug("accepted $logID");
    return 1;
  }
}
1;

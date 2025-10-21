use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;
use namespace::autoclean;

package Game::EvonyTKR::Util::Buff {
  use Mojo::Base 'Game::EvonyTKR::Util::Common', -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants', -role;

  use List::AllUtils qw( any none );
  use Carp;
  use Log::Any qw($log);
  use File::FindLib 'lib';

  our $VERSION = 'v0.30.0';
  my $logger = $log;

  sub validate ($self) {
    my @errors;
    my @tbc = $self->buffConditions->@*;
    my @invalid;

    # Check if we got an array reference instead of a flat array
    if (@tbc == 1 && ref($tbc[0]) eq 'ARRAY') {
      $self->logger->error("needed to flatten buffConditions"
          . Data::Printer::np($buffConditions));
      @tbc = @{ $tbc[0] };    # Flatten it
    }

    # Find elements in @tbc that are not in the valid conditions
    my %valid_conditions = map { $_ => 1 } keys %{ $self->BuffConditionValues };
    @invalid = grep { !exists $valid_conditions{$_} } @tbc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf(
'Detected illegal value "%s" in buffConditions. All values must be one of: %s',
          $iv, join(', ', keys %{ $self->BuffConditionValues }));
      }
    }

    my @tdc = @{$debuffConditions};

    # Check if we got an array reference instead of a flat array
    if (@tdc == 1 && ref($tdc[0]) eq 'ARRAY') {
      @tdc = @{ $tdc[0] };    # Flatten it
      $self->logger->error("needed to flatten debuffConditions");
    }

    # Find elements in @tdc that are not in the valid debuff conditions
    my %valid_debuff_conditions =
      map { $_ => 1 } @{ $self->DebuffConditionValues };
    @invalid = grep { !exists $valid_debuff_conditions{$_} } @tdc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf(
'Detected illegal value "%s" in debuffConditions. All values must be one of: %s',
          $iv, join(', ', @{ $self->DebuffConditionValues }));
      }
    }

    my $type = t('BoolLike');
    $type->check($passive)
      or push @errors => "passive must be 0 or 1, not $passive";

    if (@errors) {
      $logger->logcroak(join ', ', @errors);
      return;
    }

    $value->validate();

  }

  sub has_targetedType ($self) {
    if (ref($self->targetedType) eq 'ARRAY') {
      return scalar @{ $self->targetedType };
    }
    return length($self->targetedType);
  }

  sub match_buff (
    $self, $test_attribute,
    $test_targetedType     = '',
    $test_buffConditions   = [],
    $test_debuffConditions = [],
  ) {

    # Override empty array with defaults
    if (scalar @$test_buffConditions == 0) {
      $test_buffConditions = [
        "Attacking",
        "brings a dragon",
        "brings dragon or beast to attack",
        "dragon to the attack",
        "leading the army to attack",
        "Marching",
        "When Rallying",
      ];
      $logger->debug("Empty buff conditions provided, using defaults instead");
    }

    $logger->debug(sprintf(
      'Checking match for buff: attr=%s, targetType=%s, '
        . 'buff conditions=%s, debuff conditions=%s',
      $self->attribute,
      $targetedType,
      join(', ',
        ref $self->buffConditions eq 'ARRAY' ? @{ $self->buffConditions } : ()),
      join(', ',
        ref $self->debuffConditions eq 'ARRAY'
        ? @{ $self->debuffConditions }
        : ()),
    ));

    # Check attribute match
    return 0 unless $self->attribute eq $test_attribute;

    # Check targetedType match if provided
    if (length($test_targetedType) && length($self->targetedType)) {
      # targetType often comes from generals, convert it for use here.
      if (exists $self->$GeneralTypes2TroopTypes{$test_targetedType}) {
        $test_targetedType =
          $self->$GeneralTypes2TroopTypes{$test_targetedType};
        $logger->debug("Normalized test_targetedType to '$test_targetedType'");
      }
      else {
        $logger->warn(
          "Unrecognized general specialist key: $test_targetedType");
        return 0;
      }

      # Match against the buff's targetedTypes
      if ($self->targetedType ne $test_targetedType) {
        $logger->debug(sprintf(
          '  ✗ Rejected: test_targetedType "%s" not matched by "%s"',
          $test_targetedType, $self->targetedType
        ));
        return 0;
      }
    }
    elsif (ref($self->targetedType) eq 'ARRAY') {
      my $match = 0;
      foreach my $tt ($self->targetedType->@*) {
        if ($tt eq $test_attribute) {
          $match = 1;
          last;
        }
      }
      if ($match == 0) {
        $logger->debug(sprintf(
          '  ✗ Rejected: test_targetedType "%s" not matched by "%s"',
          $test_targetedType, join ', ', $self->targetedType->@*
        ));
        return 0;
      }
    }

    # Check debuff conditions
    my $has_debuff_conditions = scalar @{ $self->debuffConditions } > 0;

    # If test_debuffConditions is empty, reject any buff with debuff conditions
    if (scalar @$test_debuffConditions == 0) {
      if ($has_debuff_conditions) {
        $logger->debug(
          "  ✗ Rejected: buff has debuff conditions but none were requested");
        return 0;
      }
    }
    # If test_debuffConditions is provided,
    # check that all debuff conditions are in the allowed list
    elsif ($has_debuff_conditions) {
      foreach my $condition ($self->debuffConditions->@*) {
        if (none { $_ eq $condition } @$test_debuffConditions) {
          $logger->debug(
            "  ✗ Rejected: debuff condition '$condition' not in allowed list");
          return 0;
        }
      }
    }
    else {
      $logger->debug("  ✗ Rejected: debuff conditions are not "
          . "present in buff and are required.");
      return 0;
    }

    # Check buff conditions
    my $has_buff_conditions = scalar @{ $self->buffConditions } > 0;

    # If test_buffConditions is provided,
    # check that all buff conditions are in the allowed list
    if (scalar @$test_buffConditions > 0) {
      # A buff with no conditions should match when conditions are specified
      if (!$has_buff_conditions) {
        $logger->debug(
          "  ✓ Buff has no conditions, accepting unconditional buff");
        # Continue to the end of the function
      }
      else {
        $logger->debug("Checking buff conditions: "
            . join(', ', $self->buffConditions->@*)
            . " against allowed: "
            . join(', ', @$test_buffConditions));

        # Check if ANY of the buff's conditions are NOT in the allowed list
        my %allowed_conditions = map { $_ => 1 } @$test_buffConditions;
        foreach my $condition (@$buffConditions) {
          if (!exists $allowed_conditions{$condition}) {
            $logger->debug(
              "  ✗ Rejected: buff condition '$condition' not in allowed list");
            return 0;
          }
        }

      }
    }
    else {
      $logger->error("NO TEST BUFF CONDITIONS!!");
    }

    $logger->debug("  ✓ Buff matched");
    return 1;
  }

  sub from_hash ($class, $hashref) {
    my $logger = $log;
    my $v      = Game::EvonyTKR::Model::Buff::Value->new(
      number => abs($hashref->{value}->{number}),
      unit   => ($hashref->{value}->{unit} // 'percentage'),
    );
    my $r = Game::EvonyTKR::Model::Buff->new(
      attribute => $hashref->{attribute},
      passive   => ($hashref->{passive} // 0),
      value     => $v,
    );
    if (exists $hashref->{targetedType}) {
      $logger->debug(
        'found targetedType in hashref: ' . $hashref->{targetedType});
      $r->set_target($hashref->{targetedType});
    }
    if (exists $hashref->{troop}) {
      $logger->debug('found troop in hashref: ' . $hashref->{troop});
      $r->set_target($hashref->{troop});
    }
    if (exists $hashref->{condition}) {
      foreach my $c (@{ $hashref->{conditions} }) {
        $r->set_condition($c);
      }
    }
    elsif (exists $hashref->{conditions}) {
      foreach my $c (@{ $hashref->{conditions} }) {
        $r->set_condition($c);
      }
    }
    return $r;
  }

}
1;

__END__

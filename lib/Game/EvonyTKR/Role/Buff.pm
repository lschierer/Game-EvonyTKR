use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
use namespace::autoclean;

package Game::EvonyTKR::Role::Buff {
  use Mojo::Base -role,                                            -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',                   -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;

  use List::AllUtils qw( any none );
  use Carp;
  use File::FindLib 'lib';

  our $VERSION = 'v0.30.0';

  sub validate ($self) {
    my @errors;
    my @tbc = $self->buffConditions->@*;
    my @invalid;

    # Check if we got an array reference instead of a flat array
    if (@tbc == 1 && ref($tbc[0]) eq 'ARRAY') {
      $self->logger->error("needed to flatten buffConditions"
          . Data::Printer::np($self->buffConditions));
      @tbc = @{ $tbc[0] };    # Flatten it
    }

    # Find elements in @tbc that are not in the valid conditions
    my %valid_conditions = map { $_ => 1 } keys %{ $self->BuffConditionValues };
    @invalid = grep { !exists $valid_conditions{$_} } @tbc;

    # Report errors for invalid conditions
    if (@invalid) {
      foreach my $iv (@invalid) {
        push @errors,
          sprintf('Detected illegal value "%s" in buffConditions. '
            . 'All values must be one of: %s',
          $iv, join(', ', keys %{ $self->BuffConditionValues }));
      }
    }

    my @tdc = @{ $self->debuffConditions };

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
          sprintf('Detected illegal value "%s" in debuffConditions. '
            . 'All values must be one of: %s',
          $iv, join(', ', @{ $self->DebuffConditionValues }));
      }
    }

    unless ($self->passive == 0 || $self->passive == 1) {
      push @errors, "passive must be 0 or 1, not $self->passive";
    }

    if (@errors) {
      $self->logger->logcroak(join ', ', @errors);
      return;
    }

    $self->value->validate();

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
    print STDERR "DEBUG: match_buff called for attribute: $test_attribute\n";
    my $no_op_Conditions = [
      "brings a dragon",
      'brings a sacred dragon',
      'brings a spiritual beast',
      'During SvS',
      'leading the army',
      'you own the General',
    ];
    # Override empty array with defaults
    if (scalar @$test_buffConditions == 0) {
      #$test_buffConditions = [
      #  "Attacking",
      #  "brings a dragon",
      #  "brings dragon or beast to attack",
      #  "dragon to the attack",
      #  "leading the army to attack",
      #  "Marching",
      #  "When Rallying",
      #];
      $self->logger->warn("Empty buff conditions provided!!");
    }

    $self->logger->debug(sprintf(
      'Checking match for buff: attr=%s, targetType=%s, '
        . 'buff conditions=%s, debuff conditions=%s',
      $self->attribute,
      $self->targetedType,
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
      if (exists $self->GeneralTypes2TroopTypes->{$test_targetedType}) {
        $test_targetedType =
          $self->GeneralTypes2TroopTypes->{$test_targetedType};
        $self->logger->debug(
          "Normalized test_targetedType to '$test_targetedType'");
      }
      else {
        $self->logger->warn(
          "Unrecognized general specialist key: $test_targetedType");
        return 0;
      }

      # Match against the buff's targetedTypes
      if ($self->targetedType ne $test_targetedType) {
        $self->logger->debug(sprintf(
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
        $self->logger->debug(sprintf(
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
        $self->logger->debug(
          "  ✗ Rejected: buff has debuff conditions but none were requested");
        return 0;
      }
    }
    # If test_debuffConditions is provided,
    # check that all debuff conditions are in the allowed list
    elsif ($has_debuff_conditions) {
      foreach my $condition ($self->debuffConditions->@*) {
        if (none { $_ eq $condition } @$test_debuffConditions) {
          $self->logger->debug(
            "  ✗ Rejected: debuff condition '$condition' not in allowed list");
          return 0;
        }
      }
    }
    else {
      $self->logger->debug("  ✗ Rejected: debuff conditions are not "
          . "present in buff and are required.");
      return 0;
    }

    # Check buff conditions
    my $has_buff_conditions = scalar @{ $self->buffConditions } > 0;

    # If test_buffConditions is provided,
    # check that all buff conditions are in the allowed list
    my %allowed_conditions;
    if (scalar @$test_buffConditions > 0) {
      %allowed_conditions =
        map { $_ => 1 } ($test_buffConditions->@*, $no_op_Conditions->@*);
    }
    else {
      %allowed_conditions = map { $_ => 1 } $no_op_Conditions->@*;
    }

    # A buff with no conditions should match when conditions are specified
    if (!$has_buff_conditions) {
      $self->logger->debug(
        "  ✓ Buff has no conditions, accepting unconditional buff");
      # Continue to the end of the function
    }
    else {
      foreach my $condition ($self->buffConditions->@*) {
        if (!exists $allowed_conditions{$condition}) {
          $self->logger->debug(
            "  ✗ Rejected: buff condition '$condition' not in allowed list");
          return 0;    #####<---- Line 220
        }
      }
    }

    $self->logger->debug("  ✓ Buff matched");
    return 1;
  }

  sub from_hash ($class, $hashref) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
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

use v5.42.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Readonly;
use namespace::autoclean;

class Game::EvonyTKR::Model::Data : isa(Game::EvonyTKR::Shared::Constants) {
# PODNAME: Game::EvonyTKR::Model::Data
  require Type::Tiny::Enum;
  use UUID qw(uuid5);
  use namespace::autoclean;
  use File::FindLib 'lib';
  use List::AllUtils qw( all any none );
  use X500::DN;
  use X500::RDN;
  our $VERSION = 'v0.30.0';
  my $debug = 0;
  use Carp;

  method validateBuffActivation ($proposed) {

    my %valid;
    foreach my $key ($self->AllowedBuffActivationValues->@*) {
      $valid{$key} = 1;
    }

    if (none { $_ eq $proposed } $self->AllowedBuffActivationValues->@*) {
      $self->logger->warn(
"validateBuffActivation detected illegal Buff Activation Condition $proposed"
      );
      return 0;
    }

    return 1;
  }

  field $allowedValueUnits : reader =
    Type::Tiny::Enum->new(values => [qw( flat percentage )]);

  method validateSpecialtyLevels (@specialties) {
    my $logger = $self->logger;

    # Ensure we have exactly 4 specialties
    if (scalar @specialties != 4) {
      $logger->warn("Expected 4 specialties, got " . scalar @specialties);
      return 0;
    }

    # Validate that each specialty level is valid
    foreach my $index (0 .. 3) {
      my $level = $specialties[$index];
      if (none { $_ =~ /$level/i } @{ $self->SpecialtyLevelValues }) {
        $logger->warn("Invalid specialty level at index $index: $level");
        return 0;
      }
    }

    # Check the specialty 4 rule
    my $all_gold = all { $_ eq 'gold' } @specialties[0 .. 2];

    if ($all_gold && $specialties[3] eq 'none') {
      $logger->warn(
        "When specialties 1-3 are all gold, specialty 4 cannot be 'none'");
      return 0;
    }

    if (!$all_gold && $specialties[3] ne 'none') {
      $logger->warn(
        "When specialties 1-3 are not all gold, specialty 4 must be 'none'");
      return 0;
    }

    return 1;
  }

  method normalizeSpecialtyLevels (@specialties) {
    my $logger     = $self->logger;
    my @normalized = @specialties;

    # Ensure we have exactly 4 specialties
    if (scalar @normalized != 4) {
      $logger->warn("Expected 4 specialties, got "
          . scalar @normalized
          . ". Padding with defaults.");
      while (scalar @normalized < 4) {
        push @normalized, 'gold';
      }
      @normalized = @normalized[0 .. 3] if scalar @normalized > 4;
    }

    # Validate and normalize each specialty level
    foreach my $index (0 .. 3) {
      if (none { $_ eq $normalized[$index] } $self->SpecialtyLevelValues->@*) {
        $logger->warn(
"Invalid specialty level at index $index: $normalized[$index], using default"
        );
        $normalized[$index] = 'gold';
      }
    }

    # Apply the specialty 4 rule
    my $all_gold = all { $_ eq 'gold' } @normalized[0 .. 2];

    if ($all_gold && $normalized[3] eq 'none') {
      $logger->warn(
"When specialties 1-3 are all gold, specialty 4 cannot be 'none'. Setting to gold."
      );
      $normalized[3] = 'gold';
    }

    if (!$all_gold && $normalized[3] ne 'none') {
      $logger->warn(
"When specialties 1-3 are not all gold, specialty 4 must be 'none'. Setting to none."
      );
      $normalized[3] = 'none';
    }

    return @normalized;
  }

  method checkCovenantLevel ($proposedLevel) {
    unless (defined($proposedLevel) && length($proposedLevel)) {
      $self->logger->error("Invalid proposed level!!! $proposedLevel");
      return 0;
    }
    my $check = {};
    foreach my $key ($self->CovenantCategoryValues->@*) {
      $check->{$key} = 1;
    }
    return exists $check->{$proposedLevel};
  }

  method checkAscendingLevel ($proposedLevel) {
    if ($proposedLevel =~ /none/i) {
      return 1;
    }
    if ($proposedLevel =~ /red/) {
      return
        any { $proposedLevel eq $_ } $self->AscendingAttributeLevelValues(1);
    }
    else {
      return
        any { $proposedLevel eq $_ } $self->AscendingAttributeLevelValues(0);
    }
  }

  Readonly::Scalar my $rallySpotCapacity => {
    1  => 800,
    2  => 1200,
    3  => 2000,
    4  => 3200,
    5  => 4600,
    6  => 6400,
    7  => 8400,
    8  => 10800,
    9  => 13600,
    10 => 16600,
    11 => 20000,
    12 => 23600,
    13 => 27600,
    14 => 32000,
    15 => 36600,
    16 => 41600,
    17 => 46800,
    18 => 52400,
    19 => 58400,
    20 => 64600,
    21 => 71200,
    22 => 78000,
    23 => 85200,
    24 => 92800,
    25 => 100000,
    26 => 110000,
    27 => 125000,
    28 => 145000,
    29 => 170000,
    30 => 200000,
    31 => 225000,
    32 => 250000,
    33 => 285000,
    34 => 315000,
    35 => 350000,
    36 => 385000,
    37 => 420000,
    38 => 460000,
    39 => 500000,
    40 => 550000,
    41 => 600000,
    42 => 660000,
    43 => 720000,
    44 => 790000,
    45 => 860000,
  };

  method toHashRef {
    return {};
  }

  # Method for JSON serialization
  method TO_JSON {
    return $self->to_hash();
  }

  # Stringification method using JSON
  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }
}
1;

__END__

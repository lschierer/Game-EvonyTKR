use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Readonly;
use namespace::autoclean;

class Game::EvonyTKR::Model::Data : isa(Game::EvonyTKR::Model::Logger) {
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

  field $AttributeNames : reader = Type::Tiny::Enum->new(
    values => [qw(
      attack
      defense
      leadership
      politics
    )]
  );

  field $allowedBuffActivation : reader = Type::Tiny::Enum->new(
    values => [
      "Overall", "PvM",     "Attacking", "Reinforcing",
      "Defense", "In City", "Out City",  "Wall",
      "Mayor",   "Officer",
    ]
  );

  field $buffAttributeValues : reader = Type::Tiny::Enum->new(
    values => [
      "Attack",
      "Death to Soul",
      "Death to Survival",
      "Death to Wounded",
      "Defense",
      "Deserter Capacity",
      "Double Items Drop Rate",
      "HP",
      "Hospital Capacity",
      "March Size Capacity",
      "Marching Speed to Monsters",
      "Marching Speed",
      "Rally Capacity",
      "Resources Production",
      "Stamina cost",
      "SubCity Construction Speed",
      "SubCity Gold Production",
      "SubCity Training Speed",
      "SubCity Troop Capacity",
      "Training Capacity",
      "Training Speed",
      "Wounded to Death",
    ]
  );

  field $buffConditionValues : reader = Type::Tiny::Enum->new(
    values => [
      "Against Monsters",
      "Attacking",
      "brings a dragon",
      "brings dragon or beast to attack",
      "Defending",
      "dragon to the attack",
      "leading the army to attack",
      "Marching",
      "Reinforcing",
      "When City Mayor for this SubCity",
      "When Defending Outside The Main City",
      "When Rallying",
      "In Main City",
      'In City',
      "When the Main Defense General",
    ]

  );

  field $debuffConditionValues : reader = Type::Tiny::Enum->new(
    values => [
      "Enemy",
      "Enemy In City",
      "Reduces",
      "Reduces Enemy",
      "Reduces Enemy in Attack",
      "Reduces Enemy with a Dragon",
      "Reduces Monster",
    ]
  );

  field $bookConditionValues : reader =
    Type::Tiny::Enum->new(values => ["all the time", "when not mine"]);

  method AllConditions() {
    my %seen;
    my $allowedConditions = grep { not $seen{$_}++ } (
      @{ $buffConditionValues->values },
      @{ $debuffConditionValues->values },
      @{ $bookConditionValues->values }
    );
    return $allowedConditions;
  }

  field $targetedTypeValues : reader = Type::Tiny::Enum->new(
    values => [
      'Ground Troops',
      'Mounted Troops',
      'Ranged Troops',
      'Siege Machines',
      'All Troops',
      'Monsters',
    ]
  );

  field $GeneralKeys : reader = Type::Tiny::Enum->new(
    values => [qw(
      ground_specialist
      mounted_specialist
      ranged_specialist
      siege_specialist
      mayor
      officer
      wall
    )]
  );

  field $allowedValueUnits : reader =
    Type::Tiny::Enum->new(values => [qw( flat percentage )]);

  field $specialityLevels : reader;

  ADJUST {
    my @step1 = qw( none green blue purple orange gold );
    Readonly::Array my @slv => @step1;
    $specialityLevels = \@slv;
  }

  method validateSpecialityLevels (@specialities) {
    my $logger = $self->logger;

    # Ensure we have exactly 4 specialities
    if (scalar @specialities != 4) {
      $logger->warn("Expected 4 specialities, got " . scalar @specialities);
      return 0;
    }

    # Validate that each speciality level is valid
    foreach my $index (0 .. 3) {
      my $level = $specialities[$index];
      if (!$self->specialityLevels->check($level)) {
        $logger->warn("Invalid speciality level at index $index: $level");
        return 0;
      }
    }

    # Check the speciality 4 rule
    my $all_gold = all { $_ eq 'gold' } @specialities[0 .. 2];

    if ($all_gold && $specialities[3] eq 'none') {
      $logger->warn(
        "When specialities 1-3 are all gold, speciality 4 cannot be 'none'");
      return 0;
    }

    if (!$all_gold && $specialities[3] ne 'none') {
      $logger->warn(
        "When specialities 1-3 are not all gold, speciality 4 must be 'none'");
      return 0;
    }

    return 1;
  }

  method normalizeSpecialityLevels (@specialities) {
    my $logger     = $self->logger;
    my @normalized = @specialities;

    # Ensure we have exactly 4 specialities
    if (scalar @normalized != 4) {
      $logger->warn("Expected 4 specialities, got "
          . scalar @normalized
          . ". Padding with defaults.");
      while (scalar @normalized < 4) {
        push @normalized, 'gold';
      }
      @normalized = @normalized[0 .. 3] if scalar @normalized > 4;
    }

    # Validate and normalize each speciality level
    foreach my $index (0 .. 3) {
      if (none { $_ eq $normalized[$index] } $specialityLevels->@*) {
        $logger->warn(
"Invalid speciality level at index $index: $normalized[$index], using default"
        );
        $normalized[$index] = 'gold';
      }
    }

    # Apply the speciality 4 rule
    my $all_gold = all { $_ eq 'gold' } @normalized[0 .. 2];

    if ($all_gold && $normalized[3] eq 'none') {
      $logger->warn(
"When specialities 1-3 are all gold, speciality 4 cannot be 'none'. Setting to gold."
      );
      $normalized[3] = 'gold';
    }

    if (!$all_gold && $normalized[3] ne 'none') {
      $logger->warn(
"When specialities 1-3 are not all gold, speciality 4 must be 'none'. Setting to none."
      );
      $normalized[3] = 'none';
    }

    return @normalized;
  }

  field $covenantLevels : reader;

  ADJUST {
    my @step1 = qw( none war cooperation peace faith honor civilization );
    Readonly::Array my @clv => @step1;
    $covenantLevels = \@clv;
  }

  method checkCovenantLevel ($proposedLevel) {
    my $check = {};
    foreach my $key ($covenantLevels->@*) {
      $check->{$key} = 1;
    }
    return exists $check->{$proposedLevel};
  }

  Readonly::Scalar my $redAscendingLevelNames => {
    red1 => '1 Red Star',
    red2 => '2 Red Stars',
    red3 => '3 Red Stars',
    red4 => '4 Red Stars',
    red5 => '5 Red Stars',
  };

  Readonly::Scalar my $purpleAscendingLevelNames => {
    purple1 => '1 Purple Star',
    purple2 => '2 Purple Stars',
    purple3 => '3 Purple Stars',
    purple4 => '4 Purple Stars',
    purple5 => '5 Purple Stars',
  };

  method checkAscendingLevel ($proposedLevel) {
    if ($proposedLevel =~ /none/i) {
      return 1;
    }
    if ($proposedLevel =~ /red/) {
      return exists $redAscendingLevelNames->{$proposedLevel};
    }
    else {
      return exists $purpleAscendingLevelNames->{$proposedLevel};
    }
  }

  method AscendingLevelLabel ($levelName) {
    if ($levelName =~ /red/) {
      return $redAscendingLevelNames->{$levelName};
    }
    else {
      return $purpleAscendingLevelNames->{$levelName};
    }
  }

  method AscendingLevelNames ($red = 1, $printable = 0) {
    my $towalk;
    if ($red) {
      $towalk = $redAscendingLevelNames;
    }
    else {
      $towalk = $purpleAscendingLevelNames;
    }
    my @results = ('None');
    if ($printable) {
      foreach my $key (sort keys %{$towalk}) {
        push @results, $towalk->{$key};
      }
    }
    else {
      # push the keys to preserve the initial value
      # and the flat array.
      foreach my $key (sort keys %{$towalk}) {
        push @results, $key;
      }
    }
    return \@results;
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

  field $globalDN : reader = X500::DN->new(
    X500::RDN->new('OU' => 'EvonyTKR'),
    X500::RDN->new('OU' => 'Game'),
    X500::RDN->new('OU' => 'module'),
    X500::RDN->new('dc' => 'Perl'),
    X500::RDN->new('dc' => 'org'),
  );

  field $UUID5_base : reader;

  field $UUID5_Generals : reader = {};

  ADJUST {
    my $ns_base = uuid5(dns => 'perl.org');
    $UUID5_base = uuid5($ns_base, $globalDN->getX500String());
    my $UUID5_Generals_base = uuid5($UUID5_base, 'Generals');
    for my $k (@{ $self->GeneralKeys()->values() }) {
      $UUID5_Generals->{$k} = uuid5($UUID5_Generals_base, $k);
      $self->logger()->trace("base for $k is " . $UUID5_Generals->{$k});

    }
  }

  method _isTrue {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && blessed($self) eq __CLASS__;
  }

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

# ABSTRACT: Runtime Data values for Game::EvonyTKR

=pod

=head1 DESCRIPTION

Due to the encapsulation and initialization order requirements, even if the perlclass feature had already implemented the :common attribute, things marked as common would not be initialized in time for other parameters to validate against them.  Thus I need a ::Data class that users can initialize first.

=cut

=head1 METHODS

=method new()

instantiate the shared data helper functions.

=method buffConditions

returns those conditions that are only relevant to Buffs.
=cut

=method debuffConditions

returns those conditions that are only relevant to Debuffs.
=cut

=method BuffAttributes

Returns an array of possible attributes such that each buff will have exactly one attribute from this list of possible Attributes.  This list is effectively attempting to replace having an enum.
=cut

=method AllConditions

A buff will have one or more conditions from this list of Conditioons.  This list is effectively attempting to replace having an emum.

This includes both Buff and Debuff conditions. see buffConditions() and debuffConditions().
=cut

=method BuffClasses

A *buff* will affect either exactly one or All classes of troops. This is attempting to replace having an enum.

A *General* can have more than one Class from this list.  That is because General Classes and Buff classes are the same values, but used totally differently.

=cut

use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Readonly;
require JSON::PP;
use namespace::autoclean;

class Game::EvonyTKR::Data::Constants {
  use File::Share ':all';
  use Carp;
  use List::AllUtils qw( first all any none );
  require X500::DN;
  require X500::RDN;
  use UUID qw(uuid5);
  our $VERSION = 'v0.01.0';

  field $distDir = Path::Tiny::path(dist_dir('Game::EvonyTKR::Data'));

  field $TroopTypeValues : reader;

  ADJUST {
    my $step1 = {
      ground  => "Ground Troops",
      mounted => "Mounted Troops",
      ranged  => "Ranged Troops",
      siege   => "Siege Machines",
    };
    Readonly::Hash my %temp => %{$step1};
    $TroopTypeValues = \%temp;
  }

  method string_to_trooptype ($string) {
    $string = s/(\w+)(?: .*)/\L$1/;
    my $key = first { $_ =~ /^$string/ } keys %{$TroopTypeValues};
    if (exists $TroopTypeValues->{$key}) {
      return $TroopTypeValues->{$key};
    }
    else {
      # return '' to indicate $string is not
      # reflective of a troop type
      return '';
    }
  }

  field $AttributeValues : reader;

  ADJUST {
    my @step1 = (
      "Attack",
      "Death to Soul",
      "Death to Survival",
      "Death to Wounded",
      "Defense",
      "Deserter Capacity",
      "Double Items Drop Rate",
      "Healing Speed",
      "Hospital Capacity",
      "HP",
      "March Size Capacity",
      "Marching Speed",
      "Rally Capacity",
      "Resources Production",
      "Stamina cost",
      "SubCity Construction Speed",
      "SubCity Death to Survival",
      "SubCity Gold Production",
      "SubCity Training Capacity",
      "SubCity Training Speed",
      "SubCity Troop Capacity",
      "SubCity Death to Survival",
      "Training Capacity",
      "Training Speed",
      "Wounded to Death",
    );
    Readonly::Array my @temp => @step1;
    $AttributeValues = \@temp;
  }

  method string_to_attribute ($string) {
    my $exact = first { $_ eq $string } $AttributeValues->@*;
    my $map   = {
      'Troop Death into Wounded Rate' => 'Death to Wounded',
      'Death into Survival Rate'      => 'Death to Survival',
      'March Size Increase'           => 'March Size Capacity',
      'Wounded into Death rate'       => 'Wounded to Death',
      'Death into Survival Rate in this Subordinate City' =>
        'SubCity Death to Survival',
      'Training Speed in this Subordinate City' => 'SubCity Training Speed',
    };
    if ($exact) {
      return $exact;
    }
    elsif (exists $map->{$string}) {
      my $a = $map->{$string};
      if (none { $a eq $_ } $AttributeValues->@*) {
        croak("$a is not a valid attribute mapping for $string");
      }
      return $a;
    }
    else {
      # return an empty string to indicate
      # that $string does not reflect an Attribute
      return '';
    }
  }

  field $BuffConditionValues : reader;

  ADJUST {
    my @step1 = (
      "Against Monsters",
      "Attacking",
      "brings a dragon",
      "brings a spiritual beast",
      "Defending",
      "Enemy",
      "In City",
      "In Main City",
      "leading the army",
      "Marching",
      "Reinforcing",
      "When City Mayor for this SubCity",
      "When Defending Outside The Main City",
      "When Rallying",
      # Officer positions "When Appointed as Hospital Officer",
      "When Appointed as Prison Officer",
      "When Appointed as Workshop Officer",
      "When Appointed as Academy Officer",
      "When Appointed as Embassy Officer",
    );
    Readonly::Array my @temp => @step1;
    $BuffConditionValues = \@temp;
  }

  field $DebuffConditionValues : reader;

  ADJUST {
    my @step1 = ("Enemy", "Monster",);
    Readonly::Array my @temp => @step1;
    $DebuffConditionValues = \@temp;
  }

  method string_to_condition ($string) {
    my $exactBuff   = first { $_ eq $string } $BuffConditionValues->@*;
    my $exactDebuff = first { $_ eq $string } $DebuffConditionValues->@*;
    my $map         = {
      'In-Rally'         => "When Rallying",
      'Against Monsters' => 'Monster',
      'Reduces Monster'  => "Monster",
      'Monsters'         => 'Monster',
    };

    if ($exactBuff) {
      return $exactBuff;
    }
    elsif ($exactDebuff) {
      return $exactDebuff;
    }
    elsif (exists $map->{$string}) {
      my $v = $map->{$string};
      if (none { $c eq $_ }
        ($BuffConditionValues->@*, $DebuffConditionValues->@*)) {
        croak("$v is not a valid condition mapping for $string");
      }
      return $v;
    }
    else {
      # return an empty string to indicate
      # that $string does not reflect a condition
      return '';
    }
  }

  method AllConditions() {
    my %seen;
    my @allowedConditions = grep { not $seen{$_}++ } (
      @{$buffConditionValues}, @{$debuffConditionValues},
      @{$bookConditionValues}
    );
    return @allowedConditions;
  }

  field $SpecialtyLevelValues : reader;
  ADJUST {
    my @step1 = ('none', 'green', 'blue', 'purple', 'orange', 'gold',);
    Readonly::Array my @temp => @step1;
    $ConditionValues = \@temp;
  }

  field $CovenantLevelValues : reader;

  ADJUST {
    my @step1 = qw( none war cooperation peace faith honor civilization );
    Readonly::Array my @temp => @step1;
    $CovenantLevelValues = \@temp;
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

  method AscendingAttributeLevelValues ($isRed = 1) {
    if ($isRed) {
      return keys %{$redAscendingLevelNames};
    }
    else {
      return keys %{$purpleAscendingLevelNames};
    }
  }

  method AscendingAttributeLevelNames ($isRed = 1) {
    if ($isRed) {
      return values %{$redAscendingLevelNames};
    }
    else {
      return values %{$purpleAscendingLevelNames};
    }
  }

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

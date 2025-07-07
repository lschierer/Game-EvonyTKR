use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
require X500::DN;
require X500::RDN;
require Log::Log4perl::Config;

class Game::EvonyTKR::Shared::Constants : isa(Game::EvonyTKR::Model::Logger) {
  # PODNAME: Game::EvonyTKR::Shared::Constants
  use File::Share ':all';
  use Carp;
  use List::AllUtils qw( first all any none );
  use UUID           qw(uuid5);
  use Readonly;
  use namespace::autoclean;
  use File::FindLib 'lib';
  our $VERSION = 'v0.01.0';

  field $distDir : reader;

  ADJUST {
    $distDir = dist_dir('Game::EvonyTKR');
    $distDir = Path::Tiny::path($distDir);

  }

  method execute {
    say "execute called for " . __PACKAGE__;
  }

  field $TroopTypeValues : reader;

  ADJUST {
    Readonly::Scalar my $temp => {
      ground  => "Ground Troops",
      mounted => "Mounted Troops",
      ranged  => "Ranged Troops",
      siege   => "Siege Machines",
    };
    $TroopTypeValues = $temp;
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
    Readonly::Array my @temp => (
      "Attack",
      'Construction Speed',
      "Death to Soul",
      "Death to Survival",
      "Death to Wounded",
      "Defense",
      "Deserter Capacity",
      "Double Items Drop Rate",
      "Healing Speed",
      "Hospital Capacity",
      "HP",
      "March Size",
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
      "Training Capacity",
      "Training Speed",
      "Wounded to Death",
    );
    $AttributeValues = \@temp;
  }

  method string_to_attribute ($string) {
    my $exact = first { lc($_) eq lc($string) } $AttributeValues->@*;
    my $map   = {
      'troop death into wounded rate' => 'Death to Wounded',
      'death into survival rate'      => 'Death to Survival',
      'march size increase'           => 'March Size',
      'march size'                    => 'March Size',
      'wounded into death rate'       => 'Wounded to Death',
    };
    if ($exact) {
      return $exact;
    }
    elsif (exists $map->{ lc($string) }) {
      my $a = $map->{ lc($string) };
      if (none { lc($a) eq lc($_) } $AttributeValues->@*) {
        $self->logger->logcroak(
          "$a is not a valid attribute mapping for $string");
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
    Readonly::Array my @temp => (
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
    $BuffConditionValues = \@temp;
  }

  field $DebuffConditionValues : reader;

  ADJUST {
    Readonly::Array my @temp => ("Enemy", "Monsters",);
    $DebuffConditionValues = \@temp;
  }

  # this should be the maximum number of words in *either*
  # a direct phrase in a condition *or* a mapping to a condition
  field $words_condition_phrase : reader = 6;

  method string_to_condition ($string) {
    my $exactBuff   = first { $_ eq $string } $BuffConditionValues->@*;
    my $exactDebuff = first { $_ eq $string } $DebuffConditionValues->@*;
    my $map         = {
      'In-Rally'             => "When Rallying",
      'Reduces Monster'      => "Monsters",            # debuff version
      'brings any dragon'    => 'brings a dragon',
      'to attack Monsters'   => 'Against Monsters',    # buff version
      'to attack'            => 'Attacking',
      'General is the Mayor' => "When City Mayor for this SubCity"
    };

    if ($exactBuff) {
      return $exactBuff;
    }
    elsif ($exactDebuff) {
      return $exactDebuff;
    }
    elsif (exists $map->{$string}) {
      my $v = $map->{$string};
      unless (any { $_ eq $v }
        ($BuffConditionValues->@*, $DebuffConditionValues->@*)) {
        $self->logger->logcroak(
          "$v is not a valid condition mapping for $string");
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
      @{$BuffConditionValues}, @{$DebuffConditionValues},
      #@{$bookConditionValues}
    );
    return @allowedConditions;
  }

  field $SpecialtyLevelValues : reader;

  ADJUST {
    Readonly::Array my @temp =>
      ('none', 'green', 'blue', 'purple', 'orange', 'gold',);
    $SpecialtyLevelValues = \@temp;
  }

  field $CovenantLevelValues : reader;

  ADJUST {
    Readonly::Array my @temp => (
      'none',  'war',   'cooperation', 'peace',
      'faith', 'honor', 'civilization',
    );
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
    my @result;
    push @result, 'none';
    if ($isRed) {
      push @result, keys %{$redAscendingLevelNames};
    }
    else {
      push @result, keys %{$purpleAscendingLevelNames};
    }
    return @result;
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

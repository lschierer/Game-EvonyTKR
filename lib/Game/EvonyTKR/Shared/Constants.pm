use v5.42.0;
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

  field $GeneralKeys : reader;

  ADJUST {
    Readonly::Array my @temp => qw(
      ground_specialist
      mounted_specialist
      ranged_specialist
      siege_specialist
      mayor
      officer
      wall
    );
    $GeneralKeys = \@temp;
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
    $string =~ s/(\w+)(?: .*)/\L$1/x;
    my $key = first { $_ =~ /^$string/x } keys %{$TroopTypeValues};
    if (exists $TroopTypeValues->{$key}) {
      return $TroopTypeValues->{$key};
    }
    else {
      # return '' to indicate $string is not
      # reflective of a troop type
      return '';
    }
  }

  field $AllowedBuffActivationValues : reader;

  ADJUST {
    Readonly::Array my @temp => (
      "Overall", "PvM",     "Attacking", "Reinforcing",
      "Defense", "In City", "Out City",  "Wall",
      "Mayor",   "Officer",
    );
    $AllowedBuffActivationValues = \@temp;
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
      'Leadership',
      "March Size",
      "Marching Speed",
      'Politics',
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

  field $MappedAttributeNames : reader;

  ADJUST {
    Readonly::Scalar my $temp => {
      'death into wounded rate'        => 'Death to Wounded',
      'death into survival rate'       => 'Death to Survival',
      'march size increase'            => 'March Size',
      'march size Capacity'            => 'March Size',
      'march size'                     => 'March Size',
      'the march size'                 => 'March Size',
      'wounded into death rate'        => 'Wounded to Death',
      'wounded into death'             => 'Wounded to Death',
      'mayor training speed'           => 'SubCity Training Speed',
      'mayor death into survival rate' => 'SubCity Death to Survival',
      'subordinate city troops’ death to survival rate' =>
        'SubCity Death to Survival',
      'subordinate city troops’ death to survival' =>
        'SubCity Death to Survival',
      'subordinate city troops’ death into survival rate' =>
        'SubCity Death to Survival',
      'subordinate city troops’ death into survival' =>
        'SubCity Death to Survival',
    };
    $MappedAttributeNames = $temp;
  }

  method string_to_attribute ($string) {
    my $exact = first { lc($_) eq lc($string) } $AttributeValues->@*;
    my $map   = $MappedAttributeNames;
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
    Readonly::Scalar my $temp => {
      'Against Monsters' => {
        "Overall"     => 0,
        "PvM"         => 1,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'Attacking' => {
        "Overall"     => 0,
        "PvM"         => 1,
        "Attacking"   => 1,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 1,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'brings a dragon' => {
        "Overall"     => 1,
        "PvM"         => 1,
        "Attacking"   => 1,
        "Reinforcing" => 1,
        "Defense"     => 1,
        "In City"     => 1,
        "Out City"    => 1,
        "Wall"        => 1,
        "Mayor"       => 1,
        "Officer"     => 0,
      },
      'brings a spiritual beast' => {
        "Overall"     => 1,
        "PvM"         => 1,
        "Attacking"   => 1,
        "Reinforcing" => 1,
        "Defense"     => 1,
        "In City"     => 1,
        "Out City"    => 1,
        "Wall"        => 1,
        "Mayor"       => 1,
        "Officer"     => 0,
      },
      'Defending' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 1,
        "Defense"     => 1,
        "In City"     => 1,
        "Out City"    => 1,
        "Wall"        => 1,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'During SvS' => {
        "Overall"     => 1,
        "PvM"         => 0,
        "Attacking"   => 1,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'In City' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 1,
        "Defense"     => 0,
        "In City"     => 1,
        "Out City"    => 0,
        "Wall"        => 1,
        "Mayor"       => 1,
        "Officer"     => 0,
      },
      'In Main City' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 1,
        "Defense"     => 0,
        "In City"     => 1,
        "Out City"    => 0,
        "Wall"        => 1,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'leading the army' => {
        "Overall"     => 1,
        "PvM"         => 1,
        "Attacking"   => 1,
        "Reinforcing" => 1,
        "Defense"     => 1,
        "In City"     => 1,
        "Out City"    => 1,
        "Wall"        => 1,
        "Mayor"       => 1,
        "Officer"     => 0,
      },
      'Marching' => {
        "Overall"     => 1,
        "PvM"         => 1,
        "Attacking"   => 1,
        "Reinforcing" => 1,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 1,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'Reinforcing' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 1,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 1,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'When City Mayor for this SubCity' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 1,
        "Officer"     => 0,
      },
      'When Defending Outside The Main City' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 1,
        "Defense"     => 1,
        "In City"     => 0,
        "Out City"    => 1,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'When Rallying' => {
        "Overall"     => 1,
        "PvM"         => 1,
        "Attacking"   => 1,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'When The Main Defense General' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 1,
        "Out City"    => 0,
        "Wall"        => 1,
        "Mayor"       => 0,
        "Officer"     => 0,
      },
      'you own the General' => {
        "Overall"     => 1,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 1,
      },
      # Officer positions 'When Appointed as Hospital Officer',
      'When Appointed as Prison Officer' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 1,
      },
      'When Appointed as Workshop Officer' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 1,
      },
      'When Appointed as Academy Officer' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 1,
      },
      'When Appointed as Embassy Officer' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 1,
      },
      'When Appointed as Barracks Officer' => {
        "Overall"     => 0,
        "PvM"         => 0,
        "Attacking"   => 0,
        "Reinforcing" => 0,
        "Defense"     => 0,
        "In City"     => 0,
        "Out City"    => 0,
        "Wall"        => 0,
        "Mayor"       => 0,
        "Officer"     => 1,
      },
    };
    $BuffConditionValues = $temp;
  }

  field $DebuffConditionValues : reader;

  field $BookConditionValues : reader;

  ADJUST {
    Readonly::Array my @bkcv => ("all the time", "when not mine");
    $BookConditionValues = \@bkcv;
  }

  ADJUST {
    Readonly::Array my @temp => ("Enemy", "Monsters",);
    $DebuffConditionValues = \@temp;
  }

  # this should be the maximum number of words in *either*
  # a direct phrase in a condition *or* a mapping to a condition
  field $words_condition_phrase : reader = 6;

  field $mapped_conditions : reader;

  ADJUST {
    Readonly::Scalar my $temp => {
      'In-Rally'                   => "When Rallying",
      'Reduces Monster'            => "Monsters",          # debuff version
      'brings any dragon'          => 'brings a dragon',
      'brings dragon'              => 'brings a dragon',
      'brings any spiritual beast' => 'brings a spiritual beast',
      'brings spiritual beast'     => 'brings a spiritual beast',
      'to attack Monsters'         => 'Against Monsters',         # buff version
      'to attack'                  => 'Attacking',
      'to reinforce'               => 'Reinforcing',
      'the Mayor'                     => "When City Mayor for this SubCity",
      'in this subordinate city'      => "When City Mayor for this SubCity",
      'in subordinate city'           => "When City Mayor for this SubCity",
      'launching Alliance War'        => 'When Rallying',
      'attacking Monsters'            => 'Against Monsters',
      'in-city'                       => 'In City',
      'from Monsters'                 => 'Against Monsters',
      'the main city defense general' => 'When The Main Defense General',
    };
    $mapped_conditions = $temp;
  }

  method string_to_condition ($string) {
    my $exactBuff   = first { $string =~ /$_/i } keys %{$BuffConditionValues};
    my $exactDebuff = first { $string =~ /$_/i } $DebuffConditionValues->@*;
    my $map         = $mapped_conditions;

    if ($exactBuff) {
      $self->logger->debug("Mapped Exact Buff $exactBuff");
      return $exactBuff;
    }
    elsif ($exactDebuff) {
      $self->logger->debug("Mapped Exact DeBuff $exactDebuff");
      return $exactDebuff;
    }
    elsif (exists $map->{$string}) {
      my $v = $map->{$string};
      unless (any { $_ eq $v }
        (keys %{$BuffConditionValues}, $DebuffConditionValues->@*)) {
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
      keys %{$BuffConditionValues},
      @{$DebuffConditionValues},
      @{$BookConditionValues}
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
      push @result, sort(keys %{$redAscendingLevelNames});
    }
    else {
      push @result, sort(keys %{$purpleAscendingLevelNames});
    }
    return @result;
  }

  method AscendingAttributeLevelNames ($isRed = 1) {
    my @result = ('None');
    my @valid  = $self->AscendingAttributeLevelValues($isRed);
    if ($isRed) {
      foreach my $index (1 .. 5) {
        my $key = $valid[$index];
        push @result, $redAscendingLevelNames->{$key};
      }
    }
    else {
      foreach my $index (1 .. 5) {
        my $key = $valid[$index];
        push @result, $redAscendingLevelNames->{$key};
      }
    }
    return @result;
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
    foreach my $gk (@{$GeneralKeys}) {
      my $specific_base = uuid5($UUID5_Generals_base, $gk);
      $UUID5_Generals->{$gk} = $specific_base;
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

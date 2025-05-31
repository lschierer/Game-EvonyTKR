use v5.40.0;
use experimental qw(class);
use utf8::all;

use FindBin;
use lib "$FindBin::Bin/../../../lib";

class Game::EvonyTKR::Model::Covenant : isa(Game::EvonyTKR::Model::Logger) {
# PODNAME: Game::EvonyTKR::Model::Covenant
  use builtin         qw(indexed);
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common   qw( t);
  use Type::Utils     qw(is enum);
  use Carp;
  use Data::Dumper;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Game::EvonyTKR::Model::Buff;
  use Game::EvonyTKR::Model::Buff::Value;
  use List::MoreUtils;
  use Util::Any -all;
  use YAML::XS qw{LoadFile Load};
  use namespace::autoclean;
  use Game::EvonyTKR::Model::Logger;
  use overload
    '""'       => \&_toString,
    "fallback" => 1;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $primary : reader : param;

  field %secondary : reader;

  field @secondaryKeys : reader = qw( one two three );

  ADJUST {
    #I really want just the three generals in this hash.
    lock_keys_plus(%secondary, @secondaryKeys);
  }

  field %Buffs : reader;

  field $CovenantLevels =
    enum ['War', 'Cooperation', 'Peace', 'Faith', 'Honor', 'Civilization',];

  ADJUST {
    #Covenants only have these levels.
    my @levels = @{ $CovenantLevels->values() };
    lock_keys_plus(%Buffs, @levels);
    for my ($index, $lv) (indexed(@levels)) {
      my $al = 10000 + $index * 2 * 1000;
      $self->logger()->trace("setting activationLevel for $lv to $al");
      $Buffs{$lv} = {
        activationLevel => $al,
        activated       => 0,
      };
    }
  }

  method setSecondary ($position, $general) {
    if (any { $_ =~ /$position/i } @secondaryKeys) {
      my $generalClass = blessed $general;
      my @classList    = split(/::/, $generalClass);
      if ($classList[2] =~ /General/) {
        $secondary{$position} = $general;
      }
      else {
        $self->logger()
          ->logcroak(
          "$generalClass is not a subclass of Game::EvonyTKR::Model::General");
      }
    }
    else {
      $self->logger()
        ->logwarn("$position is not a valid position, must be one of "
          . np @secondaryKeys);
    }
  }

  method addBuff($level, $nb, $inherited = 0) {
    if (blessed $nb ne 'Game::EvonyTKR::Model::Buff') {
      return 0;
    }
    my $tcheck = $CovenantLevels->compiled_check();
    if (none { $tcheck->($_) } ($CovenantLevels)) {
      return 0;
    }

    my @levelValues = @{ $CovenantLevels->values() };
    for my $tl (@levelValues) {
      if ($tl eq $level) {
        if ($inherited) {
          my $copy;
          if ($nb->has_buffClass()) {
            $copy = Game::EvonyTKR::Model::Buff->new(
              attribute => $nb->attribute(),
              value     => $nb->value(),
              buffClass => $nb->buffClass(),
              inherited => 1,
            );
          }
          else {
            $copy = Game::EvonyTKR::Model::Buff->new(
              attribute => $nb->attribute(),
              value     => $nb->value(),
              inherited => 1,
            );
          }
          if ($nb->has_condition()) {
            for my $c ($nb->condition()) {
              $copy->set_condition($c);
            }
          }
          $self->logger()
            ->trace("Adding inherited buff at level $tl " . np $copy);
          push @{ $Buffs{$tl}->{'buffs'} }, $copy;
        }
        else {
          $self->logger()
            ->trace("Adding uninherited buff at level $tl " . np $nb);
          push @{ $Buffs{$tl}->{'buffs'} }, $nb;
        }
        if ($tl eq 'War') {
          $self->addBuff('Cooperation', $nb, 1);
        }
        elsif ($tl eq 'Cooperation') {
          $self->addBuff('Peace', $nb, 1);
        }
        elsif ($tl eq 'Peace') {
          $self->addBuff('Faith', $nb, 1);
        }
        elsif ($tl eq 'Faith') {
          $self->addBuff('Honor', $nb, 1);
        }
        elsif ($tl eq 'Honor') {
          $self->addBuff('Civilization', $nb, 1);
        }
        last;
      }
    }
    return 1;
  }

  method activeBuffs() {
    my @cla        = @{ $CovenantLevels->values() };
    my $powerLevel = 0;
    $powerLevel += $primary->effective_attack();
    $powerLevel += $primary->effective_defense();
    $powerLevel += $primary->effective_leadership();
    $powerLevel += $primary->effective_politics();
    for my $key (@{ keys %secondary }) {
      $powerLevel += $secondary{$key}->effective_attack();
      $powerLevel += $secondary{$key}->effective_defense();
      $powerLevel += $secondary{$key}->effective_leadership();
      $powerLevel += $secondary{$key}->effective_politics();
    }
    $self->logger()->debug(sprintf(
      "Covenant for %s: effective powerLevel is %.5f",
      $primary->name(), $powerLevel
    ));
    for my ($i, $clk) (indexed(@cla)) {
      my $cl = $Buffs{$clk};
      if ($cl->{'activated'}) {
        if ($cl->{'activationLevel'} <= $powerLevel) {
          return @{ $cl->{'buffs'} };
        }
        else {
          $self->logger()->debug(sprintf(
"Covenant for %s: effective powerLevel is %.5f, required power is %d",
            $primary->name(), $powerLevel, $cl->{'activationLevel'},
          ));
        }
      }
      else {
        $self->logger()->debug(sprintf(
          "Covenant for %s: level %s is not active",
          $primary->name(), $cla[$i]
        ));
      }
    }
    return ();
  }

  method getEvAnsScore($name, $resultRef, $BuffMultipliers, $GeneralBias) {
    my @ab = $self->activeBuffs();
    my $bc = scalar @ab;
    $self->logger()->debug("getEvAnsScore for $name found $bc buffs");
    if ($bc > 0) {
      for my ($i, $thisBuff) (indexed(@ab)) {
        my $result =
          $thisBuff->getEvAnsScore($name, $BuffMultipliers, $GeneralBias,);
        $self->logger()
          ->debug(
"getEvAnsScore for $name recieved $result from getEvAnsScore for buff $i"
          );
        my $category = $BuffMultipliers->EvAnsCategory($thisBuff);
        if (not defined $category) {
          $self->logger()
            ->warn("getEvAnsScore for $name found no category returned for "
              . np $thisBuff);
          $category = 'Unused';
        }
        else {
          $self->logger()
            ->debug("getEvAnsScore for $name found category $category for "
              . np $thisBuff);
        }
        $resultRef->{'SPS'}->{$category} += $result;
        $self->logger()
          ->debug("getEvAnsScore for $name; $category currently has value: "
            . $resultRef->{'SPS'}->{$category});
        $resultRef->{$category}->{'SPS'} += $result;
        $self->logger()
          ->trace(
          "getEvAnsScore for $name; $category -> SPS  currently has value: "
            . $resultRef->{$category}->{'SPS'});

      }
    }
  }

  method toHashRef($verbose = 0) {
    $self->logger()
      ->trace("Starting toHashRef for Covenant, verbose is $verbose");
    my $returnRef = {
      primary   => $primary->name(),
      secondary => {
        one   => $secondary{'one'},
        two   => $secondary{'two'},
        three => $secondary{'three'},
      },
    };
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method readFromFile() {
    my $CovenantFile = $primary->name() . '.yaml';
    $self->logger()->debug("about to get $CovenantFile");
    my $CovenantShare =
      File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'covenants');
    my $FileWithPath = File::Spec->catfile($CovenantShare, $CovenantFile);
    if (-T -s -r $FileWithPath) {
      $self->logger()->debug("$CovenantFile exists as expected");
      my $data = LoadFile($FileWithPath);

      my @fileGenerals = @{ $data->{'generals'} };
      if (scalar @fileGenerals != 3) {
        $self->logger()
          ->logcroak(
          "Wrong number of generals in covenant for " . $primary->name());
      }
      else {
        $secondary{'one'}   = $fileGenerals[0];
        $secondary{'two'}   = $fileGenerals[1];
        $secondary{'three'} = $fileGenerals[2];
      }
      my @levels     = @{ $CovenantLevels->values() };
      my @fileLevels = @{ $data->{'levels'} };

      for my ($index, $entry) (indexed(@fileLevels)) {
        my $value = $entry->{'category'};
        if (none { $_ eq $entry->{'category'} } @levels) {
          $self->logger()
            ->logcroak("$value is invalid; expected one of " . np @levels);
        }
        else {
          $Buffs{$value}->{'type'} = $entry->{type};
          $Buffs{$value}->{'buff'} = {};

          my @eBuffs = @{ $entry->{'buff'} };
          foreach my $eb (@eBuffs) {
            my $v;
            my $b;

            my @ebKeys = keys %{$eb};
            if (any { $_ eq 'value' } @ebKeys) {
              $self->logger()
                ->debug("$CovenantFile at $value has a buff with a value");
              $v = Game::EvonyTKR::Model::Buff::Value->new(
                number => $eb->{'value'}->{'number'},
                unit   => $eb->{'value'}->{'unit'},
              );
              if (any { $_ =~ /class/i } @ebKeys) {
                $b = Game::EvonyTKR::Model::Buff->new(
                  attribute => $eb->{'attribute'},
                  value     => $v,
                  buffClass => $eb->{'class'},
                );
              }
              else {
                $b = Game::EvonyTKR::Model::Buff->new(
                  attribute => $eb->{'attribute'},
                  value     => $v,
                );
              }
            }
            else {
              $self->logger()
                ->warn("$CovenantFile at $value has a buff without a value");
            }
            if (defined $b) {
              if (any { $_ eq 'condition' } @ebKeys) {
                my @conditions = @{ $eb->{'condition'} };
                for my $ebc (@conditions) {
                  $b->set_condition($ebc);
                }
              }
            }
            $self->addBuff($value, $b);
            $self->logger()->info("$CovenantFile at $value adds buff " . np $b);
          }
        }
      }
    }
    else {
      $self->logger()->error("cannot find $CovenantFile");
    }
  }

};
1;

__END__

# ABSTRACT: Module for processing information about Evony TKR General Covenants

=head1 DESCRIPTION

Covenants are one of several ways that a General can provide Buffs for Troops. 

Covenants differ from other ways in that a Covenant, while associated with a particular General, requires the possession and development of at least one other, and possibly as many as three other, Generals.  It also differs in that some of the effects of the Covenants are what I refer to as "Passive" Buffs, that is the buff is active all the time, not just when that particular General is in use. 

To get the Buff for a given Covenant Level, you must

=for :List

* Have the primary General

* Activate the Covenant to the desired level

* The Primary and Secondary Generals' Basic attributes added together must equal or exceed the minimum value for that level.

Thus Cultivating and Ascending Generals from the list for each Covenant will help obtain the Buffs provided by that Covenant, but Specialities, Armor, and Beasts/Dragons will not. because while these increase the General's power, they do only Cultivating and Ascending increase the General's Basic Attributes.

This makes figuring out when to include these buffs I<much> more complicated than any other Buff so far considered.  Typically I look solely at Pairs of Generals.  For a Covenant, I need to consider your I<entire inventory> of Generals,
as there are passive buffs always active, and the supporting Generals may not be part of the Pair, but their basic attributes are still critical to determine the activation conditions. 

Todo:  actually get the activation conditions for Covenants right.  For now I am depending on the user to set activated or not which while required is not sufficient. 
=cut

=method primary()

Returns the Primary Game::EvonyTKR::Model::General with whom this Covenant is associated. 

=cut 

=method secondary()

returns a hash containing the secondary or supporting Game::EvonyTKR::Model::General objects for this covenant.
=cut

=method secondaryKeys()

returns an array of the keys that are allowed values for the hash in secondary()
or in the setSecondary() method. 
=cut

=method setSecondary($position, $general)

sets the Game::EvonyTKR::Model::General $general as a supporting general in $position in the %secondary hash. 
the $position field must use one of the values returned by secondaryKeys().
=cut

=method CovenantLevels()

returns a reference to Type::Tiny::Enum of possible levels for the Covenant.
=cut

=method Buffs()

returns a hash of the Buffs for this Covenant
The primary keys are the values from CovenantLevels(), each of which returns
a HashRef. 
=cut

=method addBuff($level, $nb, $inherited = 0)

adds the Game::EvonyTKR::Model::Buff to the specified $level so long as $level is a valid selection from CovenantLevels().

$inherited should not be used by external callers, it is used internally to set up the Buff structure such that each Level contains all Buffs from the previous levels, but marked as such. 

Todo:  This approach is flawed, see the note about properly accounting for the complicated activation. 
=cut

method toHashRef() 

returns a HashRef that can be passed a JSON serializer successfully. 
=cut

method ""

calls a JSON serializer on the HashRef from toHashRef()
=cut

method readFromFile() 

reads the convenant data into memory from the YAML representation in the distro's shared data directory.
=cut

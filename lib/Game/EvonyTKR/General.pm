use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::General : isa(Game::EvonyTKR::Data) {

# PODNAME: Game::EvonyTKR::General
  use builtin qw(indexed);
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils   qw(is enum);
  use Util::Any -all;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Game::EvonyTKR::Covenant;
  use Game::EvonyTKR::Data;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  require Game::EvonyTKR::Buff::EvaluationData::Attacking;
  require Game::EvonyTKR::Buff::EvaluationData::Monster;
  use Game::EvonyTKR::Ascending;
  use JSON::MaybeXS;
  use UUID     qw(uuid5);
  use YAML::XS qw{LoadFile Load};
  use namespace::autoclean;

# VERSION
  use File::FindLib 'lib';
  use overload
    '<=>' => \&_comparison,
    'cmp' => \&_comparison,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $name : reader : param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $uuid : reader;

  ADJUST {
    $uuid = uuid5($self->UUID5_base(), $name);
  }

  field $_generalType : reader = 'All Troops';

  field $leadership : reader = 0;

  field $leadership_increment : reader = 0;

  field $attack : reader = 0;

  field $attack_increment : reader = 0;

  field $defense : reader = 0;

  field $defense_increment : reader = 0;

  field $politics : reader = 0;

  field $politics_increment : reader = 0;

  field $level : reader : param //= 45;

  field @specialities : reader;

  field $ascending : reader : param //= true;

  field $ascendingAttributes : reader //= Game::EvonyTKR::Ascending->new();

  field $builtInBook : reader;

  field @otherBooks : reader;

  field $hasCovenant : reader = false;

  field %BasicAESAdjustment = (
    'None'    => 0,
    '1Purple' => 0,
    '2Purple' => 0,
    '3Purple' => 0,
    '4Purple' => 0,
    '5Purple' => 0,
    '1Red'    => 10,
    '2Red'    => 20,
    '3Red'    => 30,
    '4Red'    => 40,
    '5Red'    => 50,
  );

  ADJUST {
    lock_keys(%BasicAESAdjustment);
  }

  method _getEvAns4BasicAttributes($resultRef, $BuffMultipliers) {

    $resultRef->{'BAS'}->{'Attack'} =
      $self->effective_attack() *
      $BuffMultipliers->getMultiplierForBuff('Attack', $self->generalType());

    $resultRef->{'BAS'}->{'Defense'} =
      $self->effective_defense() *
      $BuffMultipliers->getMultiplierForBuff('Defense', $self->generalType());

    $resultRef->{'BAS'}->{'Leadershp'} =
      $self->effective_leadership() *
      $BuffMultipliers->getMultiplierForBuff('HP', $self->generalType());

    $resultRef->{'BAS'}->{'Politics'} =
      $self->effective_politics() *
      $BuffMultipliers->getMultiplierForBuff('Death to Wounded',
      $self->generalType());

    for my $key (keys %{ $resultRef->{'BAS'} }) {
      $resultRef->{'BAS'}->{'Overall'} += $resultRef->{'BAS'}->{$key};
    }

    $resultRef->{'Attack'}->{'BAS'} += $resultRef->{'BAS'}->{'Attack'};

    $resultRef->{'Toughness'}->{'BAS'} += $resultRef->{'BAS'}->{'Defense'};

    $resultRef->{'Toughness'}->{'BAS'} +=
      $resultRef->{'BAS'}->{'Leadershp'};

    $resultRef->{'Preservation'}->{'BAS'} +=
      $resultRef->{'BAS'}->{'Politics'};

  }

  method getEvAnsScoreAsPrimary($situation) {
    my $resultRef = $self->computeEvansScoreComponents($situation);
    my $BAS =
      exists $resultRef->{'BAS'}->{'Overall'}
      ? $resultRef->{'BAS'}->{'Overall'}
      : 0;
    my $SBS =
      exists $resultRef->{'SBS'}->{'Overall'}
      ? $resultRef->{'SBS'}->{'Overall'}
      : 0;
    my $CVS =
      exists $resultRef->{'CVS'}->{'Overall'}
      ? $resultRef->{'CVS'}->{'Overall'}
      : 0;
    my $SPS =
      exists $resultRef->{'SPS'}->{'Overall'}
      ? $resultRef->{'SPS'}->{'Overall'}
      : 0;
    my $AES =
      exists $resultRef->{'AES'}->{'Overall'}
      ? $resultRef->{'AES'}->{'Overall'}
      : 0;
    my $TLGS = $BAS + $SBS + $CVS + $SPS + $AES;
    $self->logger()->trace("BAS for $name is " . $BAS);
    $self->logger()->trace("SBS for $name is " . $SBS);
    $self->logger()->trace("CVS for $name is " . $CVS);
    $self->logger()->trace("SPS for $name is " . $SPS);
    $self->logger()->trace("AES for $name is " . $AES);
    $self->logger()->info("TLGS as Primary for $name is $TLGS");
    return $TLGS;
  }

  method getEvAnsScoreAsSecondary($situation) {
    my $resultRef = $self->computeEvansScoreComponents($situation);
    my $SBS =
      exists $resultRef->{'SBS'}->{'Overall'}
      ? $resultRef->{'SBS'}->{'Overall'}
      : 0;
    my $CVS =
      exists $resultRef->{'CVS'}->{'Overall'}
      ? $resultRef->{'CVS'}->{'Overall'}
      : 0;
    my $SPS =
      exists $resultRef->{'SPS'}->{'Overall'}
      ? $resultRef->{'SPS'}->{'Overall'}
      : 0;
    my $TLGS = $SBS + $CVS + $SPS;
    $self->logger()->trace("SBS for $name is " . $SBS);
    $self->logger()->trace("CVS for $name is " . $CVS);
    $self->logger()->trace("SPS for $name is " . $SPS);
    $self->logger()->info("TLGS as Secondary for $name is $TLGS");
    return $TLGS;
  }

  method computeEvansScoreComponents($situation) {
    $self->logger()
      ->trace("computeEvansScoreComponents for $name with $situation");
    my $BuffMultipliers;
    my $resultRef = {
      'BAS'          => {},
      'SBS'          => {},
      'CVS'          => {},
      'SPS'          => {},
      'AES'          => {},
      'Attack'       => {},
      'Toughness'    => {},
      'Preservation' => {},
    };
    if ($situation =~ /Attacking/i) {
      $BuffMultipliers = Game::EvonyTKR::Buff::EvaluationData::Attacking->new();
    }
    elsif ($situation =~ /Monster/i) {
      $BuffMultipliers = Game::EvonyTKR::Buff::EvaluationData::Monster->new();
    }
    else {
      $self->logger()->error("Unsupported situation $situation");
      return 0;
    }

    $self->_getEvAns4BasicAttributes($resultRef, $BuffMultipliers);

    if (defined $builtInBook) {
      $builtInBook->getEvAnsScore($self->name(), $resultRef,
        $BuffMultipliers, $self->generalType(),);
    }

    if (scalar @otherBooks >= 1) {
      for my $book (@otherBooks) {
        $book->getEvAnsScore($self->name(), $resultRef,
          $BuffMultipliers, $self->generalType(),);
      }
    }

    for my $key (keys %{ $resultRef->{'SBS'} }) {
      if ($key ne 'Overall') {
        $resultRef->{'SBS'}->{'Overall'} += $resultRef->{'SBS'}->{$key};
      }
    }

    for my $sp (@specialities) {
      $sp->getEvAnsScore($self->name(), $resultRef,
        $BuffMultipliers, $self->generalType(),);
    }
    for my $key (keys %{ $resultRef->{'SPS'} }) {
      if ($key ne 'Overall') {
        $resultRef->{'SPS'}->{'Overall'} += $resultRef->{'SPS'}->{$key};
      }
    }

    if ($ascending) {
      $ascendingAttributes->getEvAnsScore($self->name(), $resultRef,
        $BuffMultipliers, $self->generalType(),);

      for my $key (keys %{ $resultRef->{'AES'} }) {
        if ($key ne 'Overall') {
          $resultRef->{'AES'}->{'Overall'} +=
            $resultRef->{'AES'}->{$key};
        }
      }
    }
    else {
      $resultRef->{'AES'}->{'Overall'} += 0;
    }

    if ($hasCovenant) {

    }

    for my $key (keys %{ $resultRef->{'Attack'} }) {
      $resultRef->{'Attack'}->{'Overall'} +=
        $resultRef->{'Attack'}->{$key};
    }

    for my $key (keys %{ $resultRef->{'Toughness'} }) {
      $resultRef->{'Toughness'}->{'Overall'} +=
        $resultRef->{'Toughness'}->{$key};
    }

    for my $key (keys %{ $resultRef->{'Preservation'} }) {
      $resultRef->{'Preservation'}->{'Overall'} +=
        $resultRef->{'Preservation'}->{$key};
    }
    return $resultRef;
  }

  method generalType() {
    $self->logger()->debug(sprintf(
      '%s is of type %s with class %s',
      $name, $self->_generalType, blessed $self,
    ));
    return $self->_generalType;
  }

  method addBuiltInBook($newBook) {
    my $type = blessed $newBook;
    if ($type ne 'Game::EvonyTKR::SkillBook::Special') {
      $self->logger()
        ->logcroak(
        "builtInBook must be a Game::EvonyTKR::SkillBook::Special, not $type");
    }
    $self->logger()
      ->trace(
      "adding builtInBook with type $type and name " . $newBook->name());
    $builtInBook = $newBook;
  }

  method setCovenant($ce) {
    if ($ce) {
      $hasCovenant = 1;
    }
    else {
      $hasCovenant = 0;
    }
  }

  method validation {
    my @errors;

    my $type = t('Bool');
    $type->check($ascending)
      or push @errors => "ascending must be a bool, not $ascending";

    $type = t('PositiveOrZeroNum');
    is_Num($leadership)
      or push @errors => "leadership must be a number, not $leadership";
    $type->check($leadership)
      or push @errors => "leadership must be positive, not $leadership";

    is_Num($leadership_increment)
      or push @errors =>
      "leadership_increment must be a number, not $leadership_increment";
    $type->check($leadership_increment)
      or push @errors =>
      "leadership_increment must be positive, not $leadership_increment";

    is_Num($attack)
      or push @errors => "attack must be a number, not $attack";
    $type->check($attack)
      or push @errors => "attack must be positive, not $attack";

    is_Num($attack_increment)
      or push @errors =>
      "attack_increment must be a number, not $attack_increment";
    $type->check($attack_increment)
      or push @errors =>
      "attack_increment must be positive, not $attack_increment";

    is_Num($defense)
      or push @errors => "defense must be a number, not $defense";
    $type->check($defense)
      or push @errors => "defense must be positive, not $defense";

    is_Num($defense_increment)
      or push @errors =>
      "defense_increment must be a number, not $defense_increment";
    $type->check($defense_increment)
      or push @errors =>
      "defense_increment must be positive, not $defense_increment";

    is_Num($politics)
      or push @errors => "politics must be a number, not $politics";

    is_Num($politics_increment)
      or push @errors =>
      "politics_increment must be a number, not $politics_increment";
    $type->check($politics_increment)
      or push @errors =>
      "politics_increment must be positive, not $politics_increment";

    my $pInt = t('PositiveOrZeroInt');
    is_Int($level)
      or push @errors => "level must be an integer, not $level";
    $pInt->check($level)
      or push @errors => "level must be positive, not $level";
    $type = t('IntRange[1, 45]');
    $type->check($level)
      or push @errors => "level must be between 1 and 45 inclusive";

    if (@errors) {
      die join ', ' => @errors;
    }
  }

  method _adjustBasicAttribute($attribute, $attribute_increment) {
    my $AES_adjustment = 0;

    if ($ascending) {
      $self->logger()->trace($self->name() . " is ascended");
      my $stars = $ascendingAttributes->activeLevel();
      if (not $stars =~ /None/i) {
        $AES_adjustment = $BasicAESAdjustment{$stars};
        if ($AES_adjustment == 0) {
          $self->logger()
            ->trace($self->name()
              . " did not match any value for stars $stars in BasicAESAdjustment: "
              . np %BasicAESAdjustment);
        }
      }
    }
    $self->logger()
      ->trace("for "
        . $self->name()
        . " level is $level, attribute_increment is $attribute_increment,  attribute is $attribute"
      );
    my $step = $level * $attribute_increment + $attribute;
    $self->logger()->trace("step1 for " . $self->name() . " is $step");
    $step = $step + 500;
    $self->logger()->trace("step2 for " . $self->name() . " is $step");
    $step = $step + $AES_adjustment;
    $self->logger()->trace("step3 for " . $self->name() . " is $step");

    if ($step < 900) {
      $step = $step * 0.1;
    }
    else {
      $step = 90 + ($step - 900) * 0.2;
    }
    $self->logger()->trace("step4 for " . $self->name() . " is $step");
    return $step;
  }

  method effective_leadership() {
    $self->logger()
      ->trace('computing effective_leadership for ' . $self->name());
    return $self->_adjustBasicAttribute($self->leadership(),
      $self->leadership_increment());
  }

  method effective_attack() {
    $self->logger()->trace('computing effective_attack for ' . $self->name());
    return $self->_adjustBasicAttribute($self->attack(),
      $self->attack_increment());
  }

  method effective_defense() {
    $self->logger()->trace('computing effective_defense for ' . $self->name());
    return $self->_adjustBasicAttribute($self->defense(),
      $self->defense_increment());
  }

  method effective_politics() {
    $self->logger()->trace('computing effective_politics for ' . $self->name());
    return $self->_adjustBasicAttribute($self->politics(),
      $self->politics_increment());
  }

  method changeActiveSpecialityLevel($specialityNumber, $newLevel) {
    my $type = t('PositiveOrZeroInt');
    if ($type->check($specialityNumber)) {
      $type = t('IntRange[1, 4]');
      if ($type->check($specialityNumber)) {
        if (scalar @specialities < $specialityNumber) {
          $self->logger()->warn(sprintf(
            '%s has %d specialities defined, not %d.',
            $name, scalar @specialities,
            $specialityNumber
          ));

        }
        else {
          $specialityNumber--;

          my $tcheck = $self->specialityLevels()->compiled_check();
          if (not $tcheck->($newLevel)) {
            $self->logger()->warn(sprintf(
              'Detected invalid SpecialityLevel %s for %d in %s, valid options are %s',
              $newLevel, $specialityNumber + 1,
              $name,     Data::Printer::np $self->specialityLevels()->values()
            ));
          }
          else {
            # I am ready to actually do work
            if ($specialityNumber == 3) {
              my @otherLevels = ();
              for my ($l, $v) (indexed(@specialities)) {
                if ($l < 4) {
                  push @otherLevels, $v->activeLevel();
                  if ($v->activeLevel() !~ /Gold/i) {
                    $self->logger()
                      ->debug(
                      "detected that one of the first 3 is not Gold.  Setting #4 to None."
                      );
                    $specialities[3]->setActiveLevel('None');
                  }
                }
              }
              if (all { $_ =~ /Gold/i } @otherLevels) {
                $self->logger()
                  ->debug(
                  "detected that the first 3 are all Gold.  Setting #4 to $newLevel as requested."
                  );
                $specialities[3]->setActiveLevel($newLevel);
              }
            }
            elsif ($newLevel !~ /Gold/i) {
              if ($specialityNumber != 3) {
                my $debugSN = $specialityNumber + 1;

                $self->logger()->debug(sprintf(
                  'Detected that %s for %d is not Gold.  Ensuring that #4 is None.',
                  $newLevel, $debugSN
                ));
                $specialities[3]->setActiveLevel('None');
              }
              $specialities[$specialityNumber]->setActiveLevel($newLevel);
            }
            else {
              $specialities[$specialityNumber]->setActiveLevel($newLevel);
              $self->logger()->debug(sprintf(
                'Detected that %s for %d is Gold. Recursive call for #4 just in case.',
                $newLevel, $specialityNumber + 1
              ));
              $self->changeActiveSpecialityLevel(4, 'Green');
            }
          }
        }
      }
      else {
        $self->logger()
          ->warn(
          "detected invalid speciality number $specialityNumber for $name");
      }
    }
    else {
      $self->logger()
        ->warn(
        "detected invalid speciality number $specialityNumber for $name");
    }
  }

  method addAnotherBook($newBook) {
    my $bookClass = blessed $newBook;
    my @classList = split(/::/, $bookClass);
    if ($classList[2] ne 'SkillBook') {
      $self->logger()
        ->logcroak(
        "Attempt to add $bookClass which is not a 'Game::EvonyTKR::SkillBook' to $name"
        );
    }
    if ($bookClass ne 'Game::EvonyTKR::SkillBook::Special') {
      $self->logger()
        ->croak(
        "Attempt to add $bookClass which is not a 'Game::EvonyTKR::SkillBook::Special' to $name"
        );
    }
    push @otherBooks, $newBook;
    $self->logger()->debug("added SkillBook " . $newBook->name() . " to $name");
  }

  method addSpeciality($newSpeciality) {
    my $specialityClass = blessed $newSpeciality;
    my @classList       = split(/::/, $specialityClass);
    if ($classList[2] ne 'Speciality') {
      croak
      "Attemp to add $specialityClass which is not a 'Game::EvonyTKR::Speciality' to $name";
    }
    push @specialities, $newSpeciality;
    $self->logger()
      ->debug("added Speciality " . $newSpeciality->name() . " to $name");
    $self->logger()
      ->trace(sprintf('this results in %d specialities for %s',
      scalar @specialities, $name));
  }

  method setLevel($newLevel) {
    if (is_Int($newLevel)) {
      my $type = t('IntRange[1, 45]');
      if ($type->check($newLevel)) {
        $level = $newLevel;
      }
    }
  }

  method toHashRef($verbose = 0) {
    my $returnRef = {
      name  => $name,
      level => $level,
      type  => $self->generalType(),
    };

    $self->logger()->trace("add covenant info to returnRef for $name");
    $returnRef->{hasCovenant} = $self->hasCovenant();

    $self->logger()->trace("add book stuff to returnRef for $name");
    $returnRef->{builtInBook} =
      defined $self->builtInBook
      ? $self->builtInBook->toHashRef($verbose)
      : {};

    my @sbRefs;
    for my $ob (@otherBooks) {
      push @sbRefs, $ob->toHashRef($verbose);
      $self->logger()->trace('General toHashRef @sbRefs ' . np @sbRefs);
    }
    $returnRef->{otherBooks} = \@sbRefs;

    $self->logger()->trace("add specialities to returnRef for $name");
    my @specialityRefs;
    for my $sp (@specialities) {
      push @specialityRefs, $sp->toHashRef($verbose);
    }
    $returnRef->{specialities} = \@specialityRefs;

    $self->logger()->trace("add ascending attributes to returnRef for $name");
    $returnRef->{ascendingAttributes} =
      $self->ascendingAttributes()->toHashRef($verbose);

    $returnRef->{EvAnsScores} = {
      AttackingAsPrimary   => $self->getEvAnsScoreAsPrimary('Attacking'),
      AttackingAsSecondary => $self->getEvAnsScoreAsSecondary('Attacking'),
      MonsterAsPrimary     => $self->getEvAnsScoreAsPrimary('Monster'),
      MonsterAsSecondary   => $self->getEvAnsScoreAsSecondary('Monster'),
    };
    if ($verbose) {

      $self->logger()->trace("add basic attributes to returnRef for $name");
      $returnRef->{basicAttributes} = {
        leadership           => $self->leadership(),
        leadership_increment => $self->leadership_increment(),
        attack               => $self->attack(),
        attack_increment     => $self->attack_increment(),
        defense              => $self->defense(),
        defense_increment    => $self->defense_increment(),
        politics             => $self->politics(),
        politics_increment   => $self->politics_increment(),
      };

      $self->logger()->trace("add scores info to returnRef for $name");

      $returnRef->{ComponentScores} = {
        Attacking => $self->computeEvansScoreComponents('Attacking'),
        Monster   => $self->computeEvansScoreComponents('Monster'),
      };
      return $returnRef;
    }
    else {
      $returnRef->{basicAttributes} = {
        leadership => $self->effective_leadership(),
        attack     => $self->effective_attack(),
        defense    => $self->effective_defense(),
        politics   => $self->effective_politics(),
      };

      return $returnRef;
    }
  }

  method TO_JSON {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method _comparison($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'General') {
      my $od = p $other;
      croak "$od is not a Game::EvonyTKR::General";
    }
    return $self->name() cmp $other->name();
  }

  method _equality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'General') {
      $self->logger()->error('$other is not a Game::EvonyTKR::General');
      return 0;
    }
    if (__CLASS__->generalType() eq $other->generalType()) {
      return $self->name() eq $other->name();
    }
  }

  method _inequality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList  = split(/::/, $otherClass);
    if ($classList[2] ne 'General') {
      $self->logger()->error('$other is not a Game::EvonyTKR::General');
      return 0;
    }
    return not $self eq $other;
  }

}
1;

__END__
# ABSTRACT: Module for processing information about Evony TKR Generals.

=head1 DESCRIPTION

Generals in Evony TKR are one of the more complicated and simultaneously
most frequently changing things that a player must make decisions about.

This base class implements the attributes and methods common to all Generals, but should not be used directly.  Rather sub classes should be created for each of the crtical use cases in the game.  If the decision of which general to use does not require customized logic, it almost certainly is not critical enough to need particular consideration here either.

=cut

=method name

Returns the general's name, which will also be the key by which we find the general.
=cut

=method leadership

Returns the base value for leadership, one of the four basic attributes of a general.
=cut

=method leadership_increment

Returns the amount by which the effective value of leadership (as opposed to its base value) increases with each level gained
=cut

=method effective_leadership()

returns the value that a user of this general at this investment level will experience for leadership.
=cut

=method attack

Returns the base value for attack, one of the four basic attributes of a general.
=cut

=method attack_increment

Returns the amount by which the effective value of attack (as opposed to its base value) increases with each level gained
=cut

=method effective_attack()

returns the value that a user of this general at this investment level will experience for attack.
=cut

=method defense

Returns the base value for defense, one of the four basic attributes of a general.
=cut

=attr defense_increment

Returns the amount by which the effective value of defense (as opposed to its base value) increases with each level gained
=cut

=method effective_defense()

returns the value that a user of this general at this investment level will experience for defense.
=cut

=method politics

Returns the base value for politics, one of the four basic attributes of a general.
=cut

=attr politics_increment

Returns the amount by which the effective value of politics (as opposed to its base value) increases with each level gained
=cut

=method effective_politics()

returns the value that a user of this general at this investment level will experience for politics.
=cut

=method level

generals start at level 1 and can grow to level 45.  Thier effective statistics increase as they do so by the increments listed in the _increment versions of each attribute.

This returns the level at which the general is currently being evaluated. 
=cut

=method specialities()

This returns the array of Game::EvonyTKR::Speciality instances associated with this General
=cut

=method addSpeciality($newSpeciality) 

Used to add a Game::EvonyTKR::Speciality to this General.  

Todo:  Once Game::EvonyTKR::Speciality overloads comparison operators, use them to ensure uniqueness. 
=cut

=method changeActiveSpecialityLevel($specialityLevel, $newLevel)

This takes a number 1-4 indicating which to work on and a level from the options in Game::EvonyTKR::Speciality
It handles the situation that the 4th speciality can only be set if the other 3 are all Gold, and will automatically be Green once that is true.

TODO: handle the 5th special. 

=cut

=method ascending()

This returns true or false, for whether or not the general can be ascended. 
=cut

=method builtInBook

each general comes with one Game::EvonyTKR::SkillBook built in.  This will be an instance of the ::Special variety of SkillBook.

This returns this book.
=cut

=method otherBooks

Some generals have other Game::EvonyTKR::SkillBooks of type ::Special beyond the one universally
built in. Or rather, I have chosen to represnt the extra buffs given by a general's "skin" or 
optional outfit as if it were a second ::SkillBook::Special. 

Books added here can optionally influence calculations.
=cut

=method addAnotherBook($newBook)

This allows you to populate the otherBooks field with the buffs provided by the extra skins.
=cut

=method 

=method setCovenant($ce) 

if $ce is true, sets the value of hasCovenant to true. If $ce is false, sets hasCovenant to false.
=cut

=method hasCovenant()

returns true if the General has a corresponding Game::EvonyTKR::Covenant
=cut

=method toHashRef

In typescript, you can run almost any object through JSON.stringify() and get something usable. This method is essentially what would happen if you ran a Game::EvonyTKR::General through JSON.stringify() to get a JSON representation of it, but then immediately read it back in with JSON.parse into a perl hash (instead of allowing it to be detected as an object), with all the top level things in the JSON being the keys of the hash. 
=cut

=method ""
This returns as a string the results of JSON encoding the results of the Hash produced by the toHashRef function.
=cut

=method <=>

This simply compares on the General's name.  I can envison doing something based on a computed power score.
=cut

=method eq

This simply compares on the General's name.  If the names are the same, then the user of the class ought not
create a second General with different attributes.
=cut

=method ne

This simply compares on the General's name.  It is a strict inverse of the _equality, for convience.
=cut

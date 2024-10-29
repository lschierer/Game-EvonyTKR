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
  use Game::EvonyTKR::BasicAttributes;
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

  field $basic_attributes : reader = Game::EvonyTKR::BasicAttributes->new();

  field $level : reader : param //= 45;

  field @specialities : reader;

  field $ascending : reader : param //= true;

  field $ascendingAttributes : reader //= Game::EvonyTKR::Ascending->new();

  field $built_in_book : reader;

  field @other_books : reader;

  field $hasCovenant : reader = false;

  method _getEvAns4BasicAttributes($resultRef, $BuffMultipliers) {

    $resultRef->{'BAS'}->{'Attack'} =
      $self->basic_attributes()->attack()->score(
      $self->level(),
      $self->ascendingAttributes()->activeLevel(),
      $self->name(),
      $BuffMultipliers->getMultiplierForBuff(
        'Attack', $self->generalType(), 'attack'
      )
      );

    $resultRef->{'BAS'}->{'Defense'} =
      $self->basic_attributes()->defense()->score(
      $self->level(),
      $self->ascendingAttributes()->activeLevel(),
      $self->name(),
      $BuffMultipliers->getMultiplierForBuff(
        'Defense', $self->generalType(), 'defense'
      )
      );

    $resultRef->{'BAS'}->{'Leadershp'} =
      $self->basic_attributes()->leadershp()->score(
      $self->level(),
      $self->ascendingAttributes()->activeLevel(),
      $self->name(),
      $BuffMultipliers->getMultiplierForBuff(
        'HP', $self->generalType(), 'leadershp'
      )
      );

    $resultRef->{'BAS'}->{'Politics'} =
      $self->basic_attributes()->politics()->score(
      $self->level(),
      $self->ascendingAttributes()->activeLevel(),
      $self->name(),
      $BuffMultipliers->getMultiplierForBuff(
        'Death to Wounded',
        $self->generalType(), 'politics'
      )
      );

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

    if (defined $built_in_book) {
      $built_in_book->getEvAnsScore($self->name(), $resultRef,
        $BuffMultipliers, $self->generalType(),);
    }

    if (scalar @other_books >= 1) {
      for my $book (@other_books) {
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

  method addbuilt_in_book($newBook) {
    my $type = blessed $newBook;
    if ($type ne 'Game::EvonyTKR::SkillBook::Special') {
      $self->logger()
        ->logcroak(
        "built_in_book must be a Game::EvonyTKR::SkillBook::Special, not $type"
        );
    }
    $self->logger()
      ->trace(
      "adding built_in_book with type $type and name " . $newBook->name());
    $built_in_book = $newBook;
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
    push @other_books, $newBook;
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
    $self->logger()->trace(sprintf(
      'this results in %d specialities for %s',
      scalar @specialities, $name
    ));
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
      defined $self->built_in_book
      ? $self->built_in_book->toHashRef($verbose)
      : {};

    my @sbRefs;
    for my $ob (@other_books) {
      push @sbRefs, $ob->toHashRef($verbose);
      $self->logger()->trace('General toHashRef @sbRefs ' . np @sbRefs);
    }
    $returnRef->{other_books} = \@sbRefs;

    $self->logger()->trace("add specialities to returnRef for $name");
    my @specialityRefs;
    for my $sp (@specialities) {
      push @specialityRefs, $sp->toHashRef($verbose);
    }
    $returnRef->{specialities} = \@specialityRefs;

    $self->logger()->trace("add ascending attributes to returnRef for $name");
    $returnRef->{ascending} = $self->ascending();
    if($self->ascending()) {
      $returnRef->{ascendingAttributes} =
        $self->ascendingAttributes()->toHashRef($verbose);
    }

    if ($verbose) {
      $self->logger()->trace("add scores info to returnRef for $name");
      $returnRef->{EvAnsScores} = {
        AttackingAsPrimary   => $self->getEvAnsScoreAsPrimary('Attacking'),
        AttackingAsSecondary => $self->getEvAnsScoreAsSecondary('Attacking'),
        MonsterAsPrimary     => $self->getEvAnsScoreAsPrimary('Monster'),
        MonsterAsSecondary   => $self->getEvAnsScoreAsSecondary('Monster'),
      };
      $returnRef->{ComponentScores} = {
        Attacking => $self->computeEvansScoreComponents('Attacking'),
        Monster   => $self->computeEvansScoreComponents('Monster'),
      };
    }

    $self->logger()->trace("add basic attributes to returnRef for $name");
    $returnRef->{basicAttributes} = $self->basic_attributes()->_toHashRef($verbose,
      $self->level(), $self->ascendingAttributes()->activeLevel(), $self->name());

    return $returnRef;
  }

  method TO_JSON {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $self->toHashRef();
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

=method built_in_book

each general comes with one Game::EvonyTKR::SkillBook built in.  This will be an instance of the ::Special variety of SkillBook.

This returns this book.
=cut

=method other_books

Some generals have other Game::EvonyTKR::SkillBooks of type ::Special beyond the one universally
built in. Or rather, I have chosen to represnt the extra buffs given by a general's "skin" or
optional outfit as if it were a second ::SkillBook::Special.

Books added here can optionally influence calculations.
=cut

=method addAnotherBook($newBook)

This allows you to populate the other_books field with the buffs provided by the extra skins.
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

use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General :isa(Game::EvonyTKR::Logger){
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils qw(is enum);
  use Util::Any -all;
  use Data::Printer;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Buff::EvaluationMultipliers;
  use Game::EvonyTKR::Ascending;
  use JSON::MaybeXS;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::General
use overload
    '<=>' => \&_comparison,
    'cmp' => \&_comparison,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality,
    '""'  => \&_toString;

# ABSTRACT: Module for processing information about Evony TKR Generals.

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

  field $name :reader :param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $leadership :reader :param;

  field $leadership_increment :reader :param;

  field $attack :reader :param;

  field $attack_increment :reader :param;

  field $defense :reader :param;

  field $defense_increment :reader :param;

  field $politics :reader :param;

  field $politics_increment :reader :param;

  field $level :reader :param //= 45;

  field @specialities :reader;

  field $ascending :reader :param //= true;

  field $ascendingAttributes :reader :param //= Game::EvonyTKR::Ascending->new();

  use constant DEFAULT_BUFF_MULTIPLIERS => Game::EvonyTKR::Buff::EvaluationMultipliers->new();

  field $BuffMultipliers :reader :param //= __CLASS__->DEFAULT_BUFF_MULTIPLIERS;

  ADJUST {
    my @errors;
    my $type = blessed $BuffMultipliers;
    if($type ne 'Game::EvonyTKR::Buff::EvaluationMultipliers'){
      push @errors => "BuffMultipliers must be a Game::EvonyTKR::Buff::EvaluationMultipliers, not $type";
      if (@errors) {
        die join ', ' => @errors;
      }
    }
  }

  field $builtInBook :reader :param;

  field @otherBooks :reader;

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
    my @errors;
    my $type = blessed $builtInBook;
    if($type ne 'Game::EvonyTKR::SkillBook::Special'){
      push @errors => "builtInBook must be a Game::EvonyTKR::SkillBook::Special, not $type";
      if (@errors) {
        die join ', ' => @errors;
      }
    }
  }

  ADJUST {
    my @errors;
    
    my $type = t('Bool');
    $type->check($ascending) or push @errors => "ascending must be a bool, not $ascending";

    $type = t('PositiveOrZeroNum');
    is_Num($leadership) or push @errors => "leadership must be a number, not $leadership";
    $type->check($leadership) or push @errors => "leadership must be positive, not $leadership";

    is_Num($leadership_increment) or push @errors => "leadership_increment must be a number, not $leadership_increment";
    $type->check($leadership_increment) or push @errors => "leadership_increment must be positive, not $leadership_increment";
    
    is_Num($attack) or push @errors => "attack must be a number, not $attack";
    $type->check($attack) or push @errors => "attack must be positive, not $attack";
    
    is_Num($attack_increment) or push @errors => "attack_increment must be a number, not $attack_increment";
    $type->check($attack_increment) or push @errors => "attack_increment must be positive, not $attack_increment";
    
    is_Num($defense) or push @errors => "defense must be a number, not $defense";
    $type->check($defense) or push @errors => "defense must be positive, not $defense";
    
    is_Num($defense_increment) or push @errors => "defense_increment must be a number, not $defense_increment";
    $type->check($defense_increment) or push @errors => "defense_increment must be positive, not $defense_increment";
    
    is_Num($politics) or push @errors => "politics must be a number, not $politics";
    
    is_Num($politics_increment) or push @errors => "politics_increment must be a number, not $politics_increment";
    $type->check($politics_increment) or push @errors => "politics_increment must be positive, not $politics_increment";
    
    my $pInt = t('PositiveOrZeroInt');
    is_Int($level) or push @errors => "level must be an integer, not $level";
    $pInt->check($level) or push @errors => "level must be positive, not $level";
    $type = t('IntRange[1, 45]');
    $type->check($level) or push @errors => "level must be between 1 and 45 inclusive";

    if (@errors) {
      die join ', ' => @errors;
    }
  }

  method _adjustBasicAttribute($attribute, $attribute_increment) {
    my $AES_adjustment = 0;

    if($ascending){
      $self->logger()->trace($self->name() . " is ascended");
      my $stars = $ascendingAttributes->activeLevel();
      $AES_adjustment =$BasicAESAdjustment{$stars};
      if($AES_adjustment == 0) {
        $self->logger()->trace($self->name() . " did not match any value for stars $stars in BasicAESAdjustment: " . np %BasicAESAdjustment );
      }
    }
    $self->logger()->trace("for " . $self->name() . " level is $level, attribute_increment is $attribute_increment, attack is $attack");
    my $step = $level * $attribute_increment + $attribute;
    $self->logger()->trace("step1 for " . $self->name() . " is $step");
    $step = $step + 500;
    $self->logger()->trace("step2 for " . $self->name() . " is $step");
    $step = $step + $AES_adjustment;
    $self->logger()->trace("step3 for " . $self->name() . " is $step");
    if($step < 900) {
      $step = $step * 0.1;
    } else {
      $step = 90 + ($step - 900) * 0.2;
    }
    $self->logger()->trace("step4 for " . $self->name() . " is $step");
    return $step;
  }

  method effective_leadership() {
    $self->logger()->trace('computing effective_leadership for ' . $self->name());
    return $self->_adjustBasicAttribute($self->leadership(), $self->leadership_increment());
  }

  method effective_attack() {
    $self->logger()->trace('computing effective_attack for ' . $self->name());
    return $self->_adjustBasicAttribute($self->attack(), $self->attack_increment());
  }

  method effective_defense() {
    $self->logger()->trace('computing effective_defense for ' . $self->name());
    return $self->_adjustBasicAttribute($self->defense(), $self->defense_increment());
  }

  method effective_politics() {
    $self->logger()->trace('computing effective_politics for ' . $self->name());
    return $self->_adjustBasicAttribute($self->politics(), $self->politics_increment());
  }

  method is_ground_general() {
    return 0;
  }

  method is_mounted_general() {
    return 0;
  }

  method is_ranged_general() {
    return 0;
  }

  method is_siege_general() {
    return 0;
  }

  method is_wall_general() {
    return 0;
  }

  method is_mayor() {
    return 0;
  }

  method is_officer() {
    return 0;
  }

  method addAnotherBook($newBook) {
    my $bookClass = blessed $newBook;
    my @classList = split(/::/, $bookClass);
    if($classList[2] ne 'SkillBook'){
      croak "Attempt to add $bookClass which is not a 'Game::EvonyTKR::SkillBook' to $name";
    }
    if($bookClass ne 'Game::EvonyTKR::SkillBook::Special') {
      croak "Attempt to add $bookClass which is not a 'Game::EvonyTKR::SkillBook::Special' to $name";
    }
    push @otherBooks, $newBook;
    $self->logger()->debug("added SkillBook " . $newBook->name() . " to $name");
  }

  method addSpeciality($newSpeciality) {
    my $specialityClass = blessed $newSpeciality;
    my @classList = split(/::/, $specialityClass);
    if($classList[2] ne 'Speciality'){
      croak "Attemp to add $specialityClass which is not a 'Game::EvonyTKR::Speciality' to $name";
    }
    push @specialities, $newSpeciality;
    $self->logger()->debug("added Speciality " . $newSpeciality->name() . " to $name");
  }

  method setLevel( $newLevel ) {
    if(is_Int($newLevel) ) {
      my $type = t('IntRange[1, 45]');
      if( $type->check($newLevel)) {
        $level = $newLevel;
      }
    }
  }

  method toHashRef( $verbose = 0) {
    if($verbose) {
      return {
        name                  => $name,
        level                 => $level,
        leadership            => $self->leadership(),
        leadership_increment  => $self->leadership_increment(),
        attack                => $self->attack(),
        attack_increment      => $self->attack_increment(),
        defense               => $self->defense(),
        defense_increment     => $self->defense_increment(),
        politics              => $self->politics(),
        politics_increment    => $self->politics_increment(),
        ascendingAttributes   => $self->ascendingAttributes()->toHashRef(1),
      };
    } else {
      return {
        name                  => $name,
        level                 => $level,
        leadership            => $self->effective_leadership(),
        attack                => $self->effective_attack(),
        defense               => $self->effective_defense(),
        politics              => $self->effective_politics(),
        ascendingAttributes   => $self->ascendingAttributes()->toHashRef(),
      };
    }
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method _comparison($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList = split(/::/, $otherClass);
    if($classList[2] ne 'General') {
      my $od = p $other;
      croak "$od is not a Game::EvonyTKR::General";
    }
    return $self->name() cmp $other->name();
  }

  method _equality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList = split(/::/, $otherClass);
    if($classList[2] ne 'General') {
      croak '$other is not a Game::EvonyTKR::General'
    }
    return $self->name() eq $other->name();
  }

  method _inequality ($other, $swap = 0) {
    my $otherClass = blessed $other;
    my @classList = split(/::/, $otherClass);
    if($classList[2] ne 'General') {
      croak '$other is not a Game::EvonyTKR::General'
    }
    return $self->name() ne $other->name();
  }
}
1;

__END__

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

=method BuffMultipliers

when evaluating generals, not all buffs are equally important.  Nor are these scaling factors constant across the game, but rather differ both by type of general and by situation.  
This returns the scaling factors for this general.
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

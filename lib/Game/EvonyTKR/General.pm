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
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::General
use overload
    '<=>' => \&_comparison,
    'cmp' => \&_comparison,
    'eq'  => \&_equality,
    '=='  => \&_equality,
    'ne'  => \&_inequality,
    '!='  => \&_inequality;

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

  field $stars :reader :param //= '5red';

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

  field $starsValues = enum [
    'None',
    '1purple',
    '2purple',
    '3purple',
    '4purple',
    '5purple',
    '1red',
    '2red',
    '3red',
    '4red',
    '5red',
  ];

  field %BasicAESAdjustment = (
    'None'    => 0,
    '1purple' => 0,
    '2purple' => 0,
    '3purple' => 0,
    '4purple' => 0,
    '5purple' => 0,
    '1red'    => 10,
    '2red'    => 20,
    '3red'    => 30,
    '4red'    => 40,
    '5red'    => 50,
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

    my $check = $starsValues->compiled_check;
    my $prettyenum = np @{ $starsValues->values }; 
    $check->($stars) or push @errors => "stars must be one of the $prettyenum not $stars";

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

  method effective_leadership() {
    my $AES_adjustment = 0;

    if($ascending){
      my @values = $starsValues->values();
      for my $value (@values){
        if($stars eq $value) {
          $AES_adjustment = $BasicAESAdjustment{$stars};
          last;
        }
      }
    }

    my $step1 = $level * $leadership_increment + $leadership;
    my $step2 = $step1 + 500;
    my $step3 = $step2 + $AES_adjustment;
    my $step4;
    if($step3 < 900) {
      $step4 = $step3 * 0.1;
    } else {
      $step4 = 90 + ($step3 - 900) * 0.2;
    }
    return $step4;
  }

  method effective_attack() {
    my $AES_adjustment = 0;

    if($ascending){
      my @values = $starsValues->values();
      for my $value (@values){
        if($stars eq $value) {
          $AES_adjustment = $BasicAESAdjustment{$stars};
          last;
        }
      }
    }

    my $step1 = $level * $attack_increment + $attack;
    my $step2 = $step1 + 500;
    my $step3 = $step2 + $AES_adjustment;
    my $step4;
    if($step3 < 900) {
      $step4 = $step3 * 0.1;
    } else {
      $step4 = 90 + ($step3 - 900) * 0.2;
    }
    return $step4;
  }

  method effective_defense() {
    my $AES_adjustment = 0;

    if($ascending){
      my @values = $starsValues->values();
      for my $value (@values){
        if($stars eq $value) {
          $AES_adjustment = $BasicAESAdjustment{$stars};
          last;
        }
      }
    }

    my $step1 = $level * $defense_increment + $defense;
    my $step2 = $step1 + 500;
    my $step3 = $step2 + $AES_adjustment;
    my $step4;
    if($step3 < 900) {
      $step4 = $step3 * 0.1;
    } else {
      $step4 = 90 + ($step3 - 900) * 0.2;
    }
    return $step4;
  }

  method effective_politics() {
    my $AES_adjustment = 0;

    if($ascending){
      my @values = $starsValues->values();
      for my $value (@values){
        if($stars eq $value) {
          $AES_adjustment = $BasicAESAdjustment{$stars};
          last;
        }
      }
    }

    my $step1 = $level * $politics_increment + $politics;
    my $step2 = $step1 + 500;
    my $step3 = $step2 + $AES_adjustment;
    my $step4;
    if($step3 < 900) {
      $step4 = $step3 * 0.1;
    } else {
      $step4 = 90 + ($step3 - 900) * 0.2;
    }
    return $step4;
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

=method stars()

This returns one of the values 
'None'
'1purple'
'2purple'
'3purple'
'4purple'
'5purple'
'1red'
'2red'
'3red'
'4red'
'5red'

to indicate at what level of ascension the general is being evaluated at. 
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

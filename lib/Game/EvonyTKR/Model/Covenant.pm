use v5.40.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require JSON::PP;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;


class Game::EvonyTKR::Model::Covenant : isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::Covenant
  use builtin         qw(indexed);
  require Data::Printer;
  require Readonly;
  use namespace::autoclean;
  use File::FindLib 'lib';
  use List::AllUtils qw( any none );
  use Carp;
  use overload
    '""'       => \&as_string,
    'bool'     => sub { $_[0]->_isTrue() },
    "fallback" => 1;

  my $debug = 1;

  field $primary : reader : param;

  field $secondary : reader;

  field @secondaryKeys : reader = qw( one two three );

  ADJUST {
    my $step1 = {};
    foreach my $key ( @secondaryKeys ) {
      $step1->{$key} = $primary;
    }
    # Use Readonly::Hash1 for shallow readonly - hash structure is fixed but array contents can change
    Readonly::Hash1 my %step2 => %{$step1};
    $secondary = \%step2;
  }

  field $categories : reader;

  field $CovenantLevels =
    enum ['None', 'War', 'Cooperation', 'Peace', 'Faith', 'Honor', 'Civilization',];

  ADJUST {
    #Covenants only have these levels.
    my @levels = @{ $CovenantLevels->values() };
    my $step1 = {};
    foreach my $key (@levels) {
      if ($key eq 'None'){
        next;
      }
      $step1->{$key} = {
        type => "personal",
        activationLevel => 0,
        buffs => []
      };
    }
    Readonly::Hash1 my %step2 => %{$step1};
    $categories = \%step2;

    for my ($index, $lv) (indexed(@levels)) {
      my $al = 10000 + $index * 2 * 1000;
      $self->logger()->trace("setting activationLevel for $lv to $al");
      $categories->{$lv} = {
        activationLevel => $al,
      };
    }
  }

  method setSecondary ($position, $general) {
    if (any { $_ =~ /$position/i } @secondaryKeys) {
      my $generalClass = blessed $general;
      if($generalClass ne 'Game::EvonyTKR::Model::General') {
        $self->logger->logcroak("$generalClass is not a Game::EvonyTKR::Model::General.");
      }
      $position = lc($position);
      $secondary->{$position} = $general;
    }
    else {
      $self->logger()
        ->logwarn("$position is not a valid position, must be one of "
          . np @secondaryKeys);
    }
  }

  method addBuff ($level, $nb) {
    my $red = 1;
    if (!blessed($nb) || blessed($nb) ne "Game::EvonyTKR::Model::Buff") {
      $self->logger->error(sprintf(
        'attempting to add buff of type %s not "Game::EvonyTKR::Model::Buff"',
        !blessed($nb) ? Scalar::Util::reftype($nb) : blessed($nb)));
      exit 0;
    }

    if (none { $level =~ /$_/i) @{ $CovenantLevels->values() } ) {
      $self->logger->error(sprintf(
        'level should be one of %s, not %s',
        join(', ' @{ $CovenantLevels->values() }),
        $level
      ));
      return 0;
    }
    if ($level eq 'None') {
      return 0;
    }

    push @{ $categories->{$level}->{buffs} }, $nb;
    return scalar @{ $ascending->{$level}->{buffs} };
  }

  method to_hash($verbose = 0) {
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

  method TO_JSON {
    return $self->to_hash();
  }

  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
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

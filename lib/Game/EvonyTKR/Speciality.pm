use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../lib";

class Game::EvonyTKR::Speciality : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Speciality
  use builtin         qw(indexed);
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common   qw( t);
  use Type::Utils     qw(is enum);
  use Carp;
  use Data::Dumper;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Game::EvonyTKR::Buff;
  use Game::EvonyTKR::Buff::Value;
  use List::MoreUtils;
  use Util::Any -all;
  use YAML::XS qw{LoadFile Load};
  use namespace::autoclean;
  use Game::EvonyTKR::Logger;
  use overload
    '""'       => \&_toString,
    "fallback" => 1;

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

  field %Buffs : reader;
  field $levels : reader =
    enum ['Green', 'None', 'Blue', 'Purple', 'Orange', 'Gold',];

  field $activeLevel : reader : param //= 'None';

  ADJUST {
    my @lv = @{ $levels->values() };
    for my $tl (@lv) {
      $Buffs{$tl} = ();
    }
    my @errors;
    my $t            = $levels->compiled_check();
    my $prettyLevels = np @{ $levels->values() };
    $t->($activeLevel)
      or push @errors =>
      "activeLevel must be one of $prettyLevels, not $activeLevel";
  }

  method setActiveLevel ($newLevel) {
    my $t = $levels->compiled_check();
    if ($t->($newLevel)) {
      $activeLevel = $newLevel;
    }
  }

  method add_buff($level, $nb, $inherited = 0) {
    if (blessed $nb ne 'Game::EvonyTKR::Buff') {
      return 0;
    }
    my $tcheck = $levels->compiled_check();
    if (none { $tcheck->($_) } ($level)) {
      return 0;
    }

    my @levelValues = @{ $levels->values() };
    for my $tl (@levelValues) {
      if ($level eq 'None') {
        # Level None never has any buffs, this was a mistake.
        last;
      }
      if ($tl eq 'None') {
        next;
      }
      if ($tl eq $level) {
        if ($inherited) {
          my $copy;
          if ($nb->has_buffClass()) {
            $copy = Game::EvonyTKR::Buff->new(
              attribute => $nb->attribute(),
              value     => $nb->value(),
              buffClass => $nb->buffClass(),
              inherited => 1,
            );
          }
          else {
            $copy = Game::EvonyTKR::Buff->new(
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
            ->trace("Adding inherited buff at level $tl" . np $copy);
          push @{ $Buffs{$tl} }, $copy;
        }
        else {
          $self->logger()
            ->trace("Adding uninherited buff at level $tl" . np $nb);
          push @{ $Buffs{$tl} }, $nb;
        }
        if ($tl eq 'Green') {
          $self->add_buff('Blue', $nb, 1);
        }
        elsif ($tl eq 'Blue') {
          $self->add_buff('Purple', $nb, 1);
        }
        elsif ($tl eq 'Purple') {
          $self->add_buff('Orange', $nb, 1);
        }
        elsif ($tl eq 'Orange') {
          $self->add_buff('Gold', $nb, 1);
        }
        last;
      }
    }
    return 1;
  }

  method getEvAnsScore($name, $resultRef, $BuffMultipliers, $GeneralBias) {
    my @ab = @{ $Buffs{$activeLevel} };
    my $bc = scalar @ab;
    $self->logger()
      ->debug("getEvAnsScore for $name found $bc buffs at $activeLevel");
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
      ->trace("Starting toHashRef for Speciality, verbose is $verbose");
    my $returnRef = {
      name        => $self->name(),
      activeLevel => $self->activeLevel(),
    };
    my @values = @{ $levels->values() };
    if ($verbose) {
      # I initially thought to test for duplicate buffs.  This fails because
      # Generals (see Dmitry) can *have* duplicate buffs.  Instead I created
      # the inherited field for Buffs.
      for my $key (sort keys %Buffs) {
        $self->logger()->trace("processing $key");
        for my $thisBuff (@{ $Buffs{$key} }) {
          if (not $thisBuff->inherited()) {
            $self->logger()
              ->trace("found unique buff for $key " . np $thisBuff);
            push @{ $returnRef->{$key} }, $thisBuff->toHashRef();
          }
          else {
            if ($thisBuff->inherited()) {
              $self->logger()
                ->trace("found inherited buff at $key " . np $thisBuff);
            }
          }
        }
      }
    }
    else {
      $self->logger()->debug("activeLevel is $activeLevel");
      $self->logger()->debug(
        exists $Buffs{$activeLevel}
        ? "'$activeLevel' is a valid key"
        : "'$activeLevel' is not a valid key"
      );
      for my $thisBuff (@{ $Buffs{$activeLevel} }) {
        push @{ $returnRef->{$activeLevel} }, $thisBuff->toHashRef();
      }
    }
    return $returnRef;
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method readFromFile() {
    my $SpecialityFileName = $name . '.yaml';
    $self->logger()->debug("about to get $SpecialityFileName");
    my $SpecialityShare =
      File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'specialities');
    my $FileWithPath =
      File::Spec->catfile($SpecialityShare, $SpecialityFileName);
    if (-T -s -r $FileWithPath) {
      $self->logger()->debug("$SpecialityFileName exists as expected");
      my $data       = LoadFile($FileWithPath);
      my @fileLevels = @{ $data->{'levels'} };
      foreach my $fl (@fileLevels) {
        my @flBuffs = @{ $fl->{'buff'} };
        foreach my $flb (@flBuffs) {
          my $v;
          my $b;
          my @flKeys = keys %{$flb};

          if (any { $_ eq 'value' } @flKeys) {
            $self->logger()
              ->debug("SpecialityFileName has a buff with a value");
            if (not exists $flb->{'attribute'}) {
              $self->logger()
                ->logcroak(
                "attribute is not defined in a buff for $SpecialityFileName");
            }
            $v = Game::EvonyTKR::Buff::Value->new(
              number => $flb->{'value'}->{'number'},
              unit   => $flb->{'value'}->{'unit'},
            );
            if (any { $_ eq 'class' } @flKeys) {

              $b = Game::EvonyTKR::Buff->new(
                attribute => $flb->{'attribute'},
                value     => $v,
                buffClass => $flb->{'class'},
              );
            }
            else {
              $b = Game::EvonyTKR::Buff->new(
                attribute => $flb->{'attribute'},
                value     => $v,
              );
            }
          }
          else {
            $self->logger()
              ->warn("SpecialityFileName has a buff without a value");
          }
          if (defined $b) {
            if (any { $_ eq 'condition' } @flKeys) {
              my @conditions = @{ $flb->{'condition'} };
              foreach my $flc (@conditions) {
                $b->set_condition($flc);
              }
            }

            $self->logger()
              ->info(
              "Adding buff from $SpecialityFileName to " . $fl->{'level'});
            $self->add_buff($fl->{'level'}, $b);
          }
          else {
            $self->logger()->warn('No buff defined');
          }
        }

      }
    }

  }
}

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR Specialities.

=head1 DESCRIPTION

Specialities are one of several ways that a General can provide Buffs for Troops.

Unlike SkillBooks, all Specialities are essentially with one slight exception to do two slight exceptions:

=for :List

* The 4th standard Speciality can only be activated if the first 3 standard Specialities are at Gold level.  When they do reach Gold level, this 4th Speciality automatically gets Green level. 

* Flex Specialities can only be added once a General has 4 active Specialities.  Flex specialities grow in distictingly different ways, and unlike other Specialities can be added and removed from a General in an almost SkillBook like way, but when present, are otherwise exactly like any other Speciality in how the Buffs are applied and when the Buffs are used.   Flex Specialities I<do> in fact need a sub class. 

=cut

=method name()

returns the name field from the Speciality. 
=cut

=method activeLevel

returns the level, 'Green', 'Blue', 'Purple', 'Orange', or 'Gold', that is active at this time. 
=cut

=method setActiveLevel($newLevel)

sets the activeLevel to newLevel presuming it is a valid value (one of 'Green', 'Blue', 'Purple', 'Orange', or 'Gold').
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Buff as its sole parameter and adds it as one of the buffs this Speciality at the specified $level.  $level must be one of 'Green', 'Blue', 'Purple', 'Orange', or 'Gold' or the function will fail to add the buff.  

It is because Flex specialities require greater granularity that they require a subclass. 

NOTE:  Because Specialities frequently have the same Buff at multiple levels, this method cannot protect against being called twice for the same Buff at this time.  It is up to the caller to use with care. 
=cut

=method Buffs()

Returns a hash with the levels 'Green', 'Blue', 'Purple', 'Orange', or 'Gold' as the keys and an array with the buffs at that level as the values. 

Each level is cumulative, you never need to read more than the array for the currently active level. 
=cut


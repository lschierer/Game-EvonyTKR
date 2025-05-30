use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';
require JSON::PP;

class Game::EvonyTKR::Model::Speciality : isa(Game::EvonyTKR::Model::Logger) {
  use builtin         qw(indexed);
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common   qw( t);
  use Type::Utils     qw(is enum);
  use Carp;
  require Data::Printer;
  require Game::EvonyTKR::Model::Buff;
  require Game::EvonyTKR::Model::Data;
  use List::MoreUtils;
  require Path::Tiny;
  use Util::Any -all;
  require YAML::PP;
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Model::Speciality
# VERSION
  use Game::EvonyTKR::Model::Logger;
  use overload
    '""'       => \&TO_JSON,
    "fallback" => 1;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $name : reader : param;

  field $EvonyData : reader = Game::EvonyTKR::Model::Data->new();

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $levels : reader;
  field $level_names : reader = $EvonyData->specialityLevels();

  field $activeLevel : reader : param //= 'None';

  ADJUST {
    my @lv = @{ $level_names->values() };
    for my $tl (@lv) {
      $levels->{$tl} = ();
    }
    my @errors;
    my $t            = $level_names->compiled_check();
    my $prettyLevels = Data::Printer::np @{ $level_names->values() };
    $t->($activeLevel)
      or push @errors =>
      "activeLevel must be one of $prettyLevels, not $activeLevel";
  }

  method setActiveLevel ($newLevel) {
    my $t = $level_names->compiled_check();
    if ($t->($newLevel)) {
      $activeLevel = $newLevel;
    }
    else {
      $self->logger()->warn(sprintf(
        '%s is not a valid level, options are %s',
        $newLevel, Data::Printer::np $level_names->values()
      ));
    }
  }

  method add_buff($level, $nb, $inherited = 0) {
    if (blessed $nb ne 'Game::EvonyTKR::Model::Buff') {
      return 0;
    }
    my $tcheck = $level_names->compiled_check();
    if (none { $tcheck->($_) } ($level)) {
      return 0;
    }

    my @levelValues = @{ $level_names->values() };
    for my $tl (@levelValues) {
      if ($level eq 'None') {
        # Level None never has any levels, this was a mistake.
        last;
      }
      if ($tl eq 'None') {
        next;
      }
      if ($tl eq $level) {
        if ($inherited) {
          my $copy;
          if ($nb->has_targetedType()) {
            $copy = Game::EvonyTKR::Model::Buff->new(
              attribute     => $nb->attribute,
              value         => $nb->value,
              targetedTypes => $nb->targetedTypes,
              inherited     => 1,
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
            ->trace(
            "Adding inherited buff at level $tl" . Data::Printer::np $copy);
          push @{ $levels->{$tl} }, $copy;
        }
        else {
          $self->logger()
            ->trace(
            "Adding uninherited buff at level $tl" . Data::Printer::np $nb);
          push @{ $levels->{$tl} }, $nb;
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
    my @ab = @{ $levels->{$activeLevel} };
    my $bc = scalar @ab;
    $self->logger()
      ->debug("getEvAnsScore for $name found $bc levels at $activeLevel");
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
              . Data::Printer::np $thisBuff);
          $category = 'Unused';
        }
        else {
          $self->logger()
            ->debug("getEvAnsScore for $name found category $category for "
              . Data::Printer::np $thisBuff);
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

  method toHashRef {
    return {
      id     => $name,
      name   => $name,
      levels => $levels,
    };
  }

  method TO_JSON {
    return $self->toHashRef();
  }

  field $ypp = YAML::PP->new(
    schema       => [qw/ + Perl /],
    yaml_version => ['1.2', '1.1'],
  );

  method readFromFile($dist_dir = '') {
    my $SpecialityFileName = $name . '.yaml';
    $self->logger()->debug("about to get $SpecialityFileName");
    my $SpecialityShare =
      Path::Tiny::path($dist_dir)->child("collections/specialities");
    my $FileWithPath = $SpecialityShare->child($SpecialityFileName);
    if ($FileWithPath->is_file()) {
      $self->logger()->debug("$SpecialityFileName exists as expected");
      my $data       = $ypp->load_file($FileWithPath);
      my @fileLevels = @{ $data->{'levels'} };
      foreach my $fl (@fileLevels) {
        my @fllevels = @{ $fl->{'buff'} };
        foreach my $flb (@fllevels) {
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
            $v = Game::EvonyTKR::Model::Buff::Value->new(
              number => $flb->{'value'}->{'number'},
              unit   => $flb->{'value'}->{'unit'},
            );
            if (any { $_ eq 'class' } @flKeys) {

              my @targetedTypes = ();
              if (ref $flb->{'class'} eq 'ARRAY') {
                @targetedTypes = $flb->{'class'};
              }
              else {
                push @targetedTypes, $flb->{'class'};
              }
              $b = Game::EvonyTKR::Model::Buff->new(
                attribute     => $flb->{'attribute'},
                value         => $v,
                targetedTypes => \@targetedTypes,
              );
            }
            else {
              $b = Game::EvonyTKR::Model::Buff->new(
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
                $self->logger()->trace("adding condition $flc");
                $b->set_condition($flc);
              }
            }

            $self->logger()
              ->info(
              "Adding buff from $SpecialityFileName to " . $fl->{'level'});
            $self->add_buff(lc($fl->{'level'}), $b);
            $self->logger()->info("levels now " . Data::Printer::np($levels));
          }
          else {
            $self->logger()->warn('No buff defined');
          }
        }

      }
    }
    else {
      $self->logger()
        ->warn("failed to find $SpecialityFileName at $FileWithPath");
    }

  }
}

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR Specialities.

=head1 DESCRIPTION

Specialities are one of several ways that a General can provide levels for Troops.

Unlike SkillBooks, all Specialities are essentially with one slight exception to do two slight exceptions:

=for :List

* The 4th standard Speciality can only be activated if the first 3 standard Specialities are at Gold level.  When they do reach Gold level, this 4th Speciality automatically gets Green level.

* Flex Specialities can only be added once a General has 4 active Specialities.  Flex specialities grow in distictingly different ways, and unlike other Specialities can be added and removed from a General in an almost SkillBook like way, but when present, are otherwise exactly like any other Speciality in how the levels are applied and when the levels are used.   Flex Specialities I<do> in fact need a sub class.

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

This method takes a Game::EvonyTKR::Model::Buff as its sole parameter and adds it as one of the levels this Speciality at the specified $level.  $level must be one of 'Green', 'Blue', 'Purple', 'Orange', or 'Gold' or the function will fail to add the buff.

It is because Flex specialities require greater granularity that they require a subclass.

NOTE:  Because Specialities frequently have the same Buff at multiple levels, this method cannot protect against being called twice for the same Buff at this time.  It is up to the caller to use with care.
=cut

=method levels()

Returns a hash with the levels 'Green', 'Blue', 'Purple', 'Orange', or 'Gold' as the keys and an array with the levels at that level as the values.

Each level is cumulative, you never need to read more than the array for the currently active level.
=cut

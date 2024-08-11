use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Ascending :isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Ascending

  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common qw( t);
  use Type::Utils qw(is enum);
  use Carp;
  use Data::Dumper;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Game::EvonyTKR::Buff;
  use Game::EvonyTKR::Buff::Value;
  use JSON::MaybeXS;
  use List::MoreUtils;
  use Util::Any -all;
  use YAML::XS qw{LoadFile Load};
  use namespace::autoclean;
  use Game::EvonyTKR::Logger;
  use overload
    '""'        => \&_toString,
    "fallback"  => 1;

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

 
  field %Buffs :reader;
  field $levels = enum [
    'None',
    '1Purple',
    '2Purple',
    '3Purple',
    '4Purple',
    '5Purple',
    '1Red',
    '2Red',
    '3Red',
    '4Red',
    '5Red',
  ];

  field $activeLevel :reader :param //= 'None';

  ADJUST {
    my @lv = $levels->values();
    for my $tl (@lv){
      $Buffs{$tl} = ();
    }
    my @errors;
    my $t = $levels->compiled_check();
    my $prettyLevels = np @{ $levels->values() };
    $t->($activeLevel) or push @errors => "activeLevel must be one of $prettyLevels, not $activeLevel";
  }

  method setActiveLevel ($newLevel) {
    my $t = $levels->compiled_check();
    if($t->($newLevel)){
      $activeLevel = $newLevel;
    }
  }

  method add_buff($level, $nb) {
    if(blessed $nb ne 'Game::EvonyTKR::Buff'){
      return 0;
    } 
    my $t = $levels->compiled_check();
    if(not $t->($level)) {
      return 0;
    }
    my @levelValues = $levels->values();

    for my $tl (@levelValues) {
      if($level eq 'None'){
        # Level None never has any buffs, this was a mistake. 
        last;
      }
      if($tl eq 'None'){
        next;
      }
      if($tl eq $level) {
        push @{$Buffs{$tl} }, $nb;
        if ($tl eq '1Purple') {
          $self->add_buff('2Purple', $nb);
        } elsif ($tl eq '2Purple') {
          $self->add_buff('3Purple', $nb);
        } elsif ($tl eq '3Purple') {
          $self->add_buff('4Purple', $nb);
        } elsif ($tl eq '4Purple') {
          $self->add_buff('5Purple', $nb);
        } elsif ($tl eq '1Red') {
          $self->add_buff('2Red', $nb);
        } elsif ($tl eq '2Red') {
          $self->add_buff('3Red', $nb);
        } elsif ($tl eq '3Red') {
          $self->add_buff('4Red', $nb);
        } elsif ($tl eq '4Red') {
          $self->add_buff('5Red', $nb);
        }
        last;
      }
    }
    return 1;
  }

  method toHashRef( $verbose = 0) {
    my $returnRef = {};
    my @values = @{ $levels->values()};
    if($verbose) {
      for my $key ( keys %Buffs) {
        $returnRef->{$key} = $Buffs{$key};
      }
    } else {
      $returnRef->{$activeLevel} = $Buffs{$activeLevel};
    }
    return $returnRef;
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method readFromFile($name) {
    my $AscendingFileName = $name . '.yaml';
    $self->logger()->debug("about to get $AscendingFileName");
    my $AscendingShare = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'ascending');
    my $FileWithPath = File::Spec->catfile($AscendingShare, $AscendingFileName);
    if( -T -s -r $FileWithPath ) {
      $self->logger()->debug("$AscendingFileName exists as expected");
      my $data = LoadFile($FileWithPath);
      my @fileLevels = @{ $data->{'ascending'}};
      foreach my $fl (@fileLevels){
        my @flBuffs = @{ $fl->{'buff'} };
        foreach my $flb (@flBuffs) {
          my $v;
          my $b;
          my @flKeys = keys %{$flb};
          
          if(any {$_ eq 'value'} @flKeys) {
            $self->logger()->debug("AscendingFileName has a buff with a value");
            $v = Game::EvonyTKR::Buff::Value->new(
              number  => $flb->{'value'}->{'number'},
              unit    => $flb->{'value'}->{'unit'},
            );
            if(any {$_ eq 'class'} @flKeys) {
              $b = Game::EvonyTKR::Buff->new(
                attribute  => $flb->{'attribute'},
                value      => $v,
                buffClass   => $flb->{'class'},
              );
            } else {
              $b = Game::EvonyTKR::Buff->new(
                attribute  => $flb->{'attribute'},
                value      => $v,
              );
            }
          }else {
            $self->logger()->warn("AscendingFileName has a buff without a value");
          }
          if(defined $b) {
            if(any {$_ eq 'condition'} @flKeys) {
              my @conditions = @{ $flb->{'condition'} };
              foreach my $flc (@conditions){
                $b->set_condition($flc);
              }
            }
            
            $self->logger()->info("Adding buff from $AscendingFileName to " . $fl->{'level'});
            $self->add_buff($fl->{'level'}, $b);
          }else {
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

Ascending is one of several ways that a General can provide Buffs for Troops.

Ascending works similarly to Specialities in that there are multiple levels, however there is only one set of Ascending Buffs per general, instead of the four possible Specialities (plus possible Flex Speciality).   

=for :List

* Ascending Buffs can be either values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

* Ascending Buffs 1Red through 5Red provide ehancements to Basic skills and affect the General's effective scores for the basic attributes. 

* Ascending a Historic (Gold/Red) General costs both general fragments and Blood of Ares.  Ascending a General that is merely Historic but not Legendary (Purple) requires general fragments, but not Blood of Ares. 

* For low spenders, Blood of Ares is a severe limiting factor on ascending generals.  As you grow in spending, it limits the I<rate> at which you ascend, but not I<if> you ascend a general.  Fragments are the true limiting concern for players of all levels.  It is not possible to get sufficient fragments of all generals to fully ascend them without spending in any reasonable amount of time.  It is not possible to ascend certain "retired" generals I<at all> if you have not already done so.  These "retired" Generals are included in the distribution data to allow players who I<do> have them to compare effectiveness in using them. 
=cut

=method name()

returns the name field from the Speciality. 
=cut

=method activeLevel

returns the level, values 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals), that is active at this time. 
=cut

=method setActiveLevel($newLevel)

sets the activeLevel to newLevel presuming it is a valid value 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals).

Todo: Make sure the value is valid I<for the General's quality>. 
=cut

=method add_buff($level, $nb)

This method takes a Game::EvonyTKR::Buff as its sole parameter and adds it as one of the buffs this Speciality at the specified $level.  $level must be one of 1Purple through 5Purple (for Purple quality generals), or 1Red through 5Red (for Gold/Red quality generals) or the function will fail to add the buff.  

Todo: Make sure that this is not called twice with the same Buff/Level combination.  Make sure the Level provided is valid I<for that General's quality>. 
=cut

=method Buffs()

Returns a hash with the levels 1Purple through 5Purple, and 1Red through 5Red as the keys and an array with the buffs at that level as the values.  Note all 10 levels are always present, but only 5 will have Buffs assigned.  The other 5 will return empty lists. 

Each level is cumulative, you never need to read more than the array for the currently active level. 
=cut

use v5.40.0;
use experimental qw(class);
use utf8::all;
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

class Game::EvonyTKR::General::Pair::Creator :isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::General::Pair::Creator
  use Carp;
  use Clone 'clone';
  use Class::ISA;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
  use Util::Any -all;
  use File::ShareDir ':ALL';
  use File::HomeDir;
  use File::Spec;
  use File::Path qw(make_path);
  use DBM::Deep;
  use Data::Printer;
  use Hash::Map;
  use YAML::PP::LibYAML;
  use List::MoreUtils;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::SkillBook::Standard;
  use Game::EvonyTKR::Buff::Data;
  use Game::EvonyTKR::General::Conflicts;
  use Game::EvonyTKR::General::Pair;
  use namespace::autoclean;
  use Game::EvonyTKR::Logger;


  my $debug = 1;

  my $distData = File::HomeDir->my_dist_data( 'Game-Evony', { create => 1 } );
  my $dbPath = File::Spec->catfile($distData, "db");

  # some constants
  field $classData = Game::EvonyTKR::Buff::Data->new();
  field $conflicData = Game::EvonyTKR::General::Conflicts->new();

  field %generals :reader;

  method set_generals( %ng ) {

    if(scalar %ng >= 1) {
      $self->logger()->debug("hashRef is " . \%ng);
      $self->logger()->debug("size is " . scalar %ng);

       %generals = %ng;
    }
  }

=method getPairs()

this method presupposes that set_generals has already been called. It returns
an array of Generals that do not conflict.
If there are insufficient generals to make pairs, it returns 0 (false).

This assumes that the Generals are hashed by their names.

if conflicts have not been intialized in the Game::EvonyTKR::General::Conflicts class, 
it will not consider conflicts. 
=cut
  method getPairs() {
    if(scalar %generals >= 2 ) {
      my %pairs;
      my $sg1 = Hash::Map->new();
      $sg1->set_source(%generals);

      my $sg2 = Hash::Map->new();
      $sg2->set_source(%generals);
      
      while ( my ($key1, $value1) = $sg1->each_source ) {
        $self->logger()->info( "looking for pairs for $key1");
        my @conflicts = $conflicData->getConflictsByName($value1->name());
        while ( my ($key2, $value2) = $sg2->each_source ) {
          $self->logger()->debug("testing $key2 against $key1");

          if($value1->name() ne $value2->name()) {
            
            if(scalar @conflicts >= 1) {
              my $name2 = $value2->name();
              if( any { $_ =~ qr/$name2/} @conflicts) {
                $self->logger()->info("found conflict between " . $value1->name() . " and " . $value2->name());
                next;
              }
            }
            my $key = 'unknown';
            my @BuffClasses = $classData->BuffClasses();
            if($value1->is_ground_general() and $value2->is_ground_general()){
              $key = first { $_ =~ qr/ground/i } @BuffClasses;
            } elsif ($value1->is_mounted_general() and $value2->is_mounted_general()) {
              $key = first {$_ =~ qr/mounted/i } @BuffClasses;
            } elsif ($value1->is_ranged_general() and $value2->is_ranged_general()) {
              $key = first {$_ =~ qr/ranged/i } @BuffClasses;
            } elsif($value1->is_siege_general() and $value2->is_siege_general()){
               $key = first {$_ =~ qr/siege/i } @BuffClasses;
            }
            my $pair = Game::EvonyTKR::General::Pair->new(
                primary   => $value1,
                secondary => $value2,
              );
            if($key ne 'unknown') {
              if(defined $key && $key ne '') {
                $self->logger()->debug("$key1 and $key2 are both $key");
                push @{ $pairs{$key} }, $pair;
              } else {
                croak "bad key '$key'"
              }
            }
          } else {
            $self->logger()->debug("$key1 and $key2 are the same, rejecting");
          }
        }
        
      }
      return %pairs;
    }
    croak "Cannot operate without generals."; 
  }

};
1;
__END__


# ABSTRACT: Read in shared YAML data to create pairs

=head1 DESCRIPTION

takes a hash of Game::EvonyTKR::General class objects and creates Game::EvonyTKR::General::Pair class objects.

=head2 Conflict Database

This creates a database of conflict information for the overall distribution.  In doing so, I faced several constraints.

=for :list

* L<A general might conflict with a SkillBook::Standard level 1 but not a SkillBook::Standard level 4 of the same name>

* L<While in general my ratings are based on the excelent work done by EvonyAnswers, as new Generals come out, the contents of each conflict group frequently change.>

I do not want to have to deal with finding and moving generals within the existing database from one group to another when a new general causes all the groups to shift around.  It is actually much easier to maintain effectively a unique group per general, *except* when you have to update that many yaml files by hand just to add one name to the conflicts for each general.  It is then that the groups are convient.  But in practice, whenever I've tried to *parse* the groups in code, it gets really inefficient really fast, because I have to iterate all groups to find which one contains the general I want as a primary member, then compute his/her conflicts from there.

This way, I can simply look up the general's conflicts with the chained ->get() operators on the database.  Its incredibly efficient from an information retrivial standpoint, which is what the library needs to do most of the time.  This creator class thus attempts to bridge the gap.  I can maintain yaml files by EvonyAnswers groups, but will create the database hashed by individual general for processing.

This does mean when EvonyAnswers inevitiably rejiggers the groups, I still have to shift all those yaml files around, so a future optimization will be to change hte "others" section to a series of references to other groups. This will reduce the number of edits hugely.

=cut

=attr generals

a hash of the generals to pair, using the name of the generals as the key.
=cut

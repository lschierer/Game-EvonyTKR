use v5.40.0;
use experimental qw(class);
use utf8::all;

class Game::EvonyTKR::General::Pair::Creator {
  use Carp;
  use Clone 'clone';
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use File::ShareDir ':ALL';
  use File::HomeDir;
  use File::Spec;
  use File::Path qw(make_path);
  use DBM::Deep; 
  use Data::Dumper;
  use YAML::XS qw{LoadFile Load};
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::SkillBook::Standard;
  use Game::EvonyTKR::Buff::EvaluationMultipliers;
  use namespace::autoclean;

  my $debug = 1;  

  my $distData = File::HomeDir->my_dist_data( 'Game-Evony', { create => 1 } );
  my $dbPath = File::Spec->catfile($distData, "db");
  ADJUST {
    if(! -r -w  -x -o -d $dbPath) {
      make_path($dbPath,"0770");
    }
  }
  my $db;
  my $dbFile = File::Spec->catfile($dbPath, 'pairs.db');
  ADJUST {
    $db = DBM::Deep->new(
    file      => $dbFile,
    locking   => 1,
    autoflush => 1,
    );
  }


  field %generals :reader; 

  method set_generals( %ng ) {
    
    if(scalar %ng >= 1) {
      if($debug) {
        say "hashRef is " . \%ng;
        say "size is " . scalar %ng;
      }
       %generals = %ng;
    }
  }
  
  method getConflictData() {
    
    my $data_location = File::Spec->catfile(
        dist_dir('Game-EvonyTKR'), 
        'generalConflictGroups'
      );
    while (my $file = glob(File::Spec->catfile($data_location,'*.yaml'))) {
      if ($debug) {
        say $file;
      }
      my $conflictGroup = LoadFile($file);
      if($debug) {
        say 'start of ' . $file;
        say Dumper($conflictGroup);
      }
      if($debug) {
        say Dumper($conflictGroup->{'members'});
      }
      foreach (@{$conflictGroup->{'members'}}) {
        my $entryName = $_;
        $db->put($entryName, {}) unless $db->get($entryName);
        $db->get($entryName)->put('conflicts', [
          grep (!/$entryName/, (@{$conflictGroup->{'members'}}, @{$conflictGroup->{'others'}}))
        ]);
        if($debug) {
          say "conflicts are: ";
          say Dumper( $db->get($entryName)->get('conflicts'));
        }
        foreach (@{$conflictGroup-{'books'}}) {
          my $entryRef = $_;
          
          db->get($entryName)->put('conflictingBooks', []) unless db->get($entryName)->get('conflictingBooks');
          my $sb = Game::EvonyTKR::SkillBook::Standard->new(
            name  => $entryRef->{'book1'}->{'name'},
            level => $entryRef->{'book1'}->{'level'},
          );
          unless(){
            push @{db->get($entryName)->get('conflictingBooks')}, $sb ;
          }
          
        }
      }
      if($debug) {
        say "end of $file";
      }
    }
  }

};
1;
__END__

# PODNAME: Game::EvonyTKR::General::Pair
# ABSTRACT: Manage Game::EvonyTKR::Generals as Pairs

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

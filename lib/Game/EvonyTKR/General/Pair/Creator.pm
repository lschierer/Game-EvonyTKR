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
  use Game::EvonyTKR::SkillBook::Special;
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
    }
  }

};
1;
__END__

# PODNAME: Game::EvonyTKR::General::Pair
# ABSTRACT: Manage Game::EvonyTKR::Generals as Pairs

=head1 DESCRIPTION

takes a hash of Game::EvonyTKR::General class objects and creates Game::EvonyTKR::General::Pair class objects. 

=cut

=attr generals

a hash of the generals to pair, using the name of the generals as the key. 
=cut

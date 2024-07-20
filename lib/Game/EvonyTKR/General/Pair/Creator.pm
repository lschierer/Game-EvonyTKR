use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::General::Pair {
  use Carp;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is"; 
  use File::ShareDir ':ALL';
  use File::HomeDir;
  use File::Spec;
  use File::Path qw(make_path);
  use BerkeleyDB;  
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Buff::EvaluationMultipliers;
  use namespace::autoclean;
  # PODNAME: Game::EvonyTKR::General::Pair
  # ABSTRACT: Manage Game::EvonyTKR::Generals as Pairs

=head1 DESCRIPTION

takes a hash of Game::EvonyTKR::General class objects and creates Game::EvonyTKR::General::Pair class objects. 

=cut

  my $distData = File::HomeDir->my_dist_data( 'Game-Evony', { create => 1 } );
  my $dbPath = File::Spec->catfile($distData, "db");
  ADJUST {
    if(! -r -w  -x -o -d $dbPath) {
      make_path($dbPath,"0770");
    }
  }
  my $bdbEnv = new BerkeleyDB::Env
    -Home   => $dbPath
    -Flags  => DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_CDB | DB_INIT_TXN | DB_THREAD ;

  my $conflictDb = new BerkeleyDB::Hash
    -Filename => "GeneralConflicts.db"
    -Env      => $bdbEnv;

=attr generals

a hash of the generals to pair, using the name of the generals as the key. 
=cut
  field %general :param :reader; 
  
  sub getConflictData() {
    my $data_location = dist_file('Game-EvonyTKR', 'generalConflictGroups');

  }

};
1

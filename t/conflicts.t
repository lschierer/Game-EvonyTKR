use v5.42.0;
use experimental qw(class);
use utf8::all;
use Mojo::File;
require Data::Printer;

require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::External::Common;
use Test2::V0;
use List::AllUtils qw( any none uniq );
use Sereal::Encoder;
use Sereal::Decoder;
use Carp;

# Setup logger
my $logger = Game::EvonyTKR::Log::Config->logger();

require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Model::General::Conflict::Book;

my $importTool = Game::EvonyTKR::External::Common->new(app => undef);


my $conflicts = Game::EvonyTKR::Model::General::Conflict->new(
build_index      => 1,
asst_has_dragon  => 1,
asst_has_spirit  => 1,
allow_wall_buffs => 1,
);



sub are_generals_compatible_either_role ($g1, $g2) {
  my $old = $conflicts->assume_g1_is_main;

  $conflicts->set_assume_g1_is_main(1);
  my $a = $conflicts->are_generals_compatible($g1, $g2);

  $conflicts->set_assume_g1_is_main(0);
  my $b = $conflicts->are_generals_compatible($g1, $g2);

  $conflicts->set_assume_g1_is_main($old);
  return $a || $b;
}

{
  package TestBase::WithRoles;
  use Mojo::Base -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Cache', -role;
  use Carp;

  # Add minimal required attributes
  has 'app';
}

{
  package TestClass::Generals;
  use Mojo::Base 'TestBase::WithRoles';
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Carp;

  # Mock only what's needed
  sub render { }
  sub stash { }
}


########################## Start Test Stuff

# Locate data
my $dist_dir = Mojo::File->new('./share');
my $collection_dir = $dist_dir->child("collections/data");


my $TestGenerals = TestClass::Generals->new();

my $memcachClient = $TestGenerals->create_general_cache();
if(not defined($memcachClient)){
  $logger->logcroak('failed to define memcachClient');
}

# Manager setup
$logger->info("Importing generals from $collection_dir");

my $generals = {};

my @yaml_files = ($collection_dir->child('generals'))->list->grep(sub {
  if($_ =~ m/\.ya?ml$/){
    return 1;
  }
  return 0;
})->sort(sub {"$a" cmp "$b"})->each;

# Check if memcached is available
my $test_key = 'memcached_test_' . time();
unless ($memcachClient->set($test_key, 'test')) {
  $logger->info('Memcached not available, skipping memcached tests');
  exit 1;
}
subtest 'Import All Generals' => sub {
  foreach my $index (0..$#yaml_files) {
    my $yf = $yaml_files[$index];
    my $general = $importTool->load_single_general($yf,$index);
    if($general) {
      ok($general->isa('Game::EvonyTKR::Model::General'), sprintf('%s imported as a ::Model::General', $general->name));
      ok(blessed($general->builtInBook()) && $general->builtInBook->isa('Game::EvonyTKR::Model::Book'), sprintf('%s has a ::Model::Book for a built in book', $general->name));
      ok(blessed($general->builtInBook->buffs->[0]) && $general->builtInBook->buffs->[0]->isa('Game::EvonyTKR::Model::Buff'), sprintf('%s has a ::Model::Buff as the first buff', $general->builtInBook->name));
      $generals->{$general->normalize($general->name)} = $general;
      $TestGenerals->add_general($general->normalize($general->name), $general, $memcachClient);
    } else {
      $logger->logcroak("could not import general from $yf");
    }
  }
};


isa_ok($conflicts, ['Game::EvonyTKR::Model::General::Conflict'], "conflicts is a ::General::Conflict object");


subtest 'All Generals have types to test against' => sub {
  for my $g (sort { $a->name cmp $b->name } values %{ $TestGenerals->get_generals( $memcachClient) }) {
    isa_ok($g, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', $g->name));
    my $types = $g->type // [];
    ok(@$types, sprintf('%s has at least one type', $g->name));
    ok(defined($g->builtInBook()), sprintf('%s has a defined built in book', $g->name));
    ok(blessed($g->builtInBook()) && $g->builtInBook->isa('Game::EvonyTKR::Model::Book'), sprintf('%s has a ::Model::Book', $g->name));
    ok(blessed($g->builtInBook->buffs->[0]) && $g->builtInBook->buffs->[0]->isa('Game::EvonyTKR::Model::Buff'), sprintf('first buff in book for %s is a ::Model::Buff', $g->name));
    ok($g->builtInBook->buffs->[0]->passive == 0 || $g->builtInBook->buffs->[0] == 1, sprintf('able to test passive on first buff in book for %s', $g->name));
  }
};

my $WASH        = $TestGenerals->get_general( 'Washington Prime', $memcachClient);
my $MARCO       = $TestGenerals->get_general( 'Marco Polo', $memcachClient);
my $AETHEL      = $TestGenerals->get_general( 'Aethelflaed', $memcachClient);
my $CAESAR      = $TestGenerals->get_general( 'Caesar', $memcachClient);
my $DF          = $TestGenerals->get_general( 'David Farragut', $memcachClient);
my $SC          = $TestGenerals->get_general( 'Sun Ce', $memcachClient);
my $GO          = $TestGenerals->get_general( 'Gaius Octavius', $memcachClient);
my $Hermes      = $TestGenerals->get_general( 'Hermes', $memcachClient);
my $Haakon      = $TestGenerals->get_general( 'Haakon Haraldsson', $memcachClient);
my $Barbarossa  = $TestGenerals->get_general( 'Barbarossa', $memcachClient);
my $Custer      = $TestGenerals->get_general( 'George A Custer', $memcachClient);
my $KA          = $TestGenerals->get_general( 'King Arthur', $memcachClient);
my $Jayavarman  = $TestGenerals->get_general( 'Jayavarman II', $memcachClient);
my $Cheng       = $TestGenerals->get_general( 'Cheng Yaojin', $memcachClient);
my $Laudon      = $TestGenerals->get_general( 'Laudon', $memcachClient);
my $Elektra     = $TestGenerals->get_general( 'Elektra', $memcachClient);
my $Franz       = $TestGenerals->get_general( 'Franz Joseph I', $memcachClient);
my $Douglas     = $TestGenerals->get_general( 'Douglas', $memcachClient);
my $Marcus      = $TestGenerals->get_general( 'Marcus Agrippa', $memcachClient);
my $Louis       = $TestGenerals->get_general( 'Louis IX', $memcachClient);
my $LouisXIV    = $TestGenerals->get_general( 'Louis XIV', $memcachClient);
my $OlavII      = $TestGenerals->get_general( 'Olav II', $memcachClient);

subtest 'Ensure Generals are Pressent for further tests' => sub {

  isa_ok($WASH, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Washington Prime'));
  isa_ok($MARCO, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Marco Polo'));
  isa_ok($AETHEL, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Aethelflaed'));
  isa_ok($CAESAR, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Caesar'));
  isa_ok($DF, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'David Farragut'));
  isa_ok($SC, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Sun Ce'));
  isa_ok($GO, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Gaius Octavius'));
  isa_ok($Hermes, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Hermes'));
  isa_ok($Haakon, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Haakon Haraldsson'));
  isa_ok($Barbarossa, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Barbarossa'));
  isa_ok($Custer, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'George A. Custer'));
  isa_ok($KA, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'King Arthur'));
  isa_ok($Jayavarman, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Jayavarman II'));
  isa_ok($Cheng, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Cheng Yaojin'));
  isa_ok($Laudon, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Laudon'));
  isa_ok($Elektra, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Elektra'));
  isa_ok($Franz, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Franz Joseph I'));
  isa_ok($Douglas, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Douglas'));
  isa_ok($Marcus, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Marcus Agrippa'));
  isa_ok($Louis, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Louis IX'));
  isa_ok($LouisXIV, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Louis XIV'));
  isa_ok($OlavII, ['Game::EvonyTKR::Model::General'], sprintf('%s is a ::Model::General', 'Olav II'));

  done_testing();
};

$conflicts->set_asst_has_dragon(1);
$conflicts->set_asst_has_spirit(1);

subtest 'Conflicting Generals' => sub {
ok(!are_generals_compatible_either_role($AETHEL,$CAESAR), 'Aethelflaed/Ceasar conflict');
ok(!are_generals_compatible_either_role($Douglas,$Marcus), 'Douglas/Marcus Agrippa conflict');
ok(!are_generals_compatible_either_role($Elektra,$Franz), 'Elektra/Franz Joseph I conflict');
ok(!are_generals_compatible_either_role($Hermes, $WASH), 'Hermes/Washington Prime conflict');
ok(!are_generals_compatible_either_role($Hermes,$Barbarossa), 'Hermes/Barbarossa conflict');
ok(!are_generals_compatible_either_role($Hermes,$Haakon), 'Hermes/Haakon conflict');
ok(!are_generals_compatible_either_role($Hermes,$KA), 'Hermes/King Arthur conflict');
ok(!are_generals_compatible_either_role($Laudon,$Cheng), 'Laudon/Cheng Yaojin conflict');
ok(!are_generals_compatible_either_role($MARCO,$GO), 'Marco/Gaius Octavius confict');
ok(!are_generals_compatible_either_role($MARCO,$Jayavarman), 'Marco Polo/Jayavarman II conflict');
ok(!are_generals_compatible_either_role($WASH, $Barbarossa), 'Washington Prime/Barbarossa conflict');
ok(!are_generals_compatible_either_role($WASH, $Custer), 'Washington Prime/George A. Custer conflict');
ok(!are_generals_compatible_either_role($WASH, $KA), 'Washington Prime/King Arthur conflict');
ok(!are_generals_compatible_either_role($Louis, $Franz), 'Louis IX/Franz Joseph I conflict');
ok(!are_generals_compatible_either_role($Louis, $Elektra), 'Louis IX/Elektra conflict');
ok(!are_generals_compatible_either_role($Jayavarman, $GO), 'Jayavarman II/Gaius Octavius conflict');
done_testing();
};


subtest 'Working Pairs' => sub {
ok(are_generals_compatible_either_role($Cheng,$Haakon), 'Cheng Yaojin/Haakon work');
ok(are_generals_compatible_either_role($Douglas,$Franz), 'Douglas/Franz Joseph I work');
ok(are_generals_compatible_either_role($Elektra,$Douglas), 'Elektra/Douglas work');
ok(are_generals_compatible_either_role($Elektra,$Marcus), 'Elektra/Marcus Agrippa work');
ok(are_generals_compatible_either_role($Laudon,$Haakon), 'Laudon/Haakon work');
ok(are_generals_compatible_either_role($MARCO, $WASH, ), 'Marco Polo/Washington Prime work');
ok(are_generals_compatible_either_role($MARCO,$DF), 'Marco Polo/David Farragut work (no overlaping types)');
ok(are_generals_compatible_either_role($MARCO,$Haakon), 'Marco Polo/Haakon work');
ok(are_generals_compatible_either_role($MARCO,$SC), 'Marco/Sun Ce works');
ok(are_generals_compatible_either_role($Marcus,$Franz), 'Marcus Agrippa/Franz Joseph I work');
ok(are_generals_compatible_either_role($Louis, $Marcus), 'Louis IX/Marcus Agrippa work');
ok(are_generals_compatible_either_role($Louis, $KA), 'Louis IX/King Arthur work');
ok(are_generals_compatible_either_role($Louis, $Douglas), 'Louis IX/Douglas work');
done_testing();
};

subtest 'Louis XIV Stackable Buffs Test' => sub {
ok(are_generals_compatible_either_role($LouisXIV, $OlavII), 'Louis XIV/Olav II should work (stackable)');
ok(are_generals_compatible_either_role($LouisXIV, $MARCO), 'Louis XIV/Marco Polo should work (stackable)');
done_testing();
};

my $bc = Game::EvonyTKR::Model::General::Conflict::Book->new(
build_index      => 1,
asst_has_dragon  => 1,
asst_has_spirit  => 1,
allow_wall_buffs => 1,
);

#my $l4ra = $RootManager->bookManager->getBook('Level 4 Ranged Troop Attack');
#my $l4ms = $RootManager->bookManager->getBook('Level 4 March Size');

subtest 'Books ready for testing' => sub {
#isa_ok($l4ra, ['Game::EvonyTKR::Model::Book'], sprintf('%s is a ::Model::Book', 'Level 4 Ranged Troop Attack'));
#isa_ok($l4ms, ['Game::EvonyTKR::Model::Book'], sprintf('%s is a ::Model::Book', 'Level 4 March Size'));

done_testing();
};

subtest 'Partial Conflicts with Books' => sub {
#  ok($bc->is_general_and_book_compatible($Elektra, $l4ra, { same_side => 1,}), 'Elektra and L4 Ranged Troop Attack work (same side)');
#  ok(!$bc->is_general_and_book_compatible($Elektra, $l4ra, { same_side => 0,}), 'Elektra and L4 Ranged Troop Attack conflict (other side)');
#  ok($bc->is_general_and_book_compatible($Custer, $l4ms, { same_side => 1,}), 'George A. Custer and L4 March Size work (same side)');
#  ok(!$bc->is_general_and_book_compatible($Custer, $l4ms, { same_side => 0,}), 'George A. Custer and L4 March Size conflict (other side)');
  done_testing();
};

subtest 'Full Conflicts with Books' => sub {
#  ok(!$bc->is_general_and_book_compatible($KA, $l4ms, { same_side => 1,}), 'King Arthur and Level 4 March Size conflict (same side)');
#  ok(!$bc->is_general_and_book_compatible($KA, $l4ms, { same_side => 0,}), 'King Arthur and Level 4 March Size conflict (other side)');
  done_testing();
};

done_testing();

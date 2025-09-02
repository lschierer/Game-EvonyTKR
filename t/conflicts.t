use v5.42.0;
use experimental qw(class);
use utf8::all;
require Path::Tiny;
require Data::Printer;

require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::EvonyTKR::Manager;
use Test2::V0;
use List::AllUtils qw( any none uniq );

my $lc = Game::EvonyTKR::Logger::Config->new('Game::EvonyTKR');
my $log4perl_logger = $lc->init('testing');
my $logger = Log::Log4perl->get_logger('Test');

# Locate data
my $dist_dir = Path::Tiny::path('./share');
my $collection_dir = $dist_dir->child("collections/data");


require Game::EvonyTKR::Model::General::Conflict;
require Game::EvonyTKR::Model::General::Conflict::Book;

# Manager setup
my $RootManager = Game::EvonyTKR::Model::EvonyTKR::Manager->new(SourceDir => $dist_dir);
$logger->info("Importing generals from $collection_dir");

$RootManager->generalManager->importAll($collection_dir->child("generals"));
my @generals = values %{ $RootManager->generalManager->get_all_generals() };
$logger->debug(sprintf('retrieved %s generals from manager', scalar(@generals)));

$RootManager->bookManager->importAll($collection_dir->child('skill books'));
$RootManager->bookManager->importAll($collection_dir->child('generic books'));
my @books = $RootManager->bookManager->get_all_books->@*;

my $conflicts = Game::EvonyTKR::Model::General::Conflict->new(
  rootManager => $RootManager,
);

isa_ok($conflicts, ['Game::EvonyTKR::Model::General::Conflict'], "conflicts is a ::General::Conflict object");

my $WASH        = $RootManager->generalManager->getGeneral('Washington Prime');
my $MARCO       = $RootManager->generalManager->getGeneral('Marco Polo');
my $AETHEL      = $RootManager->generalManager->getGeneral('Aethelflaed');
my $CAESAR      = $RootManager->generalManager->getGeneral('Caesar');
my $DF          = $RootManager->generalManager->getGeneral('David Farragut');
my $SC          = $RootManager->generalManager->getGeneral('Sun Ce');
my $GO          = $RootManager->generalManager->getGeneral('Gaius Octavius');
my $Hermes      = $RootManager->generalManager->getGeneral('Hermes');
my $Haakon      = $RootManager->generalManager->getGeneral('Haakon Haraldsson');
my $Barbarossa  = $RootManager->generalManager->getGeneral('Barbarossa');
my $Custer      = $RootManager->generalManager->getGeneral('George A Custer');
my $KA          = $RootManager->generalManager->getGeneral('King Arthur');
my $Jayavarman  = $RootManager->generalManager->getGeneral('Jayavarman II');
my $Cheng       = $RootManager->generalManager->getGeneral('Cheng Yaojin');
my $Laudon      = $RootManager->generalManager->getGeneral('Laudon');
my $Elektra     = $RootManager->generalManager->getGeneral('Elektra');
my $Franz       = $RootManager->generalManager->getGeneral('Franz Joseph I');
my $Douglas     = $RootManager->generalManager->getGeneral('Douglas');
my $Marcus      = $RootManager->generalManager->getGeneral('Marcus Agrippa');
my $Louis       = $RootManager->generalManager->getGeneral('Louis IX');

subtest 'All Generals have types to test against' => sub {
  for my $g (values %{ $RootManager->generalManager->get_all_generals }) {
    my $types = $g->type // [];
    ok(@$types, sprintf('%s has at least one type', $g->name));
  }
};

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

my $bc = Game::EvonyTKR::Model::General::Conflict::Book->new(
  rootManager => $RootManager,
);

my $l4ra = $RootManager->bookManager->getBook('Level 4 Ranged Troop Attack');
my $l4ms = $RootManager->bookManager->getBook('Level 4 March Size');

subtest 'Books ready for testing' => sub {
isa_ok($l4ra, ['Game::EvonyTKR::Model::Book'], sprintf('%s is a ::Model::Book', 'Level 4 Ranged Troop Attack'));
isa_ok($l4ms, ['Game::EvonyTKR::Model::Book'], sprintf('%s is a ::Model::Book', 'Level 4 March Size'));

done_testing();
};

subtest 'Partial Conflicts with Books' => sub {
  ok($bc->is_general_and_book_compatible($Elektra, $l4ra, { same_side => 1,}), 'Elektra and L4 Ranged Troop Attack work (same side)');
  ok(!$bc->is_general_and_book_compatible($Elektra, $l4ra, { same_side => 0,}), 'Elektra and L4 Ranged Troop Attack conflict (other side)');
  ok($bc->is_general_and_book_compatible($Custer, $l4ms, { same_side => 1,}), 'George A. Custer and L4 March Size work (same side)');
  ok(!$bc->is_general_and_book_compatible($Custer, $l4ms, { same_side => 0,}), 'George A. Custer and L4 March Size conflict (other side)');
  done_testing();
};

subtest 'Full Conflicts with Books' => sub {
  ok(!$bc->is_general_and_book_compatible($KA, $l4ms, { same_side => 1,}), 'King Arthur and Level 4 March Size conflict (same side)');
  ok(!$bc->is_general_and_book_compatible($KA, $l4ms, { same_side => 0,}), 'King Arthur and Level 4 March Size conflict (other side)');
  done_testing();
};

done_testing();


sub are_generals_compatible_either_role ($g1, $g2) {
  my $old = $conflicts->assume_g1_is_main;

  $conflicts->set_assume_g1_is_main(1);
  my $a = $conflicts->are_generals_compatible($g1, $g2);

  $conflicts->set_assume_g1_is_main(0);
  my $b = $conflicts->are_generals_compatible($g1, $g2);

  $conflicts->set_assume_g1_is_main($old);
  return $a || $b;
}

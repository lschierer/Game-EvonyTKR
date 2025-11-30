use v5.42.0;
use experimental qw(class);
use utf8::all;
use Mojo::File;
require Data::Printer;
use lib 't';
use lib 'lib';

require Game::EvonyTKR;
require Game::EvonyTKR::Role::Logging;
require Game::EvonyTKR::Shared::Constants;
use Test2::V0;
use List::AllUtils qw( any none uniq );
use Sereal::Encoder;
use Sereal::Decoder;
use Carp;

BEGIN {
  $ENV{MOJO_MODE}       = 'development';
  $ENV{TABLE_TERM_SIZE} = 75;
}

require Test::Package;

# Setup logger
my $logger;

require Game::EvonyTKR::Service::Conflicts;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR;
require Game::EvonyTKR::Model::Base;
require Game::EvonyTKR::Model::AscendingAttributes;
require Game::EvonyTKR::Model::Covenant;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Specialty;

my $conflicts = Game::EvonyTKR::Service::Conflicts->new(
  build_index     => 1,
  asst_has_dragon => 1,
  asst_has_spirit => 1,
);

sub are_generals_compatible_either_role ($g1, $g2) {
  my $old = $conflicts->assume_g1_is_main;

  $conflicts->assume_g1_is_main(1);
  my $a = $conflicts->are_generals_compatible($g1, $g2);

  $conflicts->assume_g1_is_main(0);
  my $b = $conflicts->are_generals_compatible($g1, $g2);

  $conflicts->assume_g1_is_main($old);
  return $a || $b;
}

{

  package TestBase::WithRoles;
  use Mojo::Base -base,                                     -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',           -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',            -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs', -role;
  use Carp;

  # Add minimal required attributes
  has 'app';
}
my $testPackage = Test::Package->new();
$logger = $testPackage->get_logger;

{

  package TestClass::Generals;
  use Mojo::Base 'TestBase::WithRoles';
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;
  use Carp;

  # Mock only what's needed
  sub render { }
  sub stash  { }
}

########################## Start Test Stuff

# Locate data

my $TestGenerals = TestClass::Generals->new();

my $memcachClient = $TestGenerals->general_cache();
if (not defined($memcachClient)) {
  croak('failed to define memcachClient');
}

# Manager setup

my $testManager;
my $generals;
my $ascending_attributes;
my $covenants;
my $generic_books;
my $builtin_books;
my $specialties;
subtest 'Data Setup' => sub {

  $testManager = Test::Package->new();
  $testManager->logger->info('starting testing.');
  isa_ok($testManager, ['Test::Package'], 'Test Package instantiated');

  $generals = $testManager->import_generals();
  ok(ref($generals) && scalar(@$generals),
    sprintf('imported %s generals', ref($generals) ? scalar(@$generals) : 0));

  $ascending_attributes = $testManager->import_ascendingAttributes();
  ok(
    ref($ascending_attributes) && scalar(@$ascending_attributes),
    sprintf('imported %s ascending_attributes',
      ref($ascending_attributes) ? scalar(@$ascending_attributes) : 0)
  );

  $covenants = $testManager->import_covenants();
  ok(
    ref($covenants) && scalar(@$covenants),
    sprintf('imported %s covenants', ref($covenants) ? scalar(@$covenants) : 0)
  );

  $generic_books = $testManager->import_generic_books();
  ok(
    ref($generic_books) && scalar(@$generic_books),
    sprintf('imported %s generic books',
      ref($generic_books) ? scalar(@$generic_books) : 0)
  );

  $builtin_books = $testManager->import_builtin_books();
  ok(
    ref($builtin_books) && scalar(@$builtin_books),
    sprintf('imported %s builtin books',
      ref($builtin_books) ? scalar(@$builtin_books) : 0)
  );

  $specialties = $testManager->import_specialties();
  ok(
    ref($specialties) && scalar(@$specialties),
    sprintf('imported %s specialties',
      ref($specialties) ? scalar(@$specialties) : 0)
  );

  subtest 'Populate Ascending Attributes' => sub {
    foreach my $general (@$generals) {
      my $pa = $general->populateAscendingAttributes();

      if ($general->ascending) {
        ok($pa && $pa == 1,
          sprintf('populated ascending attributes for "%s"', $general->name));
      }
      else {
        ok(
          !$pa || $pa == 0,
          sprintf(
'attempt to populate ascending attributes for "%s" correctly failed',
            $general->name)
        );
      }
    }
    done_testing();
  };

  subtest 'Populate Builtin Books' => sub {
    foreach my $general (@$generals) {
      my $result = $general->populateBuiltinBook();
      ok($result && $result == 1,
        sprintf('"%s" populated correctly', $general->name));
    }
    done_testing();
  };

  subtest 'Populate Specialties' => sub {
    foreach my $general (@$generals) {
      my $result = $general->populateSpecialties();
      ok($result && $result == 1,
        sprintf('"%s" populated correctly', $general->name));
    }
    done_testing();
  };

  done_testing();
};

# Check if memcached is available
my $test_key = 'memcached_test_' . time();
unless ($memcachClient->set($test_key, 'test')) {
  $logger->info('Memcached not available, skipping memcached tests');
  exit 1;
}

isa_ok(
  $conflicts,
  ['Game::EvonyTKR::Service::Conflicts'],
  "conflicts is a ::Service::Conflicts object"
);

subtest 'All Generals have types to test against' => sub {
  for my $g (sort { $a->name cmp $b->name }
    values %{ $TestGenerals->get_generals() }) {
    isa_ok(
      $g,
      ['Game::EvonyTKR::Model::General'],
      sprintf('%s is a ::Model::General', $g->name)
    );
    my $types = $g->type // [];
    ok(@$types, sprintf('%s has at least one type', $g->name));
    ok(defined($g->builtInBook()),
      sprintf('%s has a defined built in book', $g->name));
    ok(
      blessed($g->builtInBook())
        && $g->builtInBook->isa('Game::EvonyTKR::Model::Book'),
      sprintf('%s has a ::Model::Book', $g->name)
    );
    ok(
      blessed($g->builtInBook->buffs->[0])
        && $g->builtInBook->buffs->[0]->isa('Game::EvonyTKR::Model::Buff'),
      sprintf('first buff in book for %s is a ::Model::Buff', $g->name)
    );
    ok(
      $g->builtInBook->buffs->[0]->passive == 0
        || $g->builtInBook->buffs->[0] == 1,
      sprintf('able to test passive on first buff in book for %s', $g->name)
    );
  }
};

my $WASH       = $TestGenerals->get_general('Washington Prime',);
my $MARCO      = $TestGenerals->get_general('Marco Polo',);
my $AETHEL     = $TestGenerals->get_general('Aethelflaed',);
my $CAESAR     = $TestGenerals->get_general('Caesar',);
my $DF         = $TestGenerals->get_general('David Farragut',);
my $SC         = $TestGenerals->get_general('Sun Ce',);
my $GO         = $TestGenerals->get_general('Gaius Octavius',);
my $Hermes     = $TestGenerals->get_general('Hermes',);
my $Haakon     = $TestGenerals->get_general('Haakon Haraldsson',);
my $Barbarossa = $TestGenerals->get_general('Barbarossa',);
my $Custer     = $TestGenerals->get_general('George A Custer',);
my $KA         = $TestGenerals->get_general('King Arthur',);
my $Jayavarman = $TestGenerals->get_general('Jayavarman II',);
my $Cheng      = $TestGenerals->get_general('Cheng Yaojin',);
my $Laudon     = $TestGenerals->get_general('Laudon',);
my $Elektra    = $TestGenerals->get_general('Elektra',);
my $Franz      = $TestGenerals->get_general('Franz Joseph I',);
my $Douglas    = $TestGenerals->get_general('Douglas',);
my $Marcus     = $TestGenerals->get_general('Marcus Agrippa',);
my $Louis      = $TestGenerals->get_general('Louis IX',);
my $LouisXIV   = $TestGenerals->get_general('Louis XIV',);
my $OlavII     = $TestGenerals->get_general('Olav II',);

subtest 'Ensure Generals are Pressent for further tests' => sub {

  isa_ok(
    $WASH,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Washington Prime')
  );
  isa_ok(
    $MARCO,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Marco Polo')
  );
  isa_ok(
    $AETHEL,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Aethelflaed')
  );
  isa_ok(
    $CAESAR,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Caesar')
  );
  isa_ok(
    $DF,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'David Farragut')
  );
  isa_ok(
    $SC,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Sun Ce')
  );
  isa_ok(
    $GO,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Gaius Octavius')
  );
  isa_ok(
    $Hermes,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Hermes')
  );
  isa_ok(
    $Haakon,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Haakon Haraldsson')
  );
  isa_ok(
    $Barbarossa,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Barbarossa')
  );
  isa_ok(
    $Custer,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'George A. Custer')
  );
  isa_ok(
    $KA,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'King Arthur')
  );
  isa_ok(
    $Jayavarman,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Jayavarman II')
  );
  isa_ok(
    $Cheng,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Cheng Yaojin')
  );
  isa_ok(
    $Laudon,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Laudon')
  );
  isa_ok(
    $Elektra,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Elektra')
  );
  isa_ok(
    $Franz,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Franz Joseph I')
  );
  isa_ok(
    $Douglas,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Douglas')
  );
  isa_ok(
    $Marcus,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Marcus Agrippa')
  );
  isa_ok(
    $Louis,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Louis IX')
  );
  isa_ok(
    $LouisXIV,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Louis XIV')
  );
  isa_ok(
    $OlavII,
    ['Game::EvonyTKR::Model::General'],
    sprintf('%s is a ::Model::General', 'Olav II')
  );

  done_testing();
};

$conflicts->asst_has_dragon(1);
$conflicts->asst_has_spirit(1);

subtest 'Conflicting Generals' => sub {
  ok(!are_generals_compatible_either_role($AETHEL, $CAESAR),
    'Aethelflaed/Ceasar conflict');
  ok(!are_generals_compatible_either_role($Douglas, $Marcus),
    'Douglas/Marcus Agrippa conflict');
  ok(!are_generals_compatible_either_role($Elektra, $Franz),
    'Elektra/Franz Joseph I conflict');
  ok(!are_generals_compatible_either_role($Hermes, $WASH),
    'Hermes/Washington Prime conflict');
  ok(!are_generals_compatible_either_role($Hermes, $Barbarossa),
    'Hermes/Barbarossa conflict');
  ok(!are_generals_compatible_either_role($Hermes, $Haakon),
    'Hermes/Haakon conflict');
  ok(!are_generals_compatible_either_role($Hermes, $KA),
    'Hermes/King Arthur conflict');
  ok(!are_generals_compatible_either_role($Laudon, $Cheng),
    'Laudon/Cheng Yaojin conflict');
  ok(!are_generals_compatible_either_role($MARCO, $GO),
    'Marco/Gaius Octavius confict');
  ok(!are_generals_compatible_either_role($MARCO, $Jayavarman),
    'Marco Polo/Jayavarman II conflict');
  ok(!are_generals_compatible_either_role($WASH, $Barbarossa),
    'Washington Prime/Barbarossa conflict');
  ok(
    !are_generals_compatible_either_role($WASH, $Custer),
    'Washington Prime/George A. Custer conflict'
  );
  ok(
    !are_generals_compatible_either_role($WASH, $KA),
    'Washington Prime/King Arthur conflict'
  );
  ok(!are_generals_compatible_either_role($Louis, $Franz),
    'Louis IX/Franz Joseph I conflict');
  ok(!are_generals_compatible_either_role($Louis, $Elektra),
    'Louis IX/Elektra conflict');
  ok(!are_generals_compatible_either_role($Jayavarman, $GO),
    'Jayavarman II/Gaius Octavius conflict');
  done_testing();
};

subtest 'Working Pairs' => sub {
  ok(are_generals_compatible_either_role($Cheng, $Haakon),
    'Cheng Yaojin/Haakon work');
  ok(are_generals_compatible_either_role($Douglas, $Franz),
    'Douglas/Franz Joseph I work');
  ok(are_generals_compatible_either_role($Elektra, $Douglas),
    'Elektra/Douglas work');
  ok(are_generals_compatible_either_role($Elektra, $Marcus),
    'Elektra/Marcus Agrippa work');
  ok(are_generals_compatible_either_role($Laudon, $Haakon),
    'Laudon/Haakon work');
  ok(are_generals_compatible_either_role($MARCO, $WASH,),
    'Marco Polo/Washington Prime work');
  ok(
    are_generals_compatible_either_role($MARCO, $DF),
    'Marco Polo/David Farragut work (no overlaping types)'
  );
  ok(are_generals_compatible_either_role($MARCO, $Haakon),
    'Marco Polo/Haakon work');
  ok(are_generals_compatible_either_role($MARCO,  $SC), 'Marco/Sun Ce works');
  ok(are_generals_compatible_either_role($Marcus, $Franz),
    'Marcus Agrippa/Franz Joseph I work');
  ok(are_generals_compatible_either_role($Louis, $Marcus),
    'Louis IX/Marcus Agrippa work');
  ok(are_generals_compatible_either_role($Louis, $KA),
    'Louis IX/King Arthur work');
  ok(are_generals_compatible_either_role($Louis, $Douglas),
    'Louis IX/Douglas work');
  done_testing();
};

subtest 'Louis XIV Stackable Buffs Test' => sub {
  ok(are_generals_compatible_either_role($LouisXIV, $OlavII),
    'Louis XIV/Olav II should work (stackable)');
  ok(
    are_generals_compatible_either_role($LouisXIV, $MARCO),
    'Louis XIV/Marco Polo should work (stackable)'
  );
  done_testing();
};

# TODO: Implement Book conflict detection
# Use same conflicts object for book tests
my $bc = $conflicts;

# Get books for testing
my $l4ra = (
  grep {
    $_->name =~ /Ranged.*Attack/i && $_->name !~ /Monster/i && $_->level == 4
  } @$generic_books
)[0];
my $l4ms =
  (grep { $_->name =~ /March Size/i && $_->level == 4 } @$generic_books)[0];
my $l4maam =
  (grep { $_->name =~ /Mounted.*Attack.*Against.*Monster/i && $_->level == 4 }
    @$generic_books)[0];

subtest 'Books ready for testing' => sub {
  isa_ok(
    $l4ra,
    ['Game::EvonyTKR::Model::Book'],
    'Level 4 Ranged Troop Attack is a ::Model::Book'
  ) if $l4ra;
  isa_ok(
    $l4ms,
    ['Game::EvonyTKR::Model::Book'],
    'Level 4 March Size is a ::Model::Book'
  ) if $l4ms;

  done_testing();
};

subtest 'Partial Conflicts with Books' => sub {
SKIP: {
    skip "Books not available", 4 unless $l4ra && $l4ms;

    ok(
      $bc->is_general_and_book_compatible($Elektra, $l4ra, { same_side => 1, }),
      'Elektra and L4 Ranged Troop Attack work (same side)'
    );
    ok(
      !$bc->is_general_and_book_compatible(
        $Elektra, $l4ra, { same_side => 0, }
      ),
      'Elektra and L4 Ranged Troop Attack conflict (other side)'
    );
    ok(
      $bc->is_general_and_book_compatible($Custer, $l4ms, { same_side => 1, }),
      'George A. Custer and L4 March Size work (same side)'
    );
    ok(
      !$bc->is_general_and_book_compatible($Custer, $l4ms, { same_side => 0, }),
      'George A. Custer and L4 March Size conflict (other side)'
    );
  }
  done_testing();
};

subtest 'Full Conflicts with Books' => sub {
SKIP: {
    skip "Books not available", 2 unless $l4ms;
    ok(
      !$bc->is_general_and_book_compatible(
        $AETHEL, $l4maam, { same_side => 1, }
      ),
      'Aethelflaed and Level 4 Mounted Troop Attack Against Monster (same side)'
    );
    ok(
      !$bc->is_general_and_book_compatible($KA, $l4ms, { same_side => 1, }),
      'King Arthur and Level 4 March Size conflict (same side)'
    );
    ok(
      !$bc->is_general_and_book_compatible($KA, $l4ms, { same_side => 0, }),
      'King Arthur and Level 4 March Size conflict (other side)'
    );
  }
  done_testing();
};

done_testing();

__END__

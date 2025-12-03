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
use List::AllUtils qw( any none uniq first );
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
  use Mojo::Base -base,                               -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',     -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',      -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;
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
  done_testing();
  exit 1;
}

isa_ok(
  $conflicts,
  ['Game::EvonyTKR::Service::Conflicts'],
  "conflicts is a ::Service::Conflicts object"
);

subtest 'All Generals have types to test against' => sub {
  for my $g (sort { $a->name cmp $b->name } $generals->@*) {
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
  done_testing();
};

my $AETHEL       = first { $_->name eq 'Aethelflaed' } $generals->@*;
my $Artemis      = first { $_->name eq 'Artemis' } $generals->@*;
my $Baibars      = first { $_->name eq 'Baibars' } $generals->@*;
my $Barbarossa   = first { $_->name eq 'Barbarossa' } $generals->@*;
my $Beowulf      = first { $_->name eq 'Beowulf' } $generals->@*;
my $Bertrand     = first { $_->name eq 'Bertrand du Guesclin' } $generals->@*;
my $CAESAR       = first { $_->name eq 'Caesar' } $generals->@*;
my $Casimir      = first { $_->name eq 'Casimir Pulaski' } $generals->@*;
my $Champlain    = first { $_->name eq 'Champlain' } $generals->@*;
my $Cheng        = first { $_->name eq 'Cheng Yaojin' } $generals->@*;
my $Custer       = first { $_->name eq 'George A Custer' } $generals->@*;
my $DF           = first { $_->name eq 'David Farragut' } $generals->@*;
my $Douglas      = first { $_->name eq 'Douglas' } $generals->@*;
my $Elektra      = first { $_->name eq 'Elektra' } $generals->@*;
my $Franz        = first { $_->name eq 'Franz Joseph I' } $generals->@*;
my $Gaius        = first { $_->name eq 'Gaius Octavius' } $generals->@*;
my $Genghis      = first { $_->name eq 'Genghis Khan' } $generals->@*;
my $GeorgeMonck  = first { $_->name eq 'George Monck' } $generals->@*;
my $Haakon       = first { $_->name eq 'Haakon Haraldsson' } $generals->@*;
my $Hannibal     = first { $_->name eq 'Hannibal' } $generals->@*;
my $Hermes       = first { $_->name eq 'Hermes' } $generals->@*;
my $Hestia       = first { $_->name eq 'Hestia' } $generals->@*;
my $IiNaomasa    = first { $_->name eq 'Ii Naomasa' } $generals->@*;
my $Jayavarman   = first { $_->name eq 'Jayavarman II' } $generals->@*;
my $KA           = first { $_->name eq 'King Arthur' } $generals->@*;
my $Laudon       = first { $_->name eq 'Laudon' } $generals->@*;
my $LiJing       = first { $_->name eq 'Li Jing' } $generals->@*;
my $Louis        = first { $_->name eq 'Louis IX' } $generals->@*;
my $LouisXIV     = first { $_->name eq 'Louis XIV' } $generals->@*;
my $MARCO        = first { $_->name eq 'Marco Polo' } $generals->@*;
my $MariaTheresa = first { $_->name eq 'Maria Theresa' } $generals->@*;
my $Marcus       = first { $_->name eq 'Marcus Agrippa' } $generals->@*;
my $Martinus     = first { $_->name eq 'Martinus' } $generals->@*;
my $Mordred      = first { $_->name eq 'Mordred' } $generals->@*;
my $Napoleon     = first { $_->name eq 'Napoleon Prime' } $generals->@*;
my $Nathanael    = first { $_->name eq 'Nathanael Greene' } $generals->@*;
my $OlavII       = first { $_->name eq 'Olav II' } $generals->@*;
my $Poligenus    = first { $_->name eq 'Poligenus' } $generals->@*;
my $PrinceEugene = first { $_->name eq 'Prince Eugene' } $generals->@*;
my $Roland       = first { $_->name eq 'Roland' } $generals->@*;
my $SunCe        = first { $_->name eq 'Sun Ce' } $generals->@*;
my $WASH         = first { $_->name eq 'Washington Prime' } $generals->@*;

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
  ok(!are_generals_compatible_either_role($MARCO, $Gaius),
    'Marco/Gaius Octavius confict');
  ok(!are_generals_compatible_either_role($MARCO, $Jayavarman),
    'Marco Polo/Jayavarman II conflict');
  ok(!are_generals_compatible_either_role($WASH, $Barbarossa),
    'Washington Prime/Barbarossa conflict');
  ok(
    !are_generals_compatible_either_role($WASH, $Custer),
    'Washington Prime/George A. Custer conflict'
  );
  ok(!are_generals_compatible_either_role($WASH, $Haakon),
    'Washington Prime/Haakon conflict');
  ok(
    !are_generals_compatible_either_role($WASH, $KA),
    'Washington Prime/King Arthur conflict'
  );
  ok(!are_generals_compatible_either_role($Louis, $Franz),
    'Louis IX/Franz Joseph I conflict');
  ok(!are_generals_compatible_either_role($Louis, $Elektra),
    'Louis IX/Elektra conflict');
  ok(!are_generals_compatible_either_role($Jayavarman, $Gaius),
    'Jayavarman II/Gaius Octavius conflict');
  ok(!are_generals_compatible_either_role($Artemis, $Barbarossa),
    'Artemis/Barbarossa conflict');
  ok(!are_generals_compatible_either_role($Artemis, $Beowulf),
    'Artemis/Beowulf conflict');
  ok(!are_generals_compatible_either_role($Artemis, $Gaius),
    'Artemis/Gaius conflict');
  ok(!are_generals_compatible_either_role($Artemis, $Custer),
    'Artemis/Custer conflict');

  done_testing();
};

subtest 'Working Pairs' => sub {
  ok(are_generals_compatible_either_role($AETHEL, $Artemis),
    'Aethelflaed/Artemis work');
  ok(are_generals_compatible_either_role($AETHEL, $Baibars),
    'Aethelflaed/Baibars work');
  ok(are_generals_compatible_either_role($AETHEL, $Barbarossa),
    'Aethelflaed/Barbarossa work');
  ok(are_generals_compatible_either_role($AETHEL, $Beowulf),
    'Aethelflaed/Beowulf work');
  ok(are_generals_compatible_either_role($AETHEL, $Bertrand),
    'Aethelflaed/Bertrand work');
  ok(are_generals_compatible_either_role($AETHEL, $Casimir),
    'Aethelflaed/Casimir work');
  ok(are_generals_compatible_either_role($AETHEL, $Cheng),
    'Aethelflaed/Cheng work');
  ok(are_generals_compatible_either_role($AETHEL, $Gaius),
    'Aethelflaed/Gaius work');
  ok(are_generals_compatible_either_role($AETHEL, $Genghis),
    'Aethelflaed/Genghis work');
  ok(are_generals_compatible_either_role($AETHEL, $Custer),
    'Aethelflaed/George A Custer work');
  ok(are_generals_compatible_either_role($AETHEL, $GeorgeMonck),
    'Aethelflaed/George Monck work');
  ok(are_generals_compatible_either_role($AETHEL, $Haakon),
    'Aethelflaed/Haakon work');
  ok(are_generals_compatible_either_role($AETHEL, $Hannibal),
    'Aethelflaed/Hannibal work');
  ok(are_generals_compatible_either_role($AETHEL, $Hermes),
    'Aethelflaed/Hermes work');
  ok(are_generals_compatible_either_role($AETHEL, $Hestia),
    'Aethelflaed/Hestia work');
  ok(are_generals_compatible_either_role($AETHEL, $IiNaomasa),
    'Aethelflaed/Ii Naomasa work');
  ok(are_generals_compatible_either_role($AETHEL, $Jayavarman),
    'Aethelflaed/Jayavarman work');
  ok(are_generals_compatible_either_role($AETHEL, $KA),
    'Aethelflaed/King Arthur work');
  ok(are_generals_compatible_either_role($AETHEL, $Laudon),
    'Aethelflaed/Laudon work');
  ok(are_generals_compatible_either_role($AETHEL, $MARCO),
    'Aethelflaed/Marco Polo work');
  ok(are_generals_compatible_either_role($AETHEL, $MariaTheresa),
    'Aethelflaed/Maria Theresa work');
  ok(are_generals_compatible_either_role($AETHEL, $OlavII),
    'Aethelflaed/Olav II work');
  ok(are_generals_compatible_either_role($AETHEL, $Poligenus),
    'Aethelflaed/Poligenus work');
  ok(are_generals_compatible_either_role($AETHEL, $PrinceEugene),
    'Aethelflaed/Prince Eugene work');
  ok(are_generals_compatible_either_role($AETHEL, $SunCe),
    'Aethelflaed/Sun Ce work');
  ok(are_generals_compatible_either_role($AETHEL, $WASH),
    'Aethelflaed/Washington Prime work');
  ok(are_generals_compatible_either_role($Artemis, $Baibars),
    'Artemis/Baibars work');
  ok(are_generals_compatible_either_role($Artemis, $Bertrand),
    'Artemis/Bertrand work');
  ok(are_generals_compatible_either_role($Artemis, $Casimir),
    'Artemis/Casimir work');
  ok(are_generals_compatible_either_role($Artemis, $Cheng),
    'Artemis/Cheng work');
  ok(are_generals_compatible_either_role($Artemis, $Genghis),
    'Artemis/Genghis work');
  ok(are_generals_compatible_either_role($Artemis, $GeorgeMonck),
    'Artemis/George Monck work');
  ok(are_generals_compatible_either_role($Artemis, $Haakon),
    'Artemis/Haakon work');
  ok(are_generals_compatible_either_role($Artemis, $Hermes),
    'Artemis/Hermes work');
  ok(are_generals_compatible_either_role($Artemis, $IiNaomasa),
    'Artemis/Ii Naomasa work');
  ok(are_generals_compatible_either_role($Artemis, $Jayavarman),
    'Artemis/Jayavarman work');
  ok(are_generals_compatible_either_role($Artemis, $Laudon),
    'Artemis/Laudon work');
  ok(are_generals_compatible_either_role($Artemis, $LiJing),
    'Artemis/Li Jing work');
  ok(are_generals_compatible_either_role($Artemis, $LouisXIV),
    'Artemis/Louis XIV work');
  ok(are_generals_compatible_either_role($Artemis, $MARCO),
    'Artemis/Marco Polo work');
  ok(are_generals_compatible_either_role($Artemis, $MariaTheresa),
    'Artemis/Maria Theresa work');
  ok(are_generals_compatible_either_role($Artemis, $Mordred),
    'Artemis/Mordred work');
  ok(are_generals_compatible_either_role($Artemis, $Napoleon),
    'Artemis/Napoleon Prime work');
  ok(are_generals_compatible_either_role($Artemis, $Nathanael),
    'Artemis/Nathanael Greene work');
  ok(are_generals_compatible_either_role($Artemis, $OlavII),
    'Artemis/Olav II work');
  ok(are_generals_compatible_either_role($Artemis, $Poligenus),
    'Artemis/Poligenus work');
  ok(are_generals_compatible_either_role($Artemis, $PrinceEugene),
    'Artemis/Prince Eugene work');
  ok(are_generals_compatible_either_role($Artemis, $Roland),
    'Artemis/Roland work');
  ok(are_generals_compatible_either_role($Artemis, $SunCe),
    'Artemis/Sun Ce work');
  ok(are_generals_compatible_either_role($Artemis, $WASH),
    'Artemis/Washington Prime work');

  ok(are_generals_compatible_either_role($Baibars, $Barbarossa),
    'Baibars/Barbarossa work');
  ok(are_generals_compatible_either_role($Baibars, $Beowulf),
    'Baibars/Beowulf work');
  ok(are_generals_compatible_either_role($Baibars, $Bertrand),
    'Baibars/Bertrand work');
  ok(are_generals_compatible_either_role($Baibars, $Casimir),
    'Baibars/Casimir work');
  ok(are_generals_compatible_either_role($Baibars, $Cheng),
    'Baibars/Cheng work');
  ok(are_generals_compatible_either_role($Baibars, $Gaius),
    'Baibars/Gaius work');
  ok(are_generals_compatible_either_role($Baibars, $Genghis),
    'Baibars/Genghis work');
  ok(are_generals_compatible_either_role($Baibars, $Custer),
    'Baibars/George A Custer work');
  ok(are_generals_compatible_either_role($Baibars, $GeorgeMonck),
    'Baibars/George Monck work');
  ok(are_generals_compatible_either_role($Baibars, $Haakon),
    'Baibars/Haakon work');
  ok(are_generals_compatible_either_role($Baibars, $Hannibal),
    'Baibars/Hannibal work');
  ok(are_generals_compatible_either_role($Baibars, $Hermes),
    'Baibars/Hermes work');
  ok(are_generals_compatible_either_role($Baibars, $Hestia),
    'Baibars/Hestia work');
  ok(are_generals_compatible_either_role($Baibars, $IiNaomasa),
    'Baibars/Ii Naomasa work');
  ok(are_generals_compatible_either_role($Baibars, $Jayavarman),
    'Baibars/Jayavarman work');
  ok(are_generals_compatible_either_role($Baibars, $KA),
    'Baibars/King Arthur work');
  ok(are_generals_compatible_either_role($Baibars, $Laudon),
    'Baibars/Laudon work');
  ok(are_generals_compatible_either_role($Baibars, $LiJing),
    'Baibars/Li Jing work');
  ok(are_generals_compatible_either_role($Baibars, $LouisXIV),
    'Baibars/Louis XIV work');
  ok(are_generals_compatible_either_role($Baibars, $MARCO),
    'Baibars/Marco Polo work');
  ok(are_generals_compatible_either_role($Baibars, $MariaTheresa),
    'Baibars/Maria Theresa work');
  ok(are_generals_compatible_either_role($Baibars, $Martinus),
    'Baibars/Martinus work');
  ok(are_generals_compatible_either_role($Baibars, $Mordred),
    'Baibars/Mordred work');
  ok(are_generals_compatible_either_role($Baibars, $Napoleon),
    'Baibars/Napoleon Prime work');
  ok(are_generals_compatible_either_role($Baibars, $Nathanael),
    'Baibars/Nathanael Greene work');
  ok(are_generals_compatible_either_role($Baibars, $OlavII),
    'Baibars/Olav II work');
  ok(are_generals_compatible_either_role($Baibars, $Poligenus),
    'Baibars/Poligenus work');
  ok(are_generals_compatible_either_role($Baibars, $PrinceEugene),
    'Baibars/Prince Eugene work');
  ok(are_generals_compatible_either_role($Baibars, $Roland),
    'Baibars/Roland work');
  ok(are_generals_compatible_either_role($Baibars, $SunCe),
    'Baibars/Sun Ce work');
  ok(are_generals_compatible_either_role($Baibars, $WASH),
    'Baibars/Washington Prime work');

  ok(are_generals_compatible_either_role($Cheng, $Haakon),
    'Cheng Yaojin/Haakon work');
  ok(are_generals_compatible_either_role($Cheng, $Hermes),
    'Cheng Yaojin/Hermes work');
  ok(are_generals_compatible_either_role($Cheng, $Hestia),
    'Cheng Yaojin/Hestia work');
  ok(are_generals_compatible_either_role($Cheng, $Jayavarman),
    'Cheng Yaojin/Jayavarman work');
  ok(are_generals_compatible_either_role($Cheng, $KA),
    'Cheng Yaojin/King Arthur work');
  ok(are_generals_compatible_either_role($Cheng, $LouisXIV),
    'Cheng Yaojin/Louis XIV work');
  ok(are_generals_compatible_either_role($Cheng, $MARCO),
    'Cheng Yaojin/Marco Polo work');
  ok(are_generals_compatible_either_role($Cheng, $MariaTheresa),
    'Cheng Yaojin/Maria Theresa work');
  ok(are_generals_compatible_either_role($Cheng, $Mordred),
    'Cheng Yaojin/Mordred work');
  ok(are_generals_compatible_either_role($Cheng, $Napoleon),
    'Cheng Yaojin/Napoleon Prime work');
  ok(are_generals_compatible_either_role($Cheng, $Nathanael),
    'Cheng Yaojin/Nathanael Greene work');
  ok(are_generals_compatible_either_role($Cheng, $OlavII),
    'Cheng Yaojin/Olav II work');
  ok(are_generals_compatible_either_role($Cheng, $Poligenus),
    'Cheng Yaojin/Poligenus work');
  ok(are_generals_compatible_either_role($Cheng, $Roland),
    'Cheng Yaojin/Roland work');
  ok(are_generals_compatible_either_role($Cheng, $SunCe),
    'Cheng Yaojin/Sun Ce work');
  ok(are_generals_compatible_either_role($Cheng, $WASH),
    'Cheng Yaojin/Washington Prime work');

  ok(are_generals_compatible_either_role($Genghis, $Custer),
    'Genghis Khan/George A Custer work');
  ok(are_generals_compatible_either_role($Genghis, $GeorgeMonck),
    'Genghis Khan/George Monck work');
  ok(are_generals_compatible_either_role($Genghis, $Haakon),
    'Genghis Khan/Haakon work');
  ok(are_generals_compatible_either_role($Genghis, $Hannibal),
    'Genghis Khan/Hannibal work');
  ok(are_generals_compatible_either_role($Genghis, $Hermes),
    'Genghis Khan/Hermes work');
  ok(are_generals_compatible_either_role($Genghis, $Hestia),
    'Genghis Khan/Hestia work');
  ok(are_generals_compatible_either_role($Genghis, $IiNaomasa),
    'Genghis Khan/Ii Naomasa work');
  ok(are_generals_compatible_either_role($Genghis, $Jayavarman),
    'Genghis Khan/Jayavarman work');
  ok(are_generals_compatible_either_role($Genghis, $KA),
    'Genghis Khan/King Arthur work');
  ok(are_generals_compatible_either_role($Genghis, $Laudon),
    'Genghis Khan/Laudon work');
  ok(are_generals_compatible_either_role($Genghis, $LiJing),
    'Genghis Khan/Li Jing work');
  ok(are_generals_compatible_either_role($Genghis, $LouisXIV),
    'Genghis Khan/Louis XIV work');
  ok(are_generals_compatible_either_role($Genghis, $MARCO),
    'Genghis Khan/Marco Polo work');
  ok(are_generals_compatible_either_role($Genghis, $MariaTheresa),
    'Genghis Khan/Maria Theresa work');
  ok(are_generals_compatible_either_role($Genghis, $Martinus),
    'Genghis Khan/Martinus work');
  ok(are_generals_compatible_either_role($Genghis, $Mordred),
    'Genghis Khan/Mordred work');
  ok(are_generals_compatible_either_role($Genghis, $Napoleon),
    'Genghis Khan/Napoleon Prime work');
  ok(are_generals_compatible_either_role($Genghis, $Nathanael),
    'Genghis Khan/Nathanael Greene work');
  ok(are_generals_compatible_either_role($Genghis, $OlavII),
    'Genghis Khan/Olav II work');
  ok(are_generals_compatible_either_role($Genghis, $Poligenus),
    'Genghis Khan/Poligenus work');
  ok(are_generals_compatible_either_role($Genghis, $PrinceEugene),
    'Genghis Khan/Prince Eugene work');
  ok(are_generals_compatible_either_role($Genghis, $Roland),
    'Genghis Khan/Roland work');
  ok(are_generals_compatible_either_role($Genghis, $SunCe),
    'Genghis Khan/Sun Ce work');
  ok(are_generals_compatible_either_role($Genghis, $WASH),
    'Genghis Khan/Washington Prime work');

  ok(are_generals_compatible_either_role($Haakon, $Hannibal),
    'Haakon/Hannibal work');
  ok(are_generals_compatible_either_role($Haakon, $Hestia),
    'Haakon/Hestia work');
  ok(are_generals_compatible_either_role($Haakon, $KA),
    'Haakon/King Arthur work');
  ok(are_generals_compatible_either_role($Haakon, $Laudon),
    'Haakon/Laudon work');
  ok(are_generals_compatible_either_role($Haakon, $LouisXIV),
    'Haakon/Louis XIV work');
  ok(are_generals_compatible_either_role($Haakon, $MariaTheresa),
    'Haakon/Maria Theresa work');
  ok(are_generals_compatible_either_role($Haakon, $Mordred),
    'Haakon/Mordred work');
  ok(are_generals_compatible_either_role($Haakon, $Napoleon),
    'Haakon/Napoleon Prime work');
  ok(are_generals_compatible_either_role($Haakon, $Nathanael),
    'Haakon/Nathanael Greene work');
  ok(are_generals_compatible_either_role($Haakon, $OlavII),
    'Haakon/Olav II work');
  ok(are_generals_compatible_either_role($Haakon, $Poligenus),
    'Haakon/Poligenus work');
  ok(are_generals_compatible_either_role($Haakon, $PrinceEugene),
    'Haakon/Prince Eugene work');
  ok(are_generals_compatible_either_role($Haakon, $Roland),
    'Haakon/Roland work');

  ok(are_generals_compatible_either_role($Douglas, $Franz),
    'Douglas/Franz Joseph I work');
  ok(are_generals_compatible_either_role($Elektra, $Douglas),
    'Elektra/Douglas work');
  ok(are_generals_compatible_either_role($Elektra, $Marcus),
    'Elektra/Marcus Agrippa work');
  ok(are_generals_compatible_either_role($Haakon, $Jayavarman),
    'Haakon/Jayavarman II work');
  ok(are_generals_compatible_either_role($Haakon, $MARCO),
    'Haakon/Marco Polo work');
  ok(are_generals_compatible_either_role($Haakon, $SunCe),
    'Haakon/Sun Ce work');
  ok(are_generals_compatible_either_role($Haakon, $SunCe),
    'Haakon/Sun Ce work');
  ok(are_generals_compatible_either_role($Laudon, $Haakon),
    'Laudon/Haakon work');
  ok(are_generals_compatible_either_role($Laudon, $LouisXIV),
    'Laudon/Louis XIV work');
  ok(are_generals_compatible_either_role($Laudon, $MARCO),
    'Laudon/Marco Polo work');
  ok(are_generals_compatible_either_role($Laudon, $SunCe),
    'Laudon/Sun Ce work');
  ok(are_generals_compatible_either_role($Louis, $Douglas),
    'Louis IX/Douglas work');
  ok(are_generals_compatible_either_role($Louis, $KA),
    'Louis IX/King Arthur work');
  ok(are_generals_compatible_either_role($Louis, $Marcus),
    'Louis IX/Marcus Agrippa work');
  ok(
    are_generals_compatible_either_role($LouisXIV, $MARCO),
    'Louis XIV/Marco Polo should work (stackable)'
  );
  ok(are_generals_compatible_either_role($LouisXIV, $OlavII),
    'Louis XIV/Olav II should work (stackable)');
  ok(
    are_generals_compatible_either_role($MARCO, $DF),
    'Marco Polo/David Farragut work (no overlaping types)'
  );
  ok(are_generals_compatible_either_role($MARCO, $Haakon),
    'Marco Polo/Haakon work');
  ok(are_generals_compatible_either_role($MARCO, $SunCe), 'Marco/Sun Ce works');
  ok(are_generals_compatible_either_role($MARCO, $WASH,),
    'Marco Polo/Washington Prime work');
  ok(are_generals_compatible_either_role($Marcus, $Franz),
    'Marcus Agrippa/Franz Joseph I work');
  done_testing();
};

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
my $l4sma = (
  grep {
    $_->name =~ /Siege.*Attack/i && $_->name !~ /Monster/i && $_->level == 4
  } @$generic_books
)[0];
my $l4smd = (
  grep {
    $_->name =~ /Siege.*Defense/i && $_->name !~ /Monster/i && $_->level == 4
  } @$generic_books
)[0];
my $l4mdm =
  (grep { $_->name =~ /Mounted.*Defense.*Against.*Monster/i && $_->level == 4 }
    @$generic_books)[0];
my $l4ma = (
  grep {
    $_->name =~ /^Mounted.*Attack$/i && $_->name !~ /Monster/i && $_->level == 4
  } @$generic_books
)[0];
my $l4md = (
  grep {
         $_->name =~ /^Mounted.*Defense$/i
      && $_->name !~ /Monster/i
      && $_->level == 4
  } @$generic_books
)[0];

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

subtest 'Generals that should NOT conflict with books' => sub {
SKIP: {
    skip "Books not available", 5
      unless $l4sma && $l4smd && $l4mdm && $l4ma && $l4md;
    ok(
      $bc->is_general_and_book_compatible(
        $Champlain, $l4sma, { same_side => 1, }
      ),
      'Champlain and Level 4 Siege Machine Attack work (same side)'
    );
    ok(
      $bc->is_general_and_book_compatible(
        $Champlain, $l4smd, { same_side => 1, }
      ),
      'Champlain and Level 4 Siege Machine Defense work (same side)'
    );
    ok(
      $bc->is_general_and_book_compatible($AETHEL, $l4mdm, { same_side => 1, }),
      'Aethelflaed and Level 4 Mounted Defense Against Monster work (same side)'
    );
    ok(
      !$bc->is_general_and_book_compatible($Laudon, $l4ma, { same_side => 1, }),
      'Laudon and Level 4 Mounted Attack conflict (same side)'
    );
    ok(
      $bc->is_general_and_book_compatible($Laudon, $l4md, { same_side => 1, }),
      'Laudon and Level 4 Mounted Defense work (same side)'
    );
  }
  done_testing();
};

subtest 'All mounted_pairs should work' => sub {
  # Read all pairs from mounted_pairs file
  open my $fh, '<', 't/mounted_pairs' or do {
    plan skip_all => 'mounted_pairs file not found';
    return;
  };

  my @pairs;
  while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/;    # skip empty lines
    my ($g1_name, $g2_name) = split /;/, $line, 2;
    next unless defined $g1_name && defined $g2_name;
    # Trim whitespace
    $g1_name =~ s/^\s+|\s+$//g;
    $g2_name =~ s/^\s+|\s+$//g;
    push @pairs, [$g1_name, $g2_name];
  }
  close $fh;

  # Build general lookup hash
  my %general_by_name = map { $_->name => $_ } @$generals;

  my $skipped = 0;
  for my $pair (@pairs) {
    my ($g1_name, $g2_name) = @$pair;
    my $g1 = $general_by_name{$g1_name};
    my $g2 = $general_by_name{$g2_name};

    unless ($g1 && $g2) {
      $skipped++;
      next;
    }

    ok(are_generals_compatible_either_role($g1, $g2), "$g1_name/$g2_name work");
  }

  note("Skipped $skipped pairs due to missing generals") if $skipped;
  done_testing();
};

subtest 'All conflicting_pairs should conflict' => sub {
  # Read all pairs from conflicting_pairs file
  open my $fh, '<', 't/conflicting_pairs' or do {
    plan skip_all => 'conflicting_pairs file not found';
    return;
  };

  # Build general lookup hash
  my %general_by_name = map { $_->name => $_ } @$generals;

  my @pairs;
  my $skipped = 0;

  while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/;    # skip empty lines

    my ($g1_name, $g2_name) = split /;/, $line, 2;
    unless (defined $g1_name && defined $g2_name) {
      $skipped++;
      next;
    }

    # Trim whitespace
    $g1_name =~ s/^\s+|\s+$//g;
    $g2_name =~ s/^\s+|\s+$//g;

    my $g1 = $general_by_name{$g1_name};
    my $g2 = $general_by_name{$g2_name};

    unless ($g1 && $g2) {
      $skipped++;
      next;
    }

    push @pairs, [$g1_name, $g2_name, $g1, $g2];
  }
  close $fh;

  for my $pair (@pairs) {
    my ($g1_name, $g2_name, $g1, $g2) = @$pair;
    ok(!are_generals_compatible_either_role($g1, $g2),
      "$g1_name/$g2_name conflict");
  }

  note("Skipped $skipped pairs due to missing generals") if $skipped;
  note("Testing " . scalar(@pairs) . " conflict pairs");
  done_testing();
};

done_testing();

__END__

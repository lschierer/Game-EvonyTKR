#!/usr/bin/env perl
use v5.42.0;
use lib 't';
use lib 'lib';
use utf8::all;
use experimental qw(class);
use Test2::V0;
use Log::Log4perl::Level;
use Path::Tiny;
require YAML::PP;
use List::AllUtils qw( first any none all );

# set the environment variables before
# any BEGIN blocks in the modules being tested
BEGIN {
  $ENV{MOJO_MODE}       = 'development';
  $ENV{LOG_DEBUG}       = 0;
  $ENV{TABLE_TERM_SIZE} = 75;
}

# Load required modules
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Summarizer::Single;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR;
require Game::EvonyTKR::Model::Base;
require Game::EvonyTKR::Model::AscendingAttributes;
require Game::EvonyTKR::Model::Covenant;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Specialty;
require YAML::PP;
require Test::Package;

use Log::Log4perl qw(:levels);

my $testManager = Test::Package->new();
$testManager->logger->info('starting testing.');
isa_ok($testManager, ['Test::Package'], 'Test Package instantiated');

my $generic_books = $testManager->import_generic_books();
ok(
  ref($generic_books) && scalar(@$generic_books),
  sprintf('imported %s generic books',
    ref($generic_books) ? scalar(@$generic_books) : 0)
);

my $builtin_books = $testManager->import_builtin_books();
ok(
  ref($builtin_books) && scalar(@$builtin_books),
  sprintf('imported %s builtin books',
    ref($builtin_books) ? scalar(@$builtin_books) : 0)
);

my $ascending_attributes = $testManager->import_ascendingAttributes();
ok(
  ref($ascending_attributes) && scalar(@$ascending_attributes),
  sprintf('imported %s ascending_attributes',
    ref($ascending_attributes) ? scalar(@$ascending_attributes) : 0)
);

my $specialties = $testManager->import_specialties();
ok(
  ref($specialties) && scalar(@$specialties),
  sprintf('imported %s specialties',
    ref($specialties) ? scalar(@$specialties) : 0)
);

my $generals = $testManager->import_generals();
ok(ref($generals) && scalar(@$generals),
  sprintf('imported %s generals', ref($generals) ? scalar(@$generals) : 0));

my $covenants = $testManager->import_covenants();
ok(ref($covenants) && scalar(@$covenants),
  sprintf('imported %s covenants', ref($covenants) ? scalar(@$covenants) : 0));

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

# Get Marco Polo general
my $marco_polo = first { $_->name eq 'Marco Polo' } $generals->@*;
ok(defined $marco_polo, "Marco Polo general loaded");

my $aethelflaed = first { $_->name eq 'Aethelflaed' } $generals->@*;
ok(defined $aethelflaed, "Aethelflaed general loaded");

my $Harald = first { $_->name eq 'Harald' } $generals->@*;
ok(defined $Harald, "Harald general loaded");

# Test case: All values set to 'none'
subtest "Marco Polo with all values set to 'none'" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(
    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    books          => $generic_books,
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 65,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: All values set to 'none'
subtest "Aethelflaed with all values set to 'none'" => sub {

  my $covenant =
    first { $_->primary->name eq $aethelflaed->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(
    general        => $aethelflaed,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'PvM',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 80,
      'Defense'    => 125,
      'HP'         => 70
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Ranged troop buffs should match expected values"
  );

  # there are no Siege Against Monsters boooks
  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Additional test cases can be added here for other combinations
# For example:

# Test case: Red1 ascending, all else none
subtest "Marco Polo with Red1 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'red1',
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 95,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: Red2 ascending, all else none
subtest "Marco Polo with Red2 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'red2',
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 95,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: Red3 ascending, all else none
subtest "Marco Polo with Red3 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'red3',
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 125,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: Red4 ascending, all else none
subtest "Marco Polo with Red4 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'red4',
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 125,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Red5 ascending, all else none
subtest "Marco Polo with Red5 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'red5',
    covenantLevel  => 'none',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 165,
      'Defense'    => 90,
      'HP'         => 105
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Green 1st specialty, all else none
subtest "Marco Polo with Green 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'green',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 71,
      'Defense'    => 66,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Blue 1st specialty, all else none
subtest "Marco Polo with Blue 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'blue',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 72,
      'Defense'    => 67,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Purple 1st specialty, all else none
subtest "Marco Polo with Purple 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'purple',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 74,
      'Defense'    => 69,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Orange 1st specialty, all else none
subtest "Marco Polo with Orange 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'orange',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 76,
      'Defense'    => 71,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Gold 1st specialty, all else none
subtest "Marco Polo with Gold 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'gold',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 80,
      'Defense'    => 75,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Civilization covenant, all else none
subtest "Marco Polo with Civilization covenant, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'Civilization',
    specialty1     => 'none',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Add your expected values here
  # This is a placeholder - you'll need to compute these values by hand
  pass("Civilization covenant test placeholder - add actual assertions");
};

# Test case: All maxed out (Red5, Gold specialties, Civilization covenant)
subtest "Marco Polo with all maxed out" => sub {
  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'red5',
    covenantLevel  => 'civilization',
    specialty1     => 'gold',
    specialty2     => 'gold',
    specialty3     => 'gold',
    specialty4     => 'gold',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Add your expected values here
  # Add your expected values here
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 41,
      'Attack'     => 35,
      'Defense'    => 25,
      'HP'         => 25,
    },
    "Ground troop buffs should match expected values"
  );

  is($summarizer->buffValues->{'Mounted Troops'}->{'March Size'},
    41, "Marco Polo Attacking March Size");
  is($summarizer->buffValues->{'Mounted Troops'}->{'Attack'},
    265, "Marco Polo Attacking Attack");
  is($summarizer->buffValues->{'Mounted Troops'}->{'Defense'},
    120, "Marco Polo Attacking Defense");
  is($summarizer->buffValues->{'Mounted Troops'}->{'HP'},
    155, "Marco Polo Attacking HP");

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 41,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 41,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Ground Troops'},
    {
      'Attack'  => 0,
      'Defense' => 10,
      'HP'      => 10
    },
    "Ground troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Mounted Troops'},
    {
      'Attack'  => 0,
      'Defense' => 10,
      'HP'      => 10
    },
    "Mounted troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Ranged Troops'},
    {
      'Attack'  => 0,
      'Defense' => 0,
      'HP'      => 0
    },
    "Ranged troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Siege Machines'},
    {
      'Attack'  => 0,
      'Defense' => 0,
      'HP'      => 0
    },
    "Siege Machines debuffs should match expected values"
  );

};

subtest "Aethelflaed with all maxed out" => sub {

  my $covenant =
    first { $_->primary->name eq $aethelflaed->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $aethelflaed,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'PvM',
    ascendingLevel => 'red5',
    covenantLevel  => 'Civilization',
    specialty1     => 'gold',
    specialty2     => 'gold',
    specialty3     => 'gold',
    specialty4     => 'gold',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Add your expected values here
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 100,
      'Defense'    => 140,
      'HP'         => 115
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 155,
      'Defense'    => 205,
      'HP'         => 155
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 110,
      'HP'         => 115
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 65,
      'HP'         => 70
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Overall'        => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
    },
    "Aethelflaed's specific debuffs should be present"
  );
};

subtest "Harald with all maxed out" => sub {

  my $covenant   = first { $_->primary->name eq $Harald->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(

    general        => $Harald,
    isPrimary      => 1,
    targetType     => 'mayor',
    activationType => 'Mayor',
    ascendingLevel => 'red5',
    covenantLevel  => 'Civilization',
    specialty1     => 'gold',
    specialty2     => 'gold',
    specialty3     => 'gold',
    specialty4     => 'gold',
  );

  $summarizer->updateDebuffs();

  # Add your expected values here
  is(
    $summarizer->debuffValues->{'Ground Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Ground troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Mounted Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Mounted troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Ranged Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Ranged troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Siege Machines'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Siege Machines debuffs should match expected values"
  );

};

# Pair tests
require Game::EvonyTKR::Model::Buff::Summarizer::Pair;
require Game::EvonyTKR::Model::General::Pair;

subtest "Aethelflaed + Marco Polo pair with all values set to 'none'" => sub {

  # Create a pair object
  my $pair = Game::EvonyTKR::Model::General::Pair->new(
    primary   => $aethelflaed,
    secondary => $marco_polo,
    type      => 'mounted_specialist',
  );

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Pair->new(
    pair                   => $pair,
    targetType             => 'mounted_specialist',
    activationType         => 'PvM',
    ascendingLevel         => 'none',
    covenantLevel          => 'none',
    specialty1             => 'none',
    specialty2             => 'none',
    specialty3             => 'none',
    specialty4             => 'none',
    secondaryCovenantLevel => 'none',
    secondarySpecialty1    => 'none',
    secondarySpecialty2    => 'none',
    secondarySpecialty3    => 'none',
    secondarySpecialty4    => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Pair uses 6 generic books compatible with BOTH generals
  # Not simple addition of individual books
  is(
    $summarizer->pairBuffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 125,
      'Defense'    => 165,
      'HP'         => 110
    },
    "Pair mounted buffs from shared generic books"
  );

  # Debuffs should be 0 with no ascending/covenant/specialties
  is(
    $summarizer->pairDebuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present for pair"
  );
};

subtest "Aethelflaed + Marco Polo pair with gold specialties" => sub {

  my $pair = Game::EvonyTKR::Model::General::Pair->new(
    primary   => $aethelflaed,
    secondary => $marco_polo,
    type      => 'mounted_specialist',
  );

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Pair->new(
    pair                   => $pair,
    targetType             => 'mounted_specialist',
    activationType         => 'PvM',
    ascendingLevel         => 'none',
    covenantLevel          => 'none',
    specialty1             => 'gold',
    specialty2             => 'gold',
    specialty3             => 'gold',
    specialty4             => 'gold',
    secondaryCovenantLevel => 'none',
    secondarySpecialty1    => 'gold',
    secondarySpecialty2    => 'gold',
    secondarySpecialty3    => 'gold',
    secondarySpecialty4    => 'gold',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # With gold specialties on both generals plus shared books
  is(
    $summarizer->pairBuffValues->{'Mounted Troops'},
    {
      'March Size' => 33,
      'Attack'     => 245,
      'Defense'    => 225,
      'HP'         => 160
    },
    "Pair with gold specialties has combined specialty buffs plus shared books"
  );
};

subtest "Casimir Pulaski + Champlain pair with green 4th specialty" => sub {

  my $casimir = first { $_->name eq 'Casimir Pulaski' } $generals->@*;
  ok(defined $casimir, "Casimir Pulaski general loaded");

  my $champlain = first { $_->name eq 'Champlain' } $generals->@*;
  ok(defined $champlain, "Champlain general loaded");

  my $pair = Game::EvonyTKR::Model::General::Pair->new(
    primary   => $casimir,
    secondary => $champlain,
    type      => 'siege_specialist',
  );

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Pair->new(
    pair                   => $pair,
    targetType             => 'Siege Machines',
    activationType         => 'Attacking',
    ascendingLevel         => 'none',
    covenantLevel          => 'none',
    specialty1             => 'gold',
    specialty2             => 'gold',
    specialty3             => 'gold',
    specialty4             => 'green',
    secondaryCovenantLevel => 'none',
    secondarySpecialty1    => 'gold',
    secondarySpecialty2    => 'gold',
    secondarySpecialty3    => 'gold',
    secondarySpecialty4    => 'green',
    books                  => $generic_books,
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

# Expected values calculated manually:
# March: 12% (books) + 1% (Casimir sp4) + 6% (Champlain sp2) = 19%
# Attack: 25% (books) + 10% (Casimir sp1) + 6% (Casimir sp3) + 50% (Champlain book) + 10% (Champlain sp1) + 10% (Champlain sp2 Attacking) + 10% (Champlain sp3) = 121%
# Defense: 25% (books) + 10% (Casimir sp1) + 40% (Champlain book) + 10% (Champlain sp1) + 10% (Champlain sp3) = 95%
# HP: 25% (books) + 50% (Champlain book) + 10% (Champlain sp3) = 85%

  is(
    $summarizer->pairBuffValues->{'Siege Machines'},
    {
      'March Size' => 19,
      'Attack'     => 121,
      'Defense'    => 95,
      'HP'         => 85
    },
    "Casimir + Champlain siege pair buffs match manual calculation"
  );

# Debuffs: Casimir specialty 2 (Snipe) provides 10% Attack Debuff to all troop types
  is(
    $summarizer->pairDebuffValues,
    {
      'Ground Troops'  => { 'Attack' => 10, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 10, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 10, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 10, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 10, 'Defense' => 0, 'HP' => 0 },
    },
    "Casimir Snipe specialty provides 10% attack debuff to all troops"
  );
};

#
done_testing();

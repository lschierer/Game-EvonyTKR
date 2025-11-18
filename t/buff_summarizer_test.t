#!/usr/bin/env perl
use v5.40.0;
use experimental qw(class);
use utf8::all;
use Test::More;
use Test::Deep;
use File::FindLib 'lib';
use Data::Printer;

# Load required modules
require Data::Printer;
require File::ShareDir;
require Game::EvonyTKR::Model::AscendingAttributes::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Model::Covenant::Manager;
require Game::EvonyTKR::Model::EvonyTKR::Manager;
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR;
require Path::Tiny;
require Path::Tiny;
require YAML::PP;

use Log::Log4perl qw(:levels);
my $logger = Game::EvonyTKR::Log::Config->logger('Test::Package');
$logger->info(sprintf(
  'Test script logging configured with log level %s',
  Log::Log4perl::Level::to_level($logger->level()), ));


# Create a test class -- this needs to be totally replaced as it was last
# done mimicing a Corina class that no longer exists.
# it needs to import books, specialties, covenants,
# ascending attributes, and generals (in the right order, see how
# the prereqs map out across the *LoadALL* packages.
class BuffSummarizerTest : isa(Game::EvonyTKR::Shared::Constants) {


  field $dataDir = Path::Tiny::path('./share');


  method rootImport() {
    my $collectionDir = $dataDir->child("collections/data");
    say("starting root import");


    say("root import complete");

  }

  method AscendingLevelNames($red = 1, $printable = 0) {
    return $generalManager->AscendingLevelNames($red, $printable);
  }

  method AscendingLevelLabel($level) {
    return $generalManager->AscendingLevelLabel($level);
  }
}

# Create test instance and import data
my $testManager = BuffSummarizerTest->new();
$testManager->rootImport();

# Get Marco Polo general
my $marco_polo = $testManager->generalManager->getGeneral("Marco Polo");
ok(defined $marco_polo, "Marco Polo general loaded");

my $aethelflaed = $testManager->generalManager->getGeneral("Aethelflaed");
ok(defined $aethelflaed, "Aethelflaed general loaded");

my $Harald = $testManager->generalManager->getGeneral("Harald");
ok(defined $Harald, "Harald general loaded");

# Test case: All values set to 'none'
subtest "Marco Polo with all values set to 'none'" => sub {

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 65,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 80,
      'Defense'    => 125,
      'HP'         => 70
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 95,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 95,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 125,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 125,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 165,
      'Defense'    => 90,
      'HP'         => 105
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 71,
      'Defense'    => 66,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 72,
      'Defense'    => 67,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 74,
      'Defense'    => 69,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 76,
      'Defense'    => 71,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 80,
      'Defense'    => 75,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
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
  is_deeply(
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
  plan tests => 1;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
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

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 41,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 41,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  is_deeply(
    $summarizer->debuffValues->{'Ground Troops'},
    {
      'Attack'  => 0,
      'Defense' => 10,
      'HP'      => 10
    },
    "Ground troop debuffs should match expected values"
  );

  is_deeply(
    $summarizer->debuffValues->{'Mounted Troops'},
    {
      'Attack'  => 0,
      'Defense' => 10,
      'HP'      => 10
    },
    "Mounted troop debuffs should match expected values"
  );

  is_deeply(
    $summarizer->debuffValues->{'Ranged Troops'},
    {
      'Attack'  => 0,
      'Defense' => 0,
      'HP'      => 0
    },
    "Ranged troop debuffs should match expected values"
  );

  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 100,
      'Defense'    => 140,
      'HP'         => 115
    },
    "Ground troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 155,
      'Defense'    => 205,
      'HP'         => 155
    },
    "Mounted troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 110,
      'HP'         => 115
    },
    "Ranged troop buffs should match expected values"
  );

  is_deeply(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 110,
      'HP'         => 115
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is_deeply(
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

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    rootManager    => $testManager,
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
  is_deeply(
    $summarizer->debuffValues->{'Ground Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Ground troop debuffs should match expected values"
  );

  is_deeply(
    $summarizer->debuffValues->{'Mounted Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Mounted troop debuffs should match expected values"
  );

  is_deeply(
    $summarizer->debuffValues->{'Ranged Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Ranged troop debuffs should match expected values"
  );

  is_deeply(
    $summarizer->debuffValues->{'Siege Machines'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Siege Machines debuffs should match expected values"
  );

};

#
done_testing();

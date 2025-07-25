#!/usr/bin/env perl
use v5.40.0;
use utf8::all;
use experimental qw(class);
use File::FindLib 'lib';

require Data::Printer;
require File::ShareDir;
require Game::EvonyTKR;
require Log::Log4perl;
require Path::Tiny;
require Path::Tiny;
require Scalar::Util;
use namespace::clean;

use Test::Deep;
use Test::More;
use List::MoreUtils qw(uniq);


require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::AscendingAttributes::Manager;
require Game::EvonyTKR::Model::Book::Manager;
require Game::EvonyTKR::Model::Covenant::Manager;
require Game::EvonyTKR::Model::EvonyTKR::Manager;
require Game::EvonyTKR::Model::General::ConflictGroup::Manager;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::Specialty::Manager;

# set up logging that my Corina classes can use
my $loggerConfig = Game::EvonyTKR::Logger::Config->new('test');
my $logConfig = Path::Tiny->cwd()->child('share/log4perl.test.conf');
say $logConfig->absolute();
Log::Log4perl::Config->utf8(1);
Log::Log4perl::init($logConfig->canonpath());
my $logger = Log::Log4perl->get_logger('Game.EvonyTKR');

#testing infrastructure
my $distDir = Path::Tiny::path('./share');
my $RootManager =
  Game::EvonyTKR::Model::EvonyTKR::Manager->new(SourceDir => $distDir,);

#there is no need to do import *everything* but I do need to import enough to build pairs.
my $collectionDir = $distDir->child("collections/data");

say "collectionDir is " . $collectionDir->absolute->canonpath;

$logger->info("starting import of generals.");
$RootManager->generalManager->importAll($collectionDir->child("generals"));
$logger->info("import of generals complete.");

$logger->info(" starting import of conflict groups.");
$RootManager->generalConflictGroupManager->importAll(
  $collectionDir->child('general conflict groups'));
$logger->info("import of conflict groups complete");


# Now test conflict group functionality before building pairs
$logger->info("Testing conflict group functionality...");

# Test 1: Verify conflict groups were imported
my $conflictGroups = $RootManager->generalConflictGroupManager->get_conflict_groups();
ok(scalar keys %$conflictGroups > 0, "Conflict groups were imported");
diag("Found " . (scalar keys %$conflictGroups) . " conflict groups");

# Test 2: Test specific conflict group methods using known conflict data
# Conflict group 1939fb94-7382-53f6-858c-ba57897c0126: Algernon Sidney conflicts with Ragnar, Eleanor, Gunther, Princess Kaguya, and Zucca
my @conflictingGenerals = ("Algernon Sidney", "Ragnar", "Eleanor", "Gunther", "Princess Kaguya", "Zucca");

# Test that Algernon Sidney conflicts with each of the others
my $algernonSidney = $RootManager->generalManager->getGeneral("Algernon Sidney");
ok($algernonSidney, "Found Algernon Sidney general");

if ($algernonSidney) {
    foreach my $conflictingName ("Ragnar", "Eleanor", "Gunther", "Princess Kaguya", "Zucca") {
        my $conflictingGeneral = $RootManager->generalManager->getGeneral($conflictingName);

        SKIP: {
            skip "General $conflictingName not found", 1 unless $conflictingGeneral;

            my $compatible = $RootManager->generalConflictGroupManager->are_generals_compatible($algernonSidney->name, $conflictingGeneral->name);
            ok($compatible == 0, "Algernon Sidney should conflict with $conflictingName");

            if ($compatible) {
                diag("ERROR: Algernon Sidney does not conflict with $conflictingName but should! -- $compatible");
            }
        }
    }

    # Test that generals within the conflict group also conflict with each other
    my $ragnar = $RootManager->generalManager->getGeneral("Ragnar");
    my $eleanor = $RootManager->generalManager->getGeneral("Eleanor");

    if ($ragnar && $eleanor) {
        my $compatible = $RootManager->generalConflictGroupManager->are_generals_compatible($ragnar->name, $eleanor->name);
        ok($compatible == 0, "Ragnar should conflict with Eleanor (same conflict group)");
    }
}

# Now build pairs and test for conflicts
$logger->info("starting build pairs");
$RootManager->generalPairManager->build_pairs();
$logger->info("build pairs complete");

# Test 3: Verify no conflicting pairs were created
my @pairTypes = $RootManager->generalPairManager->get_pair_types();
my %allPairs;
my $conflictingPairs = 0;
my @foundConflicts;

foreach my $pt (@pairTypes){
  my @pairs = @{ $RootManager->generalPairManager->get_pairs_by_type($pt) };
  diag (sprintf('there are %s pairs of type %s.', scalar @pairs, $pt));
  foreach my $pair (@pairs) {
    my $primary = $pair->primary;
    my $secondary = $pair->secondary;
    my $pairKey = sprintf('%s-%s', $pair->primary->name, $pair->secondary->name);
    $allPairs{$pairKey} = $pair;

    # Check if this pair should conflict
    if (not $RootManager->generalConflictGroupManager->are_generals_compatible($primary->name, $secondary->name)) {
        $conflictingPairs++;
        my $conflictInfo = $primary->name . " <-> " . $secondary->name;
        push @foundConflicts, $conflictInfo;
        diag("CONFLICT FOUND: $conflictInfo");
    }
  }

}

is($conflictingPairs, 0, "No conflicting pairs should be created");
diag("Total pairs created: " . (scalar keys %allPairs));
diag("Conflicting pairs found: $conflictingPairs");

if (@foundConflicts) {
    diag("Specific conflicts found:");
    foreach my $conflict (@foundConflicts) {
        diag("  - $conflict");
    }
}

# Test 4: Specifically check for the known Algernon Sidney conflicts

if ($algernonSidney) {
    foreach my $conflictingName ("Ragnar", "Eleanor", "Gunther", "Princess Kaguya", "Zucca") {
        my $conflictingGeneral = $RootManager->generalManager->getGeneral($conflictingName);
        next unless $conflictingGeneral;

        # Check if a pair was incorrectly created between these two
        my $pairKey1 = $algernonSidney->name . "-" . $conflictingGeneral->name;
        my $pairKey2 = $conflictingGeneral->name . "-" . $algernonSidney->name;

        my $foundPair = $allPairs{$pairKey1} || $allPairs{$pairKey2};

        if ($foundPair) {
            fail("Pair should not exist between Algernon Sidney and $conflictingName");
            diag("Found prohibited pair: " . $foundPair->primary->name . " <-> " . $foundPair->secondary->name);
        } else {
            pass("Correctly excluded pair between Algernon Sidney and $conflictingName");
        }
    }
}

done_testing();

use v5.42.0;
use utf8::all;
use Mojo::File;
require Mojo::Home;
use Mojo::SQLite;
use lib 't';
use lib 'lib';
use List::AllUtils qw( first any none all );

use Test2::V0;

BEGIN {
  $ENV{MOJO_MODE} = 'development';
}

require Test::Package;
require Game::EvonyTKR::Service::Persistence;

# Setup
my $testManager = Test::Package->new();
my $generals;
my $ascending_attributes;
my $covenants;
my $generic_books;
my $builtin_books;
my $logger = $testManager->get_logger;

# Initialize persistence with SQLite connection
my $db_path = 'var/persistence.db';
my $sqlite = Mojo::SQLite->new("sqlite:$db_path");
my $persistence = Game::EvonyTKR::Service::Persistence->new(
  logger => $logger,
  sqlite => $sqlite,
);

# Helper to check if generals conflict using ML data from database
sub are_generals_conflicting ($g1_name, $g2_name) {
  my $result = $persistence->get_conflict($g1_name, $g2_name);
  return $result if defined $result;

  # If not in database, assume compatible (fail open)
  return 0;
}

subtest 'Data Setup' => sub {
  ok($persistence, 'Persistence initialized');
  ok($persistence->sqlite, 'SQLite connection available');

  # Verify we have conflict data loaded
  my $db = $persistence->sqlite->db;
  my $count = $db->query('SELECT COUNT(*) as cnt FROM general_conflicts')->hash->{cnt};
  ok($count > 0, "Conflict data loaded ($count pairs)");
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
  ok(ref($covenants) && scalar(@$covenants),
    sprintf('imported %s covenants', ref($covenants) ? scalar(@$covenants) : 0));

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
};

subtest 'Known Conflicts' => sub {
  # These pairs should conflict based on ML predictions
  my @known_conflicts = (
    ['Aethelflaed', 'Caesar'],
    ['Artemis','Barbarossa'],
    ['Artemis','Beowulf'],
    ['Artemis','Gaius Octavius'],
    ['Artemis','George A Custer'],
    ['Douglas', 'Marcus Agrippa'],
    ['Elektra', 'Franz Joseph I'],
    ['Hermes', 'Barbarossa'],
    ['Hermes', 'King Arthur'],
    ['Hermes', 'Washington Prime'],
    ['Jayavarman II','Gaius Octavius'],
    ['Laudon', 'Cheng Yaojin'],
    ['Leonidas I', 'Jadwiga'],
    ['Leonidas I', 'Lautaro'],
    ['Leonidas I', 'Queen Boudica'],
    ['Louis IX','Elektra'],
    ['Louis IX','Franz Joseph I'],
    ['Marco Polo', 'Gaius Octavius'],
    ['Marco Polo','Jayavarman II'],
    ['Washington Prime','Barbarossa'],
    ['Washington Prime','George A Custer'],
    ['Washington Prime','Haakon Haraldsson'],
    ['Washington Prime','King Arthur'],
  );

  foreach my $pair (@known_conflicts) {
    my ($g1, $g2) = @$pair;
    my $conflicts = are_generals_conflicting($g1, $g2);
    ok($conflicts, "$g1/$g2 conflict");
  }
};

subtest 'Known Compatible Pairs' => sub {
  # These pairs should be compatible based on ML predictions
  my @known_compatible = (
    ['Aethelflaed', 'Artemis'],
    ['Aethelflaed', 'Baibars'],
    ['Artemis', 'Genghis Khan'],
    ['Artemis', 'George Monck'],
    ['Baibars', 'George Monck'],
    ['Genghis Khan', 'George Monck'],
    ['Hermes', 'Haakon Haraldsson'],
  );

  foreach my $pair (@known_compatible) {
    my ($g1, $g2) = @$pair;
    my $conflicts = are_generals_conflicting($g1, $g2);
    ok(!$conflicts, "$g1/$g2 compatible");
  }
};

subtest 'CSV Passing Pairs' => sub {
  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $training_data = $mh->child('share/training_data/');

  foreach my $file ($training_data->list->grep(qr/pairs$/)->each) {
    next unless -f $file;
    next if $file =~ /conflict/i;
    note $file;

    my $fh = $file->open('<:encoding(UTF-8)') or do {
      skip_all(sprintf('%s not found', $file));
    };

    my @pairs;
    while(my $line = <$fh>){
      chomp $line;
      my ($g1_name, $g2_name) = split /;/, $line;
      next unless $g1_name && $g2_name;
      unless(any {lc($testManager->normalize($g1_name)) eq lc($testManager->normalize($_->name)) } $generals->@* ){
        note(sprintf('skipping pair with primary general "%s", which was not loaded by the testManager', $g1_name));
        next;
      }
      unless(any {lc($testManager->normalize($g2_name)) eq lc($testManager->normalize($_->name)) } $generals->@* ){
        note(sprintf('skipping pair with secondary general "%s", which was not loaded by the testManager', $g2_name));
        next;
      }

      push @pairs, [$g1_name, $g2_name];
    }
    close $fh;

    foreach my $pair (@pairs) {
      my ($g1_name, $g2_name) = @$pair;
      my $conflicts = are_generals_conflicting($g1_name, $g2_name);
      ok(!$conflicts, "$g1_name/$g2_name pair successfully");
    }
    note(sprintf('Testing %s working pairs from %s.', scalar(@pairs), $file ));
  }

  done_testing();
};

subtest 'CSV Conflict Pairs' => sub {
  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $csv_file = $mh->child('share/training_data/conflicting_pairs');

  unless (-f $csv_file) {
    skip_all("CSV file not found: $csv_file");
  }

  open my $fh, '<:encoding(UTF-8)', $csv_file or do {
    skip_all("Cannot open CSV file: $!");
  };

  my @pairs;

  while (my $line = <$fh>) {
    chomp $line;
    my ($g1_name, $g2_name) = split /;/, $line;
    next unless $g1_name && $g2_name;
    unless(any {lc($testManager->normalize($g1_name)) eq lc($testManager->normalize($_->name)) } $generals->@* ){
      note(sprintf('skipping pair with primary general "%s", which was not loaded by the testManager', $g1_name));
      next;
    }
    unless(any {lc($testManager->normalize($g2_name)) eq lc($testManager->normalize($_->name)) } $generals->@* ){
      note(sprintf('skipping pair with secondary general "%s", which was not loaded by the testManager', $g2_name));
      next;
    }

    push @pairs, [$g1_name, $g2_name];
  }
  close $fh;

  foreach my $pair (@pairs) {
    my ($g1_name, $g2_name) = @$pair;
    my $conflicts = are_generals_conflicting($g1_name, $g2_name);
    ok($conflicts, "$g1_name/$g2_name conflict");
  }

  note("Testing " . scalar(@pairs) . " conflict pairs from CSV");
};

done_testing();

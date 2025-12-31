#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use utf8::all;
use Test2::V0;
use File::Temp qw(tempfile);
use Mojo::File;

BEGIN {
  $ENV{MOJO_MODE} = 'testing';
}

require Game::EvonyTKR::Service::Persistence;

# Use temporary database for testing
my ($fh, $temp_db) = tempfile(SUFFIX => '.db', UNLINK => 1);
close $fh;

subtest 'Persistence Service Initialization' => sub {
  my $persist = Game::EvonyTKR::Service::Persistence->new(db_path => $temp_db);

  ok($persist,                 'Created persistence service');
  ok($persist->sqlite,         'SQLite connection established');
  ok($persist->is_initialized, 'Database initialized');
  is($persist->get_metadata('schema_version'), 1, 'Schema version is 1');
};

subtest 'Job Completion Tracking' => sub {
  my $persist = Game::EvonyTKR::Service::Persistence->new(db_path => $temp_db);

  ok(!$persist->is_job_completed('test_job'), 'Job not completed initially');

  $persist->mark_job_completed('test_job', 'test notes');
  ok($persist->is_job_completed('test_job'), 'Job marked as completed');

  my $completion_time = $persist->get_job_completion_time('test_job');
  ok($completion_time,     'Got completion timestamp');
  ok($completion_time > 0, 'Completion time is valid');
};

subtest 'General Storage and Retrieval' => sub {
  my $persist = Game::EvonyTKR::Service::Persistence->new(db_path => $temp_db);

  my $general_data = {
    name      => 'Aethelflaed',
    type      => ['mounted_specialist'],
    stars     => 'red5',
    ascending => 1,
  };

  $persist->store_general('Aethelflaed', $general_data);

  my $retrieved = $persist->get_general('Aethelflaed');
  is($retrieved->{name},  'Aethelflaed', 'Retrieved general name');
  is($retrieved->{stars}, 'red5',        'Retrieved general stars');
  ok($retrieved->{ascending}, 'Retrieved ascending flag');

  is($persist->count_generals(), 1, 'General count is 1');

  my $all_generals = $persist->list_generals();
  is(scalar @$all_generals,    1,             'List returns 1 general');
  is($all_generals->[0]{name}, 'Aethelflaed', 'List contains correct general');
};

subtest 'Generic Book Storage' => sub {
  my $persist = Game::EvonyTKR::Service::Persistence->new(db_path => $temp_db);

  my $book_data = {
    name  => 'Ranged Troop Attack',
    level => 4,
    buffs => [{
      attribute    => 'Attack',
      targetedType => 'Ranged Troops',
      value        => { number => 25, unit => 'percentage' },
    }],
  };

  $persist->store_generic_book('Ranged Troop Attack', 4, $book_data);

  my $retrieved = $persist->get_generic_book('Ranged Troop Attack', 4);
  is($retrieved->{name},  'Ranged Troop Attack',  'Retrieved book name');
  is($retrieved->{level}, 4,                      'Retrieved book level');
  is($retrieved->{buffs}[0]{attribute}, 'Attack', 'Retrieved buff attribute');

  # Store another level
  $book_data->{level} = 3;
  $book_data->{buffs}[0]{value}{number} = 20;
  $persist->store_generic_book('Ranged Troop Attack', 3, $book_data);

  my $all_books = $persist->list_generic_books();
  is(scalar @$all_books, 2, 'List returns 2 books');
};

subtest 'Needs Rebuild Detection' => sub {
  my $persist = Game::EvonyTKR::Service::Persistence->new(db_path => $temp_db);

  # Should not need rebuild - we have data
  ok(!$persist->needs_rebuild(), 'Does not need rebuild with data present');

  # Create empty database
  my ($fh2, $temp_db2) = tempfile(SUFFIX => '.db', UNLINK => 1);
  close $fh2;

  my $persist2 =
    Game::EvonyTKR::Service::Persistence->new(db_path => $temp_db2);
  ok($persist2->needs_rebuild(), 'Needs rebuild when empty');
};

subtest 'Lifecycle Job Isolation' => sub {
  my $persist1 = Game::EvonyTKR::Service::Persistence->new(
    db_path      => $temp_db,
    lifecycle_id => 'lifecycle_1'
  );

  $persist1->mark_job_completed('job_a');
  ok($persist1->is_job_completed('job_a'), 'Job completed in lifecycle 1');

  # Different lifecycle
  my $persist2 = Game::EvonyTKR::Service::Persistence->new(
    db_path      => $temp_db,
    lifecycle_id => 'lifecycle_2'
  );

  ok(!$persist2->is_job_completed('job_a'), 'Job not completed in lifecycle 2');

  $persist2->mark_job_completed('job_b');
  ok($persist2->is_job_completed('job_b'), 'Job b completed in lifecycle 2');
  ok(
    !$persist2->is_job_completed('job_a'),
    'Job a still not completed in lifecycle 2'
  );
};

done_testing();

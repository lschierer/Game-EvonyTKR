#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use utf8::all;
use Test2::V0;

BEGIN {
  $ENV{MOJO_MODE}      = 'testing';
  $ENV{POSTGRESQL_DSN} = 'postgresql:///evonytkr_app_data';
}

require Game::EvonyTKR::Service::PostgreSQLPersistence;

subtest 'PostgreSQL Persistence Initialization' => sub {
  my $persist = eval { Game::EvonyTKR::Service::PostgreSQLPersistence->new(); };

  if ($@) {
    skip_all "PostgreSQL not available: $@";
  }

  ok($persist,     'Created PostgreSQL persistence service');
  ok($persist->pg, 'PostgreSQL connection established');
  ok($persist->db, 'Database handle available');
};

subtest 'Metadata Storage and Retrieval' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  $persist->set_metadata('test_key', 'test_value');
  is($persist->get_metadata('test_key'),
    'test_value', 'Metadata stored and retrieved');

  $persist->set_metadata('test_key', 'updated_value');
  is($persist->get_metadata('test_key'), 'updated_value', 'Metadata updated');
};

subtest 'Job Completion Tracking' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  ok(!$persist->is_job_completed('test_job'), 'Job not completed initially');

  $persist->mark_job_completed('test_job');
  ok($persist->is_job_completed('test_job'), 'Job marked as completed');

  # Test with run_id
  ok(
    !$persist->is_job_completed('test_job_2', 'run123'),
    'Job not completed initially with run_id'
  );

  $persist->mark_job_completed('test_job_2', 'run123');
  ok($persist->is_job_completed('test_job_2', 'run123'),
    'Job marked as completed with run_id');
};

subtest 'General Storage and Retrieval' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

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

  ok($persist->count_generals() >= 1, 'General count is at least 1');

  my $all_generals = $persist->get_all_generals();
  ok(exists $all_generals->{Aethelflaed}, 'General exists in get_all_generals');
};

subtest 'Book Storage' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  my $book_data = {
    name  => 'Ranged Troop Attack',
    type  => 'generic',
    level => 4,
    buffs => [{
      attribute    => 'Attack',
      targetedType => 'Ranged Troops',
      value        => { number => 25, unit => 'percentage' },
    }],
  };

  $persist->store_generic_book('ranged_troop_attack_l4', $book_data);

  my $retrieved = $persist->get_generic_book('ranged_troop_attack_l4');
  is($retrieved->{name},  'Ranged Troop Attack', 'Retrieved book name');
  is($retrieved->{level}, 4,                     'Retrieved book level');

  ok($persist->count_generic_books() >= 1, 'Book count is at least 1');
};

subtest 'Conflict Storage and Retrieval' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  $persist->store_conflict('GeneralA', 'GeneralB', 1);
  ok($persist->get_conflict('GeneralA', 'GeneralB'),
    'Conflict stored and retrieved');
  ok($persist->get_conflict('GeneralB', 'GeneralA'),
    'Conflict is bidirectional');

  $persist->store_conflict('GeneralC', 'GeneralD', 0);
  ok(!$persist->get_conflict('GeneralC', 'GeneralD'),
    'No conflict stored correctly');
};

subtest 'Batch Conflict Storage' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  my $conflicts = {
    Gen1 => { Gen2 => 1, Gen3 => 0 },
    Gen2 => { Gen1 => 1, Gen3 => 1 },
    Gen3 => { Gen1 => 0, Gen2 => 1 },
  };

  my $count = $persist->store_conflicts_batch($conflicts);
  ok($count >= 3, 'Batch stored at least 3 conflict pairs');

  ok($persist->get_conflict('Gen1',  'Gen2'), 'Batch conflict 1-2 correct');
  ok($persist->get_conflict('Gen2',  'Gen3'), 'Batch conflict 2-3 correct');
  ok(!$persist->get_conflict('Gen1', 'Gen3'), 'Batch non-conflict 1-3 correct');
};

subtest 'Pairs Storage' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  my $pair_data = {
    primary   => 'GeneralA',
    secondary => 'GeneralB',
    buffs     => { attack => 100 },
  };

  $persist->store_pair('mounted/GeneralA/GeneralB', $pair_data);

  my $retrieved = $persist->get_pair('mounted/GeneralA/GeneralB');
  is($retrieved->{primary},   'GeneralA', 'Retrieved pair primary');
  is($retrieved->{secondary}, 'GeneralB', 'Retrieved pair secondary');
};

subtest 'Clear All Data' => sub {
  my $persist = Game::EvonyTKR::Service::PostgreSQLPersistence->new();

  # Clear all data for clean slate in future tests
  ok($persist->clear_all_data(), 'Successfully cleared all data');

  is($persist->count_generals(),      0, 'Generals table empty after clear');
  is($persist->count_generic_books(), 0, 'Books table empty after clear');
};

done_testing();

#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use utf8;
use experimental 'signatures';
use Test2::V0;

use PDL;
use PDL::NiceSlice;
use YAML::XS qw(LoadFile);
use Game::EvonyTKR::Service::PDL::Compiler;
use Game::EvonyTKR::Service::PDL::Runtime;

=head1 NAME

validate_pdl_compiler.t - Validate PDL compiler against golden test dataset

=head1 DESCRIPTION

This test validates that the PDL compiler produces the correct buff values
by comparing against the golden dataset extracted from t/buff_summarizer_test.t.

The golden dataset contains known-good expected values that the PDL implementation
must match exactly.

=cut

# Load golden dataset
my $golden_data = LoadFile('share/test_data/golden_buff_expectations.yaml');

# Create compiler
my $compiler = Game::EvonyTKR::Service::PDL::Compiler->new(
  data_dir => 'share/collections/data'
);

# Runtime provides the real mask/multiply logic; the test must not
# reimplement it (an earlier copy here had a wrong ascending ladder).
my $runtime = Game::EvonyTKR::Service::PDL::Runtime->new(
  data_dir => 'share/collections/data'
);

for my $test_case (@{$golden_data->{test_cases}}) {
  my $name = $test_case->{name};
  my $general_name = $test_case->{general};
  my $activation = $test_case->{activationType};
  my $filters = $test_case->{filters};
  my $expected_buffs = $test_case->{expected_buffs} || {};
  my $expected_debuffs = $test_case->{expected_debuffs} || {};

  subtest $name => sub {
    my $compiled = eval { $compiler->compile_general($general_name, $activation) };
    ok(!$@, "compiled $general_name ($activation) without error") or do {
      diag("Error: $@");
      return;
    };

    my $mask = $runtime->build_filter_mask($compiled, $filters);
    my $buff_vector = $runtime->matrix_multiply($compiled, $mask);

    # Check expected buffs
    for my $troop_type (sort keys %$expected_buffs) {
      my $troop_suffix = get_troop_suffix($troop_type);
      my $buffs = $expected_buffs->{$troop_type};

      my %col_map = map { $compiled->{buff_columns}[$_] => $_ }
                    0 .. $#{$compiled->{buff_columns}};

      for my $buff_name (sort keys %$buffs) {
        my $expected = $buffs->{$buff_name};

        if (lc($buff_name) eq 'march size') {
          my $column_key = 'march_size';
          next unless exists $col_map{$column_key};
          my $actual = $buff_vector->at($col_map{$column_key});
          is($actual, $expected, "$troop_type $buff_name");
          next;
        }

        my $column_key = get_column_key($buff_name, $troop_suffix);
        my $all_column_key = get_column_key($buff_name, 'all');

        my $specific_value = exists $col_map{$column_key} ?
          $buff_vector->at($col_map{$column_key}) : 0;
        my $all_value = exists $col_map{$all_column_key} ?
          $buff_vector->at($col_map{$all_column_key}) : 0;

        my $actual = $specific_value + $all_value;
        is($actual, $expected,
          "$troop_type $buff_name (specific=$specific_value, all=$all_value)");
      }
    }

    # Check expected debuffs
    for my $troop_type (sort keys %$expected_debuffs) {
      my $troop_suffix = get_troop_suffix($troop_type);
      my $debuffs = $expected_debuffs->{$troop_type};

      my %col_map = map { $compiled->{buff_columns}[$_] => $_ }
                    0 .. $#{$compiled->{buff_columns}};

      for my $buff_name (sort keys %$debuffs) {
        my $expected = $debuffs->{$buff_name};
        next if $expected == 0;

        my $column_key = get_column_key($buff_name, $troop_suffix);
        my $all_column_key = get_column_key($buff_name, 'all');

        my $enemy_specific_key = "enemy_${column_key}";
        my $enemy_all_key = "enemy_${all_column_key}";

        my $specific_value = exists $col_map{$enemy_specific_key} ?
          $buff_vector->at($col_map{$enemy_specific_key}) : 0;
        my $all_value = exists $col_map{$enemy_all_key} ?
          $buff_vector->at($col_map{$enemy_all_key}) : 0;

        my $actual = $specific_value + $all_value;
        is($actual, $expected,
          "$troop_type $buff_name debuff (specific=$specific_value, all=$all_value)");
      }
    }
  };
}

done_testing;

sub get_troop_suffix ($troop_type) {
  return 'ground' if $troop_type =~ /ground/i;
  return 'mounted' if $troop_type =~ /mounted/i;
  return 'ranged' if $troop_type =~ /ranged/i;
  return 'siege' if $troop_type =~ /siege/i;
  return 'all';
}

sub get_column_key ($buff_name, $troop_suffix) {
  my $key = lc($buff_name);
  $key =~ s/\s+/_/g;

  if ($key eq 'march_size') {
    return 'march_size';
  }
  elsif ($key =~ /^(attack|defense|hp)$/) {
    return "${key}_${troop_suffix}";
  }

  return $key;
}

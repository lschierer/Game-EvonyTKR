#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use lib 'lib';

use PDL;
use PDL::NiceSlice;
use YAML::XS qw(LoadFile);
use Mojo::Util qw(dumper);
use Game::EvonyTKR::Service::PDL::Compiler;
use Term::ANSIColor qw(colored);

=head1 NAME

validate_pdl_compiler.pl - Validate PDL compiler against golden test dataset

=head1 SYNOPSIS

  ./scripts/validate_pdl_compiler.pl

=head1 DESCRIPTION

This script validates that the PDL compiler produces the correct buff values
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

my $total_tests = 0;
my $passed_tests = 0;
my $failed_tests = 0;

print colored(['bold'], "="x80 . "\n");
print colored(['bold'], "PDL Compiler Validation\n");
print colored(['bold'], "="x80 . "\n\n");

for my $test_case (@{$golden_data->{test_cases}}) {
  my $name = $test_case->{name};
  my $general_name = $test_case->{general};
  my $activation = $test_case->{activationType};
  my $filters = $test_case->{filters};
  my $expected_buffs = $test_case->{expected_buffs} || {};
  my $expected_debuffs = $test_case->{expected_debuffs} || {};

  print colored(['cyan'], "Testing: $name\n");
  print "  General: $general_name ($activation)\n";

  eval {
    # Compile general
    my $compiled = $compiler->compile_general($general_name, $activation);

    # Build filter mask
    my $mask = build_filter_mask($compiled, $filters);

    # Compute buffs
    my $buff_vector = compute_buffs($compiled, $mask);

    # Extract values and compare
    my $result = compare_results($compiled, $buff_vector, $expected_buffs, $expected_debuffs);

    if ($result->{passed}) {
      print colored(['green'], "  ✓ PASSED\n");
      $passed_tests++;
    } else {
      print colored(['red'], "  ✗ FAILED\n");
      for my $error (@{$result->{errors}}) {
        print colored(['red'], "    - $error\n");
      }
      $failed_tests++;

      # Debug: show the matrix and mask
      if ($ENV{DEBUG}) {
        print "\n  Mask dimensions: " . join(" x ", $mask->dims) . "\n";
        print "\n  Row labels:\n";
        my $mask_flat = $mask->flat;  # Flatten in case it's 2D
        for my $i (0 .. $#{$compiled->{row_labels}}) {
          my $label = $compiled->{row_labels}[$i];
          my $mask_val = $mask_flat->at($i);
          my $marker = $mask_val ? "  <-- SELECTED" : "";
          print sprintf("    [%2d] %-20s%s\n", $i, $label, $marker);
        }
        print "\n  Buff columns: " . join(", ", @{$compiled->{buff_columns}}) . "\n";
        print "\n  Matrix dimensions: " . join(" x ", $compiled->{matrix}->dims) . "\n";
        print "\n  Result vector dimensions: " . join(" x ", $buff_vector->dims) . "\n";
        print "\n  Result vector:\n";
        print "  " . $buff_vector . "\n\n";
      }
    }

    $total_tests++;
  };

  if ($@) {
    print colored(['red'], "  ✗ ERROR: $@\n");
    $failed_tests++;
    $total_tests++;
  }

  print "\n";
}

print colored(['bold'], "="x80 . "\n");
print colored(['bold'], "Summary\n");
print colored(['bold'], "="x80 . "\n");
print "Total tests: $total_tests\n";
print colored(['green'], "Passed: $passed_tests\n");
print colored(['red'], "Failed: $failed_tests\n");

if ($failed_tests == 0) {
  print colored(['green bold'], "\n✓ All tests passed!\n");
  exit 0;
} else {
  print colored(['red bold'], "\n✗ Some tests failed.\n");
  print "\nRun with DEBUG=1 to see detailed output.\n";
  exit 1;
}

=head2 build_filter_mask

Builds a binary mask vector from user filter selections.

=cut

sub build_filter_mask ($compiled, $filters) {
  my @mask;
  my $row_labels = $compiled->{row_labels};

  for my $i (0 .. $#{$row_labels}) {
    my $label = $row_labels->[$i];
    my $active = 0;

    # Book is always active
    if ($label eq 'book') {
      $active = 1;
    }
    # Ascending levels (cumulative)
    elsif ($label =~ /^asc_(.+)$/) {
      my $level = $1;
      my $selected = $filters->{ascendingLevel} || 'none';
      $active = is_ascending_active($level, $selected);
    }
    # Covenant levels (only selected level is active)
    elsif ($label =~ /^cov_(.+)$/) {
      my $level = $1;
      my $selected = lc($filters->{covenantLevel} || 'none');
      $active = (lc($level) eq $selected) ? 1 : 0;
    }
    # Specialty levels
    elsif ($label =~ /^spec(\d+)_(.+)$/) {
      my $slot = $1;
      my $level = $2;
      my $selected = lc($filters->{"specialty$slot"} || 'none');
      $active = (lc($level) eq $selected) ? 1 : 0;
    }
    # Generic book level (single row, activated by generic1 filter)
    elsif ($label =~ /^generic_(.+)$/) {
      my $level = $1;
      my $selected = lc($filters->{generic1} || 'none');  # Use generic1 as the selector
      $active = (lc($level) eq $selected) ? 1 : 0;
    }

    push @mask, $active;
  }

  return pdl(@mask);
}

sub is_ascending_active ($level, $selected) {
  my @levels = qw(none red1 red2 red3 red4 red5 orange1 orange2 orange3 orange4 orange5);
  my %level_num = map { $levels[$_] => $_ } 0 .. $#levels;

  my $level_idx = $level_num{$level} // 0;
  my $selected_idx = $level_num{$selected} // 0;

  # Ascending is cumulative: if selected=red3, then red1, red2, red3 are all active
  return $level_idx > 0 && $level_idx <= $selected_idx ? 1 : 0;
}

=head2 compute_buffs

Computes active buffs by matrix multiplication.

=cut

sub compute_buffs ($compiled, $mask) {
  my $matrix = $compiled->{matrix};

  # Matrix dimensions: [n_rows, n_cols]
  # Mask dimensions: [n_rows] or [n_rows, 1]
  # We want to select rows where mask=1 and sum them

  # Ensure mask is 1D
  my $mask_1d = $mask->flat;

  # Reshape mask to [n_rows, 1] for broadcasting
  my $mask_col = $mask_1d->reshape($mask_1d->nelem, 1);

  # Multiply matrix by mask (broadcasts across columns)
  my $masked_matrix = $matrix * $mask_col;

  # Sum over rows (first dimension) to get final buff values [n_cols]
  my $result = sumover($masked_matrix);

  return $result;
}

=head2 compare_results

Compares computed buffs against expected values.

=cut

sub compare_results ($compiled, $buff_vector, $expected_buffs, $expected_debuffs) {
  my @errors;
  my $passed = 1;

  # Map buff columns to indices
  my %col_map = map { $compiled->{buff_columns}[$_] => $_ }
                0 .. $#{$compiled->{buff_columns}};

  # Check expected buffs
  for my $troop_type (keys %$expected_buffs) {
    my $troop_suffix = get_troop_suffix($troop_type);
    my $buffs = $expected_buffs->{$troop_type};

    for my $buff_name (keys %$buffs) {
      my $expected = $buffs->{$buff_name};

      # Skip march size - it's universal, no _all column
      if (lc($buff_name) eq 'march size') {
        my $column_key = 'march_size';
        next unless exists $col_map{$column_key};
        my $actual = $buff_vector->at($col_map{$column_key});
        if ($actual != $expected) {
          push @errors, sprintf(
            "%s %s: expected %d, got %d",
            $troop_type, $buff_name, $expected, $actual
          );
          $passed = 0;
        }
        next;
      }

      # For Attack/Defense/HP, combine specific column + _all column
      my $column_key = get_column_key($buff_name, $troop_suffix);
      my $all_column_key = get_column_key($buff_name, 'all');

      my $specific_value = exists $col_map{$column_key} ?
        $buff_vector->at($col_map{$column_key}) : 0;
      my $all_value = exists $col_map{$all_column_key} ?
        $buff_vector->at($col_map{$all_column_key}) : 0;

      my $actual = $specific_value + $all_value;

      if ($actual != $expected) {
        push @errors, sprintf(
          "%s %s: expected %d, got %d (specific=%d, all=%d)",
          $troop_type, $buff_name, $expected, $actual, $specific_value, $all_value
        );
        $passed = 0;
      }
    }
  }

  # Check expected debuffs (stored in enemy_* columns as positive values)
  for my $troop_type (keys %$expected_debuffs) {
    my $troop_suffix = get_troop_suffix($troop_type);
    my $debuffs = $expected_debuffs->{$troop_type};

    for my $buff_name (keys %$debuffs) {
      my $expected = $debuffs->{$buff_name};  # Expected debuff value (positive)
      next if $expected == 0;  # Skip zero debuffs

      # For debuffs, also combine specific + _all columns
      my $column_key = get_column_key($buff_name, $troop_suffix);
      my $all_column_key = get_column_key($buff_name, 'all');

      my $enemy_specific_key = "enemy_${column_key}";
      my $enemy_all_key = "enemy_${all_column_key}";

      my $specific_value = exists $col_map{$enemy_specific_key} ?
        $buff_vector->at($col_map{$enemy_specific_key}) : 0;
      my $all_value = exists $col_map{$enemy_all_key} ?
        $buff_vector->at($col_map{$enemy_all_key}) : 0;

      my $actual = $specific_value + $all_value;

      if ($actual != $expected) {
        push @errors, sprintf(
          "%s %s debuff: expected %d, got %d (specific=%d, all=%d)",
          $troop_type, $buff_name, $expected, $actual, $specific_value, $all_value
        );
        $passed = 0;
      }
    }
  }

  return {
    passed => $passed,
    errors => \@errors,
  };
}

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

1;

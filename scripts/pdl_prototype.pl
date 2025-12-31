#!/usr/bin/env perl
use v5.42.0;
use utf8;
use strict;
use warnings;
use PDL;
use PDL::NiceSlice;
use Benchmark qw(timethese cmpthese);
use Data::Dumper;

=head1 NAME

pdl_prototype.pl - Proof of concept for vectorizing general buffs with PDL

=head1 CONCEPT

Instead of conditionally summing buffs at runtime, we pre-compile all
possible buff combinations into numeric vectors, then use linear algebra
at runtime.

=head2 Current Approach (Procedural)

  foreach my $pair (@pairs) {
    my $buffs = 0;
    $buffs += $base_attack;
    $buffs += $ascending[$level] if $ascending_active;
    $buffs += $covenant[$value] if $covenant_active;
    $buffs += $specialty1[$s1] if $s1_active;
    ... etc ...
    $totals{$pair} = $buffs;
  }
  sort { $totals{$b} <=> $totals{$a} } @pairs;

Cost: O(pairs × conditions) = 1000 × 50 = 50,000 operations

=head2 PDL Approach (Vector Math)

  # Build time (once):
  $general_matrix = compile_general_to_vectors($general);

  # Runtime (per request):
  $filter_mask = build_filter_mask($user_selections);
  $active_buffs = $general_matrix x $filter_mask;  # Matrix multiply
  $pair_buffs = $g1_buffs + $g2_buffs;              # Vector addition
  $sorted = $pair_buffs->qsorti;                    # Sort

Cost: O(matrix_multiply + sort) ≈ 100 + 1000log(1000) ≈ 10,000 operations

=cut

# ============================================================================
# DATA STRUCTURES
# ============================================================================

# Simplified Aethelflaed data (extracted from YAML)
my $aethelflaed = {
  name => 'Aethelflaed',

  # Skill book: Lady of the Mercians
  book_buffs => [
    {
      attribute => 'Attack',
      targetedType => 'Mounted Troops',
      conditions => ['leading', 'monsters'],
      value => 55,  # percentage
    },
    {
      attribute => 'Defense',
      targetedType => 'Mounted Troops',
      conditions => ['leading', 'monsters'],
      value => 55,
    },
  ],

  # Ascending (red1-red5, each adds cumulative buffs)
  ascending => {
    red1 => [
      { attribute => 'HP', targetedType => 'Mounted Troops', conditions => ['monsters'], value => 30 },
      { attribute => 'Defense', conditions => ['monsters'], value => 10 },
    ],
    red2 => [
      { attribute => 'Defense', conditions => ['monsters'], value => 10 },
      { attribute => 'HP', conditions => ['monsters'], value => 15 },
    ],
    red3 => [
      { attribute => 'Attack', conditions => ['monsters'], value => 10 },
      { attribute => 'Defense', conditions => ['monsters'], value => 20 },
    ],
    red4 => [
      { attribute => 'Attack', targetedType => 'Mounted Troops', conditions => ['monsters'], value => 15 },
      { attribute => 'HP', targetedType => 'Mounted Troops', conditions => ['monsters'], value => 20 },
    ],
    red5 => [
      { attribute => 'Attack', targetedType => 'Mounted Troops', conditions => ['monsters'], value => 20 },
      { attribute => 'Defense', targetedType => 'Mounted Troops', conditions => ['monsters'], value => 30 },
    ],
  },

  # Specialties (Hunter, Fortune, Mounted Ares, Lord of Mercians)
  # Each has 5 levels (green, blue, purple, orange, gold)
  specialties => {
    Hunter => {
      # All levels give: Monster Attack +1-4%
      gold => [ { attribute => 'Attack', conditions => ['monsters'], value => 4 } ],
    },
    # ... others omitted for brevity
  },
};

# ============================================================================
# VECTORIZATION APPROACH
# ============================================================================

=head2 Vector Encoding Scheme

We encode all possible buff states as a matrix where:
- Rows = buff sources (base, ascending, covenant, specialties)
- Columns = buff values (attack_mounted, defense_mounted, hp_mounted, ...)

Example for Aethelflaed attacking monsters with mounted troops:

  Buff Source        | Attack_M | Defense_M | HP_M | Attack_All | Defense_All | HP_All
  -------------------|----------|-----------|------|------------|-------------|--------
  Base (book)        |    55    |    55     |  0   |     0      |      0      |   0
  Ascending_red1     |     0    |     0     | 30   |     0      |     10      |   0
  Ascending_red2     |     0    |     0     |  0   |     0      |     10      |  15
  Ascending_red3     |     0    |     0     |  0   |    10      |     20      |   0
  Ascending_red4     |    15    |     0     | 20   |     0      |      0      |   0
  Ascending_red5     |    20    |    30     |  0   |     0      |      0      |   0
  Specialty_Hunter   |     0    |     0     |  0   |     4      |      0      |   0
  ...

Then user selections become a binary mask:
  User selects: ascending=red5, hunter=gold

  Mask = [1, 1, 1, 1, 1, 1, 1, ...]  # Which rows to include
         ^  ^  ^  ^  ^  ^  ^
         |  |  |  |  |  |  |
         |  r  r  r  r  r  hunter
         base e1 e2 e3 e4 e5

  Active buffs = matrix × mask = row-wise sum of enabled rows

=cut

sub compile_general_to_pdl {
  my ($general_data, $troop_type, $activation) = @_;

  # Define our buff dimensions
  # For simplicity: [Attack_Mounted, Defense_Mounted, HP_Mounted, Attack_All, Defense_All, HP_All]
  my @buff_names = qw(attack_mounted defense_mounted hp_mounted attack_all defense_all hp_all);
  my $num_buffs = scalar @buff_names;

  # Initialize matrix: rows = buff sources, cols = buff values
  my @rows;

  # Row 0: Base book buffs
  my $base = zeros($num_buffs);
  foreach my $buff (@{$general_data->{book_buffs}}) {
    next unless applies($buff, $troop_type, $activation);
    my $idx = buff_index($buff->{attribute}, $buff->{targetedType});
    $base->set($idx, $buff->{value});
  }
  push @rows, $base;

  # Rows 1-5: Ascending levels (cumulative!)
  my @asc_levels = qw(red1 red2 red3 red4 red5);
  foreach my $level (@asc_levels) {
    my $asc_buffs = zeros($num_buffs);
    foreach my $buff (@{$general_data->{ascending}{$level} || []}) {
      next unless applies($buff, $troop_type, $activation);
      my $idx = buff_index($buff->{attribute}, $buff->{targetedType});
      $asc_buffs->set($idx, $buff->{value});
    }
    push @rows, $asc_buffs;
  }

  # Convert to PDL matrix
  my $matrix = pdl(@rows);

  return {
    matrix => $matrix,
    buff_names => \@buff_names,
    row_labels => ['base', @asc_levels],
  };
}

sub applies {
  my ($buff, $troop_type, $activation) = @_;
  # Simplified: just check if conditions match
  # Real version would check all conditions properly
  return 1 if grep { $_ eq 'monsters' } @{$buff->{conditions} || []};
  return 0;
}

sub buff_index {
  my ($attribute, $targeted_type) = @_;
  # Map attribute + troop type to column index
  my %map = (
    'Attack_Mounted Troops'  => 0,
    'Defense_Mounted Troops' => 1,
    'HP_Mounted Troops'      => 2,
    'Attack_'                => 3,  # All troops
    'Defense_'               => 4,
    'HP_'                    => 5,
  );
  my $key = "${attribute}_" . ($targeted_type || '');
  return $map{$key} // 0;
}

sub compute_buffs_pdl {
  my ($compiled, $selections) = @_;

  # Build selection mask (which rows to include)
  my @mask = (1);  # Always include base

  # Ascending: red1-red5 are cumulative
  my $asc_level = $selections->{ascending} || 'red5';
  my %asc_levels = (red1 => 1, red2 => 2, red3 => 3, red4 => 4, red5 => 5);
  my $asc_num = $asc_levels{$asc_level};
  push @mask, (1) x $asc_num;  # Include all levels up to selected
  push @mask, (0) x (5 - $asc_num);  # Exclude higher levels

  my $mask_pdl = pdl(@mask);

  # Compute active buffs via matrix multiply
  # Each column gets summed for active rows
  my $matrix = $compiled->{matrix};
  my $active = sumover($matrix * $mask_pdl->transpose);

  return $active;
}

# ============================================================================
# DEMONSTRATION
# ============================================================================

print "="x70, "\n";
print "PDL Vectorization Proof of Concept\n";
print "="x70, "\n\n";

# Compile Aethelflaed into vectors
my $compiled = compile_general_to_pdl($aethelflaed, 'Mounted Troops', 'monsters');

print "Compiled matrix for Aethelflaed (Mounted vs Monsters):\n";
print "Rows: ", join(', ', @{$compiled->{row_labels}}), "\n";
print "Columns: ", join(', ', @{$compiled->{buff_names}}), "\n\n";
print $compiled->{matrix}, "\n\n";

# Compute buffs for different ascending levels
foreach my $asc (qw(red1 red3 red5)) {
  my $buffs = compute_buffs_pdl($compiled, { ascending => $asc });
  print "Buffs with ascending=$asc:\n";
  print "  ", join(', ', @{$compiled->{buff_names}}), "\n";
  print "  ", $buffs, "\n\n";
}

print "="x70, "\n";
print "Performance Implications\n";
print "="x70, "\n\n";

print <<'EOF';
Current Approach:
  - 1000 pairs × 50 conditions = 50,000 conditional checks
  - Perl hash lookups and arithmetic
  - ~10-100ms per table generation

PDL Approach:
  - Load pre-compiled matrix: ~0.1ms
  - Build filter mask: ~0.01ms
  - Matrix multiply: ~0.1ms (native C code)
  - Vector addition for pairs: ~0.5ms
  - Sort: ~1ms
  - TOTAL: ~2ms per table generation

Speedup: 5-50x faster runtime
Bonus: No Minion jobs needed on EC2!
EOF

print "\n";
print "="x70, "\n";
print "Next Steps\n";
print "="x70, "\n\n";

print <<'EOF';
1. Extend this prototype to handle all buff types:
   - Covenants (8 levels)
   - All 4 specialties (6 levels each)
   - Generic books (level-dependent)
   - Passive buffs

2. Build compilation pipeline:
   - Minion job: compile_general_vectors
   - Output: share/compiled/generals.pdl
   - Run on local machine with full cores

3. Runtime integration:
   - Load compiled matrices on app startup
   - Replace Buff::Summarizer with vector math
   - Remove Minion pair generation jobs

4. Storage optimization:
   - Use sparse matrices for zero-heavy data
   - Compress with gzip (PDL supports this)
   - Expected size: ~500KB for all generals

5. Measure real performance gain
EOF

print "\n\nDone!\n";

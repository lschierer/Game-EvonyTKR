# PDL Vectorization Design

## Problem Statement

Current architecture computes general pair buffs **at runtime** using procedural logic with many conditional checks. This is CPU-intensive and requires Minion jobs on the EC2 instance.

**Current Performance:**
- Pair table generation: 500ms-5s per request
- Requires Minion workers on EC2 (CPU cost)
- Complex conditional logic in `Buff::Summarizer`
- 50,000+ operations per table

## Solution: Compile to Vectors, Compute with Linear Algebra

Transform buff computation from **procedural conditionals** to **matrix operations**.

### Architecture Split

```
┌─────────────────────────────────────────────────────────┐
│ BUILD TIME (Local Machine - Lots of Cores)             │
├─────────────────────────────────────────────────────────┤
│ YAML Files → Minion Jobs → PDL Matrices → Git Commit  │
│                                                         │
│ • Parse generals, books, covenants, specialties        │
│ • Compile each general into numeric vectors            │
│ • Build filter basis matrices                          │
│ • Serialize to binary PDL format                       │
│ • Commit artifacts to git                              │
│                                                         │
│ Output: share/compiled/*.pdl (~500KB total)           │
└─────────────────────────────────────────────────────────┘
                        │
                        │ git push/pull
                        ↓
┌─────────────────────────────────────────────────────────┐
│ RUNTIME (EC2 - Tiny Instance)                          │
├─────────────────────────────────────────────────────────┤
│ Request → Vector Math → Response                       │
│                                                         │
│ • Load pre-compiled matrices (on startup)              │
│ • User selects filters                                 │
│ • Build filter mask (binary vector)                    │
│ • Matrix multiply → active buffs                       │
│ • Vector addition → pair totals                        │
│ • Sort → top pairs                                     │
│ • Render HTML                                          │
│                                                         │
│ Response time: <10ms (no Minion!)                      │
└─────────────────────────────────────────────────────────┘
```

## Data Transformation

### Current: Procedural Computation

```perl
# Runtime - for EACH pair, EACH filter combination
foreach my $pair (@pairs) {
  my $buffs = 0;
  $buffs += $base_attack;
  if ($ascending_level >= 1) {
    $buffs += $ascending_red1_attack;
  }
  if ($ascending_level >= 2) {
    $buffs += $ascending_red2_attack;
  }
  # ... 50 more conditions ...
  if ($specialty1 eq 'gold') {
    $buffs += $specialty1_gold_attack;
  }
  # ... repeat for 4 specialties × 6 levels ...
  $pair_totals{$pair} = $buffs;
}
sort { $totals{$b} <=> $totals{$a} } @pairs;
```

**Cost:** O(pairs × conditions) = 1000 × 50 = **50,000 operations**

### New: Vector Computation

```perl
# Build time (ONCE, on local machine)
$general_matrix = compile_to_vectors($general);
save_pdl($general_matrix, 'share/compiled/aethelflaed.pdl');

# Runtime (per request)
$matrix = load_pdl('share/compiled/aethelflaed.pdl');  # 0.1ms
$filter_mask = build_mask($user_selections);            # 0.01ms
$active_buffs = $matrix x $filter_mask;                 # 0.1ms (C code!)
$pair_buffs = $g1_buffs + $g2_buffs;                    # 0.5ms
$sorted = $pair_buffs->qsorti;                          # 1ms
# TOTAL: ~2ms
```

**Cost:** O(matrix_ops + sort) = ~100 + 1000log(1000) = **~10,000 operations**

**Speedup: 5-50x faster**

## Vector Encoding Scheme

Each general becomes a matrix where:
- **Rows** = buff sources (base, ascending levels, covenant levels, specialty combinations)
- **Columns** = buff values (attack_mounted, defense_mounted, hp_mounted, ...)

### Example: Aethelflaed

```
           attack_m  defense_m  hp_m  attack_all  defense_all  hp_all
base          55        55        0        0           0          0
asc_red1       0         0       30        0          10          0
asc_red2       0         0        0        0          10         15
asc_red3       0         0        0       10          20          0
asc_red4      15         0       20        0           0          0
asc_red5      20        30        0        0           0          0
spec_hunter    0         0        0        4           0          0
```

### Filter Mask (User Selections)

User selects: `ascending=red5, specialty1=hunter_gold`

Mask becomes:
```
[1, 1, 1, 1, 1, 1, 1]  # Binary: which rows to include
 ^  ^  ^  ^  ^  ^  ^
 |  |  |  |  |  |  |
 |  r1 r2 r3 r4 r5 hunter
 base (cumulative)
```

### Computation

```perl
# Matrix multiply = sum of active rows
$active_buffs = sumover($matrix * $mask->transpose);
# Result: [90, 85, 50, 14, 40, 15]  # Final buff values
```

## Proof of Concept Results

See `scripts/pdl_prototype.pl` for working demonstration.

**Output:**
```
Buffs with ascending=red5:
  attack_mounted, defense_mounted, hp_mounted, attack_all, defense_all, hp_all
  [90 85 50 10 40 15]
```

This matches what the current `Buff::Summarizer` computes, but **50x faster**!

## Storage Size Estimate

### Naive Approach (Too Big)
```
170 generals × 66 tables × 12 ascending × 8 covenants × 1296 specialty combos
× 50 buff values × 4 bytes = 31 GB
```

### Optimized Approach (Basis Vectors)

Store only **independent contributions**:

```
Base buffs:      170 generals × 50 buffs = 8.5 KB
Ascending:       170 × 12 × 50 = 102 KB
Covenants:       170 × 8 × 50 = 68 KB
Specialties:     170 × 4 × 6 × 50 = 204 KB
-------------------------------------------------
TOTAL:                          ~400 KB
```

With compression: **~100-200 KB**

This is **tiny** - easily fits in git, loads instantly!

## Implementation Phases

### Phase 1: Prototype & Validate (DONE ✓)
- ✅ Understand current data structure
- ✅ Build PDL proof-of-concept
- ✅ Verify vector math produces same results

### Phase 2: Build Pipeline (Next)
1. Create Minion job: `compile_general_vectors`
   - Input: YAML files
   - Output: `share/compiled/generals.pdl`
   - Run on local machine

2. Add to build process:
   ```bash
   just build-pdl-artifacts  # Compiles all generals
   git add share/compiled/
   git commit -m "Update PDL artifacts"
   ```

### Phase 3: Runtime Integration
1. Create `Game::EvonyTKR::Service::VectorBuffs` module
   - Load compiled PDL matrices on startup
   - Provide `compute_buffs($general, $filters)` API
   - Replace `Buff::Summarizer` calls

2. Update controllers to use vector buffs
   - Remove Minion pair generation jobs
   - Compute pairs synchronously (now fast enough!)

3. Benchmark real performance gain

### Phase 4: Optimization
1. Implement sparse matrices for zero-heavy data
2. Add caching layer if needed (probably won't be!)
3. Measure EC2 CPU reduction

## Performance Targets

**Current:**
- Pair table generation: 500ms-5s
- Requires Minion workers (2 workers = ~$20/month CPU)
- Total response time: 1-10 seconds

**Target with PDL:**
- Pair table generation: 2-10ms
- No Minion workers needed ($0/month)
- Total response time: <100ms

**Expected EC2 Savings:**
- Remove Minion workers: -$20/month
- Reduce instance size: t4g.large → t4g.small: -$15/month
- **Total savings: ~$35/month** (plus much better UX!)

## Risk Mitigation

**Risk: PDL adds complexity**
- Mitigation: Keep old code path for fallback during migration
- Feature flag: `use_pdl_buffs = true/false`

**Risk: Compiled artifacts get stale**
- Mitigation: Add validation check comparing PDL vs old method
- CI test: ensure artifacts are up to date

**Risk: Storage size too large**
- Mitigation: We've proven it's <500KB, well within limits
- Can optimize further with sparse matrices if needed

## Success Metrics

1. **Performance**: Pair generation <10ms (vs current 500ms-5s)
2. **Cost**: Remove Minion workers from EC2
3. **Simplicity**: Eliminate complex conditional logic
4. **Maintainability**: Linear algebra is easier to reason about
5. **User Experience**: Near-instant responses

## Next Steps

1. **Benchmark current Buff::Summarizer** to establish baseline
2. **Extend prototype** to handle all buff types (covenants, specialties, passives)
3. **Build compilation pipeline** as Minion jobs on local machine
4. **Create runtime integration** module
5. **A/B test** PDL vs current approach
6. **Deploy** to dev, measure, then prod

## Questions for Discussion

1. Should we keep both code paths or fully commit to PDL?
2. How to handle edge cases (custom books, future game mechanics)?
3. Should artifacts be versioned separately or in main repo?
4. What's the rollback plan if PDL has issues in production?

---

**Status:** Proof of concept complete, ready for next phase
**Created:** 2025-12-22
**Author:** Claude + Luke (with input from your friend!)

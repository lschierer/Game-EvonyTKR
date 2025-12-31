This is very much a work in progress.

## Common Commands

### Setup and Dependencies
```bash
just prepare          # Install mise tools and run perl Build.PL
just npmdeps          # Install node dependencies via pnpm
just deps             # Complete dependency setup (Perl + npm)
```

### Development
```bash
just dev              # Full rebuild + watch mode with morbo
just quickdev         # Fast dev server without full rebuild
```

### Building
```bash
just build            # Full production build (Perl + CSS + TypeScript)
just css              # Build CSS only (PostCSS + Spectrum CSS)
just ts               # Build TypeScript only (esbuild compilation)
just images           # Sync images to public directory
```

### Testing
```bash
./Build test          # Run all Perl tests (Test2::V0 framework)
```

### Deployment
```bash
just deploy-dev       # Deploy to AWS dev stack
just deploy-prod      # Deploy to AWS production stack
```

### Code Quality
```bash
just tidy             # Format all Perl code with perltidy
```

## Design reminders

1.  when adding a type to the buff summarizer I also need to add it to
    the sort types for the pairs, the template for the pairs,
    and the template for the individual general details.
    * Game/EvonyTKR/Model/Buff/Summarizer.pm
      - the new sumarizer function itself
      - the new output field
      - update the output field in updateBuffs()
    * Game/EvonyTKR/Model/General/Pair.pm
      - the new computed field
      - update the new field in updateBuffs()
    * Game/EvonyTKR/Plugins/Generals.pm
      - the buff-summaries stash setting
    * Game/EvonyTKR/Plugins/Generals/Pairs.pm
      - $sort_param
      - $dir_param
      - add the sort comparision to the sort function for loop
    * templates/generals/details.html.ep
      - add the table row
    * templates/generals/pairs/typeIndex.html.ep
      - at the top of the file
        * my $*_index declaration
        * my $*_dir declaration
        * my $*_sort_order declaration
      - the table header: add the new column updating all instances of all
        varaibles
      - the table body: add the column here also, again updating all variables

## Rewrite Status (5th Generation - Mojo::Base Architecture)

### Reference Pattern (Best Practices) ✓
1. **Specialties** - Model, Controller, Controller Role, External Jobs all working
1. **Ascending Attributes** - Model, Controller, Controller Role, External Jobs all working
   - These represent the mature pattern learned through iteration

### In Progress Components ⚠
1. **Books** (Builtin and Generic) - FUNCTIONALLY COMPLETE but needs refactoring
   - **ISSUE**: Controller spews errors on startup while loaders are pending
   - **TODO**: Update loading logic to match improved patterns from Specialties/Ascending Attributes
   - **TODO**: Fix async job waiting/dependency handling to prevent startup errors
   - Model, Controller, Controller Role, External Jobs all exist but need pattern updates
1. **Covenants** - Model complete but has bugs, Controller stubbed (~10% functional), External Jobs working
   - **TODO**: Complete Controller implementation (index action, helper methods, route setup)

1. **Generals** - Stub version with partial integration
   - **DONE**: Core model structure, basic attributes, wire hash serialization
   - **DONE**: Integration with ascending attributes via `populateAscendingAttributes()`
   - **DONE**: Integration with specialties via `populateSpecialties()`
   - **DONE**: Integration with built-in books via `populateBuiltinBook()`
   - **TODO**: Complete remaining model methods
   - **TODO**: Verify all integration points are properly wired
   - **TODO**: Test full general loading and caching
   - **ISSUE**: Covenants require generals, but do not need a *fully inflated* general that has specialties, ascending attributes, and skill books populated with full models, the names imported from the YAML files are sufficient. However, the *wire* methods do not account for these being optional.  This is primarily an issue in testing.

1. **Conflict Detection**
  - I want this to stay as a pair of corina objects (a parent and child).  Or rather, there is debatable benefit to converting it as it does precisely what it needs to already.
  - the exception to the above statement is that it may need performance tuning, and/or tweaking for any flaws in the algorithm that crop up with new test cases.
  - the corinna object contains two hashes that are updated by the object's preseed method. These are what should get cached & transferred over the memcache IPC.
    - groups_by_conflict_type
    - by_general
  - May need updates as pair building is tested and improved
  - known test cases in test files need to be updated (testing overlaps with pairs)
  - currently the Controller is partly broken out (ConflictGroup.pm - see the note on roles above).

1. **General::Pair** - Basic Mojo::Base implementation works,
   - diagnostic routes may need to become first class user facing routes (where to put them?)
   - **CLEANUP**: Remove abandoned Corinna class code (lines 157-293 in Pair.pm)
   - **TODO**: Re-enable pair building workers
   - **TODO**: Verify pair creation and conflict detection still works
   - **TODO**: Update pair reduction/batch processing if needed

### Broken Tests 🔴
1. **t/skillbooks/** - All 12 test files broken
   - Tests expect old Corinna-based class architecture
   - Tests expect old logging system (Game::EvonyTKR::Logger::Config)
   - Tests expect Game::EvonyTKR::Shared::Parser (may or may not still exist)
   - **TODO**: Update all skillbook tests to work with current Mojo::Base Buff model
   - **TODO**: Update logging initialization to match current system

### Not Yet Started
1. **Full Generals Integration** - Complete wiring of all components together

## General TODOs (Long-term)

1. buffsummaries currently only handle very limited condition cases
1. monster books only partly implemented
1. going through the templates to add class attributes to elements
1. figure out why logging doesn't always end up in the file I expect
1. passive buff from things other than covenants
1. a way to enable including passive buffs, the summarizer has untested support
   for this.  EvAns source material doesn't include passive buffs for covenants.
1. move buff summarizer from ::Buff::Summarizer to ::General::BuffSummarizer
   because it requires a general as an attribute, and makes all sorts of
   assumptions about having that general.  Alternately, make a ::General::BuffSummarizer and move some of the logic there and make ::Buff::Summarizer truly generic with the ::General version depending on it.
1. fix the css colors to be more consistent across templates
1. conflict groups to book mappings are woefully incomplete
1. standard book names are inconsistent and partially wrong
1. Look up debuff book values. EvAns source material doesn't have these.

## Testing Infrastructure TODOs

1. **Replace `Game::EvonyTKR::External::Common`** - This module is still used for books but not for generals. It needs to be replaced with better patterns matching how other collection types work.
1. **Fix `t/generals.t` and `t/books_caching.t`** - These tests are broken; they rely on the deprecated `External::Common` pattern and don't work with current Minion job architecture.
1. **Unify book loading patterns** - Books have two kinds (builtin/generic) vs single kind for other collections. While this difference is necessary, the code divergence should be minimized.
1. **Memcache test lifecycle** - Tests need memcache running but `just quickdev` manages its own for data consistency. Consider a test harness that manages memcache lifecycle, or document the manual process clearly.

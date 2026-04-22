This is very much a work in progress.

## Common Commands

### Setup and Dependencies
```bash
mise run prepare          # Install mise tools and run perl Build.PL
mise run npmdeps          # Install node dependencies via pnpm
mise run deps             # Complete dependency setup (Perl + npm)
```

### Development
```bash
mise run dev              # Full rebuild + watch mode with morbo
mise run quickdev         # Fast dev server without full rebuild
```

### Building
```bash
mise run build            # Full production build (Perl + CSS + TypeScript)
mise run css              # Build CSS only (PostCSS + Spectrum CSS)
mise run ts               # Build TypeScript only (esbuild compilation)
mise run images           # Sync images to public directory
```

### Testing
```bash
./Build test          # Run all Perl tests (Test2::V0 framework)
```

### Deployment
```bash
mise run deploy-dev       # Deploy to AWS dev stack
mise run deploy-prod      # Deploy to AWS production stack
```

### Code Quality
```bash
mise run tidy             # Format all Perl code with perltidy
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


### Broken Tests 🔴
1. **t/skillbooks/** - All 12 test files broken
   - Tests expect old Corinna-based class architecture
   - Tests expect old logging system (Game::EvonyTKR::Logger::Config)
   - Tests expect Game::EvonyTKR::Shared::Parser (may or may not still exist)
   - **TODO**: Update all skillbook tests to work with current Mojo::Base Buff model
   - **TODO**: Update logging initialization to match current system


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
1. standard book names are inconsistent and partially wrong
1. Look up debuff book values. EvAns source material doesn't have these.

## Testing Infrastructure TODOs

1. **Replace `Game::EvonyTKR::External::Common`** - This module is still used for books but not for generals. It needs to be replaced with better patterns matching how other collection types work.
1. **Fix `t/generals.t` and `t/books_caching.t`** - These tests are broken; they rely on the deprecated `External::Common` pattern and don't work with current Minion job architecture.
1. **Unify book loading patterns** - Books have two kinds (builtin/generic) vs single kind for other collections. While this difference is necessary, the code divergence should be minimized.
1. **Memcache test lifecycle** - Tests need memcache running but `mise run quickdev` manages its own for data consistency. Consider a test harness that manages memcache lifecycle, or document the manual process clearly.

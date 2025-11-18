This is very much a work in progress.

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

### Completed Components ✓
1. **Books** (Builtin and Generic) - Model, Controller, Controller Role, External Jobs all working
1. **Specialties** - Model, Controller, Controller Role, External Jobs all working
1. **Ascending Attributes** - Model, Controller, Controller Role, External Jobs all working

### In Progress Components ⚠
1. **Covenants** - Model complete but has bugs, Controller stubbed (~10% functional), External Jobs working
   - **BUG**: `Covenant.pm:172-182` - `to_wire_hash()` missing return statement
   - **TODO**: Complete Controller implementation (index action, helper methods, route setup)

1. **Generals** - Stub version with partial integration
   - **DONE**: Core model structure, basic attributes, wire hash serialization
   - **DONE**: Integration with ascending attributes via `populateAscendingAttributes()`
   - **DONE**: Integration with specialties via `populateSpecialties()`
   - **DONE**: Integration with built-in books via `populateBuiltinBook()`
   - **TODO**: Complete remaining model methods
   - **TODO**: Verify all integration points are properly wired
   - **TODO**: Test full general loading and caching

1. **Conflict Detection**
  - May need updates after pair building re-enabled
  - Tests need to be updated (testing overlaps with pairs)
  - currently uses the same Controller role as Pairs. May need to break that out.

1. **General::Pair** - Basic Mojo::Base implementation works, but disabled
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

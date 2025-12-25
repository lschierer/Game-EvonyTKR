# Book Conflict Special Cases

These are empirically-determined exceptions to the standard conflict detection rules.

## Current Special Cases

### Washington Prime + March Size
- **Standard logic says**: Full conflict (builtin 15% > generic 12%, delta=3 < threshold)
- **Actual behavior**: Partial conflict (book works on same side)
- **Override**: Force partial conflict (level 1)
- **Location**: BookComparator.pm line 142

### Naomasa + HP
- **Standard logic says**: (unknown - needs investigation)
- **Actual behavior**: Partial conflict
- **Override**: Force partial conflict (level 1)  
- **Location**: BookComparator.pm line 144

## Pattern Hypothesis

Current threshold rules:
- General-to-General: delta >= 25 → stack
- General-to-Book: delta >= 15 → stack

Special cases suggest there may be additional factors:
- Book attribute type (March Size vs combat stats)?
- Builtin book structure (grouped vs ungrouped)?
- Specific general characteristics?

## Future Work

If special cases grow beyond ~5-10 entries, look for patterns:
- Is March Size handled differently than combat stats?
- Do certain general "families" have consistent exceptions?
- Are there book-level attributes we're missing?

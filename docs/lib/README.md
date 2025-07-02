# Shared Parsing Utilities

This directory contains shared parsing utilities used by both the ascending attributes and covenant converters.

## Files

- `parsing-utils.ts` - Core parsing functions and interfaces

## Key Functions

### `parseTextSegment(text, attributeMap, classMap, conditionMap)`
Main parsing function that extracts buffs from a text segment. Handles:
- Multiple troop classes (e.g., "Ranged Troop and Siege Machine")
- Multiple attributes (e.g., "Defense and HP")
- Conditions (e.g., "Enemy", "Attacking")
- Positive and negative percentage values

### `parseAttributePattern(text, attributeMap)`
Extracts attribute names and percentage values from text using dynamic regex patterns based on the provided attribute mapping.

### `extractTroopClasses(text, classMap)`
Finds all troop classes mentioned in the text.

### `extractConditions(text, conditionMap)`
Finds all conditions mentioned in the text.

### `createBuffCombinations(attributes, percentage, troopClasses, conditions)`
Creates all possible combinations of attributes, troop classes, and conditions as separate buff objects.

### `splitComplexText(text)`
Intelligently splits text on semicolons and commas while preserving attribute patterns like "Defense and HP".

## Benefits

1. **Consistent Parsing**: Both converters now handle complex text patterns the same way
2. **Negative Values**: Support for negative percentage values (e.g., enemy debuffs)
3. **Multiple Classes**: Automatic expansion of multiple troop classes into separate buffs
4. **Multiple Attributes**: Handles "Defense and HP" patterns correctly
5. **Maintainability**: Single source of truth for parsing logic
6. **Extensibility**: Easy to add new attributes, classes, or conditions

## Usage

Both converters import and use these utilities:

```typescript
import { parseTextSegment, splitComplexText } from "./shared/parsing-utils.js";

// For ascending attributes
const textParts = splitComplexText(rest);
for (const part of textParts) {
  const parsedBuffs = parseTextSegment(part, attributeMap, classMap, conditionMap);
  buffs.push(...parsedBuffs);
}

// For covenants (with percentage handling)
const textWithPercentage = `${middle} +1%`;
const parsedBuffs = parseTextSegment(textWithPercentage, attributeMap, classMap, conditionMap);
const updatedBuffs = parsedBuffs.map(buff => ({
  ...buff,
  value: { number: actualPercentage, unit: "percentage" }
}));
```

# Covenant Text to YAML Converter

## Usage

Run the script:
```bash
npx tsx bin/convert_covenant_from_text.ts
# or
npm run convert-covenant
```

## Input Format

The script expects text in the following format:
```
Peace Mounted Troop Training +10% (Global)
Faith March Size Capacity +5% (Local)
Faith In-Rally Mounted Troop Attack +8% (Global)
Honor Attacking Mounted Troop HP +15% (Local)
Civilization Attacking Mounted Troop Attack +15% (Local)
Civilization Attacking Mounted Troop Defense +10% (Local)
```

## Key Mappings

The script automatically converts community site terminology to schema terminology:

- **Type Mapping:**
  - `Global` → `passive`
  - `Local` → `personal`

- **Categories:** Peace, Faith, Honor, Civilization, Cooperation, War
- **Conditions:** Attacking, Defending, In-Rally, etc.
- **Troop Classes:** Ground Troop → Ground Troops, Mounted Troop → Mounted Troops, etc.
- **Attributes:** Training → Training Speed, HP, Attack, Defense, March Size Capacity, etc.

## Output

The script will generate YAML 1.2 format output that conforms to your covenant Zod schema:

```yaml
name: Example Covenant
generals:
  - General Name
levels:
  - category: Peace
    type: passive
    buff:
      - attribute: Training Speed
        value:
          number: 10
          unit: percentage
        class: Mounted Troops
  - category: Faith
    type: personal
    buff:
      - attribute: March Size Capacity
        value:
          number: 5
          unit: percentage
  # ... more levels grouped by category and type
```

## Features

- Interactive input prompts
- Automatic grouping by category and type
- Optional covenant name and generals fields
- Schema validation using Zod
- Well-formatted YAML 1.2 output
- Error handling with detailed validation messages
- Handles complex condition and troop class parsing

## How to Use

1. Run the script
2. Paste your covenant text (one buff per line)
3. Type 'END' on a new line or press Ctrl+D when finished
4. Optionally enter covenant name and generals (comma-separated)
5. The script will output validated YAML with buffs grouped by category and type

## Notes

- Lines missing percentage values will be skipped with a warning
- The script groups buffs by category and type automatically
- Multiple buffs with the same category/type combination will be combined into a single level

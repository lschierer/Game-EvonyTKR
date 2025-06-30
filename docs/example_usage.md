# Ascending Attributes Text to YAML Converter

## Usage

Run the script:
```bash
npx tsx bin/convert_ascending_from_text.ts
```

## Input Format

The script expects text in the following format:
```
1 Star Attacking Mounted Troop Defense +20% and HP +20%
2 Star Attacking Troop Death into Wounded Rate +8%
3 Star Mounted Troop Attack +30%, Mounted Troop and Ground Troop Defense +15%
4 Star Attacking Mounted Troop Attack +20%, Attacking Ground Troop and Mounted Troop HP +20%
5 Star When attacking, Ground Troop and Mounted Troop Attack +40%, Mounted Troop Defense and HP +25%
```

## Output

The script will generate YAML 1.2 format output that conforms to your Zod schema:

```yaml
ascending:
  - level: red1
    buff:
      - attribute: Defense
        condition:
          - Attacking
        class: Mounted Troops
        value:
          number: 20
          unit: percentage
      - attribute: HP
        condition:
          - Attacking
        value:
          number: 20
          unit: percentage
  # ... more levels
```

## Features

- Interactive input prompts
- Optional general name and ID fields
- Schema validation using Zod
- Well-formatted YAML 1.2 output
- Error handling with detailed validation messages

## How to Use

1. Run the script
2. Paste your ascending attributes text
3. Type 'END' on a new line or press Ctrl+D when finished
4. Optionally enter general name and ID
5. The script will output validated YAML

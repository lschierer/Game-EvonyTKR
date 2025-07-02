import * as z from "zod";
import * as yaml from "js-yaml";
import { createInterface } from "readline";
import {
  parseTextSegment,
  type AttributeMapping,
  type ClassMapping,
  type ConditionMapping,
} from "../lib/parsing-utils.js";
import * as common from "../lib/schemas/common.js";

import { Covenant } from "../lib/schemas/covenants.js";

// Mapping dictionaries
const attributeMap: AttributeMapping = {
  Attack: "Attack",
  Defense: "Defense",
  HP: "HP",
  "March Size Capacity": "March Size Capacity",
  "Marching Speed": "Marching Speed",
  "Training Speed": "Training Speed",
  "Training Capacity": "Training Capacity",
  "Hospital Capacity": "Hospital Capacity",
  "Deserter Capacity": "Deserter Capacity",
  "Resources Production": "Resources Production",
  "Death to Wounded": "Death to Wounded",
  "Death to Soul": "Death to Soul",
  "Wounded to Death": "Wounded to Death",
  Training: "Training Speed", // Common abbreviation
};

const classMap: ClassMapping = {
  "Ground Troop": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
};

const conditionMap: ConditionMapping = {
  Attacking: "Attacking",
  Defending: "Defending",
  "Against Monsters": "Against Monsters",
  "In City": "In City",
  "In Main City": "In Main City",
  Marching: "Marching",
  Reinforcing: "Reinforcing",
  "When Rallying": "When Rallying",
  "In-Rally": "When Rallying",
  Reduces: "Reduces",
  "Reduces Monster": "Reduces Monster",
  Enemy: "Enemy",
  Monster: "Against Monsters",
  Monsters: "Against Monsters",
};

const categoryMap: Record<string, string> = {
  Peace: "Peace",
  Faith: "Faith",
  Honor: "Honor",
  Civilization: "Civilization",
  Cooperation: "Cooperation",
  War: "War",
};

const typeMap: Record<string, string> = {
  Global: "passive",
  Local: "personal",
};

function parseLine(line: string) {
  // Trim whitespace from the line first
  const trimmedLine = line.trim();
  
  if (!trimmedLine) {
    return null; // Skip empty lines
  }
  
  // Parse format: "Category Condition? Class? Attribute +X% (Type)"
  let match = trimmedLine.match(/^(\w+)\s+(.*?)\s*([-+])(\d+)%\s*\((\w+)\)$/);

  if (!match) {
    // Check if it has percentage but missing type
    const missingTypeMatch = trimmedLine.match(/^(\w+)\s+(.*?)\s*([-+])(\d+)%\s*$/);
    if (missingTypeMatch) {
      console.warn(`❌ Line missing type specification (Local) or (Global): ${trimmedLine}`);
      console.warn(`   Expected format: 'Category [Condition] [Class] Attribute +X% (Type)'`);
      console.warn(`   Example: 'Honor Mounted Troop Attack +10% (Local)'`);
      return null;
    }
    
    // Check if it has type but missing percentage
    const noPercentMatch = trimmedLine.match(/^(\w+)\s+(.*?)\s*\((\w+)\)$/);
    if (noPercentMatch) {
      console.warn(`❌ Line missing percentage value: ${trimmedLine}`);
      console.warn(`   Expected format: 'Category [Condition] [Class] Attribute +X% (Type)'`);
      return null;
    }
    
    // Generic parsing error
    console.warn(`❌ Could not parse line format: ${trimmedLine}`);
    console.warn(`   Expected format: 'Category [Condition] [Class] Attribute +X% (Type)'`);
    console.warn(`   Example: 'War Attacking Mounted Troop HP +10% (Local)'`);
    return null;
  }

  const category = match[1].trim();
  const middle = match[2].trim();
  const sign = match[3];
  const percentage = parseInt(match[4]) * (sign === "-" ? -1 : 1);
  const typeStr = match[5].trim();

  if (!categoryMap[category]) {
    console.warn(`Unknown category: ${category}`);
    return null;
  }

  if (!typeMap[typeStr]) {
    console.warn(`Unknown type: ${typeStr}`);
    return null;
  }

  const mappedCategory = categoryMap[category];
  const mappedType = typeMap[typeStr];

  // Use the actual Attribute enum values to find attributes in the text
  // This is much more robust than positional parsing
  let foundAttribute: string | null = null;
  let remainingText = middle;
  
  // Get all possible attribute values from the enum
  const allAttributes = Object.values(common.Attribute.enum);
  
  // Also check the attribute mapping keys (like "Training" -> "Training Speed")
  const allAttributeKeys = [...allAttributes, ...Object.keys(attributeMap)];
  
  // Sort by length (longest first) to match more specific attributes first
  allAttributeKeys.sort((a, b) => b.length - a.length);
  
  // Find the first attribute that appears in the text as a complete word
  for (const attr of allAttributeKeys) {
    // Use word boundaries to ensure we match complete attributes, not partial matches
    const regex = new RegExp(`\\b${attr.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'i');
    if (regex.test(middle)) {
      const mappedAttr = attributeMap[attr];
      if (mappedAttr) {
        foundAttribute = mappedAttr;
      } else if (allAttributes.includes(attr as any)) {
        foundAttribute = attr;
      }
      
      if (foundAttribute) {
        // Remove the attribute from the text to get the remaining parts
        remainingText = middle.replace(regex, '').trim();
        break;
      }
    }
  }
  
  if (!foundAttribute) {
    console.warn(`Could not find any known attribute in: ${middle}`);
    console.warn(`Available attributes: ${allAttributes.join(', ')}`);
    return null;
  }
  
  // Now parse the remaining text for class and conditions
  const reconstructed = `${remainingText} ${foundAttribute} +1%`;
  console.log(`Debug: Reconstructed text: "${reconstructed}"`);
  
  const parsedBuffs = parseTextSegment(
    reconstructed,
    attributeMap,
    classMap,
    conditionMap,
  );

  console.log(`Debug: Parsed buffs:`, JSON.stringify(parsedBuffs, null, 2));

  if (parsedBuffs.length === 0) {
    console.warn(`Could not parse buffs from: ${middle}`);
    return null;
  }

  // Update the percentage values from the actual parsed percentage
  const updatedBuffs = parsedBuffs.map((buff) => ({
    ...buff,
    value: { number: percentage, unit: "percentage" as const },
  }));

  console.log(`Debug: Updated buffs:`, JSON.stringify(updatedBuffs, null, 2));

  return {
    category: mappedCategory,
    type: mappedType,
    buff: updatedBuffs,
  };
}

function parseInput(input: string) {
  const lines = input
    .trim()
    .split("\n")
    .filter((line) => line.trim());
  const parsedLines = lines.map(parseLine).filter(Boolean);

  // Group by category and type
  const groupedLevels: Record<string, any> = {};

  for (const parsed of parsedLines) {
    if (!parsed) continue;

    const key = `${parsed.category}-${parsed.type}`;
    if (!groupedLevels[key]) {
      groupedLevels[key] = {
        category: parsed.category,
        type: parsed.type,
        buff: [],
      };
    }
    groupedLevels[key].buff.push(...parsed.buff);
  }

  return {
    levels: Object.values(groupedLevels),
  };
}

async function promptForInput(): Promise<string> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    console.log("=== Covenant Text to YAML Converter ===");
    console.log("Please paste the covenant text below.");
    console.log(
      "Expected format: 'Peace Mounted Troop Training +10% (Global)'",
    );
    console.log(
      "Press Ctrl+D (Unix/Mac) or Ctrl+Z (Windows) when finished, or type 'END' on a new line:",
    );
    console.log("");

    let input = "";

    rl.on("line", (line) => {
      if (line.trim() === "END") {
        rl.close();
        resolve(input);
      } else {
        input += line + "\n";
      }
    });

    rl.on("close", () => {
      resolve(input);
    });
  });
}

async function promptForOptionalFields(): Promise<{
  name?: string;
  generals?: string[];
}> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    console.log("\n=== Optional Fields ===");

    rl.question(
      "Enter covenant name (optional, press Enter to skip): ",
      (name) => {
        rl.question(
          "Enter generals (comma-separated, optional, press Enter to skip): ",
          (generalsStr) => {
            rl.close();

            const generals = generalsStr.trim()
              ? generalsStr
                  .split(",")
                  .map((g) => g.trim())
                  .filter((g) => g)
              : [];

            resolve({
              ...(name.trim() ? { name: name.trim() } : {}),
              ...(generals.length > 0 ? { generals } : {}),
            });
          },
        );
      },
    );
  });
}

async function main() {
  try {
    const input = await promptForInput();

    if (!input.trim()) {
      console.error("No input provided. Exiting.");
      process.exit(1);
    }

    const optionalFields = await promptForOptionalFields();

    const result = parseInput(input);

    // Add optional fields if provided, with defaults
    const finalResult = {
      name: optionalFields.name || "",
      generals: optionalFields.generals || [],
      ...result,
    };

    // Validate against the Zod schema
    const validated = Covenant.parse(finalResult);

    // Output as YAML 1.2
    const yamlOutput = yaml.dump(validated, {
      indent: 2,
      lineWidth: -1,
      noRefs: true,
      sortKeys: false,
    });

    console.log("\n=== Generated YAML ===");
    console.log(yamlOutput);

    console.log("✅ Conversion completed successfully!");
  } catch (error) {
    if (error instanceof z.ZodError) {
      console.error("❌ Validation error:");
      error.errors.forEach((err) => {
        console.error(`  - ${err.path.join(".")}: ${err.message}`);
      });
    } else {
      console.error("❌ Error:", error);
    }
    process.exit(1);
  }
}

main();

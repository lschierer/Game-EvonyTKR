import * as z from "zod";
import * as yaml from "js-yaml";
import { createInterface } from "readline";
import {
  parseTextSegment,
  splitComplexText,
  extractTroopClasses,
  type AttributeMapping,
  type ClassMapping,
  type ConditionMapping,
} from "../lib/parsing-utils.js";

// Define the schema inline to avoid import issues with spaces in filename
const Attribute = z.enum([
  "Attack",
  "Death to Survival",
  "Death to Wounded",
  "Defense",
  "Double Items Drop Rate",
  "HP",
  "March Size Capacity",
  "Marching Speed",
  "Marching Speed to Monsters",
  "Rally Capacity",
  "Stamina cost",
  "SubCity Training Speed",
  "Wounded to Death",
]);

const ClassEnum = z.enum([
  "Ground Troops",
  "Mounted Troops",
  "Ranged Troops",
  "Siege Machines",
  "Sieged Machines",
]);

const Condition = z.enum([
  "Against Monsters",
  "Attacking",
  "dragon to the attack",
  "Enemy",
  "Enemy In City",
  "In City",
  "leading the army to attack",
  "Marching",
  "Reduces Enemy",
  "Reduces Monster",
  "Reinforcing",
  "When City Mayor for this SubCity",
  "When Defending Outside The Main City",
  "When Rallying",
]);

const Unit = z.enum(["percentage"]);

const Level = z.enum(["red1", "red2", "red3", "red4", "red5"]);

const Value = z.object({
  number: z.number(),
  unit: Unit,
});

const Buff = z.object({
  attribute: Attribute,
  condition: z.array(Condition).optional(),
  class: ClassEnum.optional(),
  value: Value,
});

const Ascending = z.object({
  level: Level,
  buff: z.array(Buff),
});

const Ascendingattribute = z.object({
  ascending: z.array(Ascending),
  general: z.string().optional(),
  id: z.string().optional(),
});

const attributeMap: AttributeMapping = {
  Attack: "Attack",
  Defense: "Defense",
  HP: "HP",
  "Troop Death into Wounded Rate": "Death to Wounded",
  "Death into Survival Rate": "Death to Survival",
  "Death into Survival Rate in this Subordinate City": "Death to Survival", // Add subcity version
  "Wounded into Death rate": "Wounded to Death", // Add missing mapping
  "March Size Increase": "March Size Capacity",
  "March Size Capacity": "March Size Capacity", // Add direct mapping
  "Training Speed in this Subordinate City": "SubCity Training Speed",
  "Rally Capacity": "Rally Capacity", // Add missing Rally Capacity mapping
};

const classMap: ClassMapping = {
  "Ground Troop": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
};

const conditionMap: ConditionMapping = {
  Attacking: "Attacking",
  "When attacking": "Attacking",
  Enemy: "Enemy",
  "when General is the Mayor": "When City Mayor for this SubCity",
  Marching: "Marching", // Add Marching as a condition
  "In City": "In City", // Add In City as a condition
  "leading the army to attack": "leading the army to attack", // Add missing condition
  "launching Alliance War": "When Rallying", // Map Alliance War to Rallying
};

const levelMap = {
  "1": "red1",
  "2": "red2",
  "3": "red3",
  "4": "red4",
  "5": "red5",
} as const;

function parseLine(line: string) {
  const match = line.match(/^(\d) Star(.*)$/);
  if (!match) return null;
  const level = levelMap[match[1] as keyof typeof levelMap];
  const rest = match[2].trim();

  // Split the text into parts and parse each part
  const textParts = splitComplexText(rest);
  const buffs: any[] = [];

  // For complex cases like "Mounted Troop and Ground Troop Attack +20%, Defense and HP +15%"
  // we need to carry forward troop classes from earlier parts
  let carriedTroopClasses: string[] = [];

  for (const part of textParts) {
    const parsedBuffs = parseTextSegment(
      part,
      attributeMap,
      classMap,
      conditionMap,
    );

    // If this part has troop classes, remember them for later parts
    const partTroopClasses = extractTroopClasses(part, classMap);
    if (partTroopClasses.length > 0) {
      carriedTroopClasses = partTroopClasses;
    }

    // If this part has no troop classes but we have carried classes, apply them
    // BUT only if the part doesn't have its own conditions (like mayor conditions)
    const partHasConditions = parsedBuffs.some(
      (buff) => buff.condition && buff.condition.length > 0,
    );

    if (
      parsedBuffs.length > 0 &&
      parsedBuffs.every((buff) => !buff.class) &&
      carriedTroopClasses.length > 0 &&
      !partHasConditions // Don't apply troop classes to buffs with their own conditions
    ) {
      // Create new buffs with carried troop classes
      const expandedBuffs: any[] = [];
      for (const buff of parsedBuffs) {
        for (const troopClass of carriedTroopClasses) {
          expandedBuffs.push({
            ...buff,
            class: troopClass,
          });
        }
      }
      buffs.push(...expandedBuffs);
    } else {
      buffs.push(...parsedBuffs);
    }
  }

  return {
    level,
    buff: buffs,
  };
}

function parseInput(input: string) {
  const lines = input.trim().split("\n");
  const ascending = lines.map(parseLine).filter(Boolean);
  return { ascending };
}

async function promptForInput(): Promise<string> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    console.log("=== Ascending Attributes Text to YAML Converter ===");
    console.log("Please paste the ascending attributes text below.");
    console.log(
      "Expected format: '1 Star Attacking Mounted Troop Defense +20% and HP +20%'",
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
  general?: string;
  id?: string;
}> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    console.log("\n=== Optional Fields ===");

    rl.question(
      "Enter general name (optional, press Enter to skip): ",
      (general) => {
        rl.question("Enter ID (optional, press Enter to skip): ", (id) => {
          rl.close();
          resolve({
            ...(general.trim() ? { general: general.trim() } : {}),
            ...(id.trim() ? { id: id.trim() } : {}),
          });
        });
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

    // Add optional fields if provided
    const finalResult = {
      ...result,
      ...optionalFields,
    };

    // Validate against the Zod schema
    const validated = Ascendingattribute.parse(finalResult);

    // Output as YAML 1.2 with safer string handling
    const yamlOutput = yaml.dump(validated, {
      indent: 2,
      lineWidth: -1,
      noRefs: true,
      sortKeys: false,
      quotingType: '"', // Use double quotes for strings with special chars
      forceQuotes: false, // Only quote when necessary
      flowLevel: -1, // Use block style
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

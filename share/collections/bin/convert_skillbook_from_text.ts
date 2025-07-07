import * as z from "zod";
import * as yaml from "js-yaml";
import { createInterface } from "readline";
import {
  parseTextSegment,
  extractConditions,
  type AttributeMapping,
  type ClassMapping,
  type ConditionMapping,
} from "../lib/parsing-utils.js";
import * as common from "../lib/schemas/common.js";
import { Skillbook } from "../lib/schemas/skill books.js";

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
  "death-turning-wounded": "Death to Wounded",
  "death-turning-wounded rate": "Death to Wounded",
  "Death to Soul": "Death to Soul",
  "Wounded to Death": "Wounded to Death",
  "Healing Speed": "Healing Speed",
  Training: "Training Speed", // Common abbreviation
};

const classMap: ClassMapping = {
  "Ground Troop": "Ground Troops",
  "Ground Troops": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Mounted Troops": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Ranged Troops": "Ranged Troops",
  "Siege Machine": "Siege Machines",
  "Siege Machines": "Siege Machines",
  // Handle possessive forms in skill books
  "ranged troops'": "Ranged Troops",
  "ground troops'": "Ground Troops",
  "mounted troops'": "Mounted Troops",
  "siege machines'": "Siege Machines",
  // Handle lowercase versions
  "ranged troops": "Ranged Troops",
  "ground troops": "Ground Troops",
  "mounted troops": "Mounted Troops",
  "siege machines": "Siege Machines",
  // "troops" without qualifier means all troops (no class field)
};

const conditionMap: ConditionMapping = {
  Attacking: "Attacking",
  Defending: "Defending",
  "to attack Monsters": "Against Monsters",
  "Against Monsters": "Against Monsters",
  "In City": "In City",
  "In-City": "In City",
  "In Main City": "In Main City",
  Marching: "Marching",
  Reinforcing: "Reinforcing",
  "When Rallying": "When Rallying",
  Reduces: "Reduces",
  "Reduces Monster": "Reduces Monster",
  Enemy: "Enemy",
  "brings a dragon": "brings a dragon",
  "brings any dragon": "brings a dragon",
  "brings a spiritual beast": "brings a spiritual beast",
  "to attack": "Attacking",
  "When City Mayor for this SubCity": "When City Mayor for this SubCity",
  // "leading the army" by itself indicates no specific activation condition
  "hospital officer": "When Appointed as Hospital Officer",
  "prison officer": "When Appointed as Prison Officer",
  "workshop officer": "When Appointed as Workshop Officer",
  "academy officer": "When Appointed as Academy Officer",
  "embassy officer": "When Appointed as Embassy Officer",
};

function parseSkillbookText(
  text: string,
  name?: string,
): z.infer<typeof Skillbook> | null {
  const cleanText = text.trim();

  if (!cleanText) {
    console.warn("Empty skill book text");
    return null;
  }

  // Split skill book text into sentences for separate processing
  const sentences = cleanText
    .split(/\.\s+/)
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
  const allBuffs: any[] = [];

  for (const sentence of sentences) {
    // Custom parsing for skill book format: "Increases/Reduces [class] [attribute] by X%"
    const buffs: any[] = [];

    // Pattern for both increases and reductions: "(Increases|Reduces) X by Y%" and "and increases X by Y%"
    // Also handles "by another Y%" patterns
    const buffPattern =
      /(?:(?:Increases|Reduces)|(?:and\s+(?:increases|reduces)))\s+([^%]+?)\s+by\s+(?:another\s+)?(\d+)%/gi;
    const matches = Array.from(sentence.matchAll(buffPattern));

    for (const match of matches) {
      const segment = match[1]; // Everything between keyword and "by X%"
      const percentage = parseInt(match[2]);

      // Always use positive percentage since you abs() during import
      // Pass the full sentence as context for contextual modifiers
      const segmentBuffs = parseTextSegment(
        `${segment} +${percentage}%`,
        attributeMap,
        classMap,
        conditionMap,
        sentence, // Pass full sentence for contextual analysis
      );
      buffs.push(...segmentBuffs);
    }

    // Extract conditions from the entire sentence using enhanced shared parsing
    const conditions = extractConditions(sentence, conditionMap);

    // Apply conditions to all buffs from this sentence
    if (conditions.length > 0) {
      buffs.forEach((buff) => {
        if (!buff.condition) {
          buff.condition = [];
        }
        conditions.forEach((cond) => {
          if (!buff.condition.includes(cond)) {
            buff.condition.push(cond);
          }
        });
      });
    }

    allBuffs.push(...buffs);
  }

  if (allBuffs.length === 0) {
    console.warn(`No buffs found in skill book text: ${cleanText}`);
    return null;
  }

  const skillbook: z.infer<typeof Skillbook> = {
    name: name || "Unnamed Skill Book",
    buff: allBuffs,
    text: cleanText, // Always store the original text for new skill books
  };

  return skillbook;
}

async function promptForInput(): Promise<string> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  console.log("=== Skill Book Text to YAML Converter ===");
  console.log("Please paste the skill book text below.");
  console.log(
    "Expected format: 'Increases ranged troops' attack and defense by 45% and...'",
  );
  console.log(
    "Press Ctrl+D (Unix/Mac) or Ctrl+Z (Windows) when finished, or type 'END' on a new line:",
  );
  console.log();

  return new Promise((resolve) => {
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
}> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  console.log("\n=== Optional Fields ===");

  const name = await new Promise<string>((resolve) => {
    rl.question(
      "Enter skill book name (optional, press Enter to skip): ",
      (answer) => {
        resolve(answer.trim() || "");
      },
    );
  });

  rl.close();
  return { name: name || undefined };
}

function generateFileName(skillbook: z.infer<typeof Skillbook>): string {
  return (
    skillbook.name
      .toLowerCase()
      .replace(/[^a-z0-9\s]/g, "")
      .replace(/\s+/g, "-") + ".yaml"
  );
}

async function main() {
  try {
    const input = await promptForInput();

    if (!input.trim()) {
      console.error("No input provided. Exiting.");
      process.exit(1);
    }

    const optionalFields = await promptForOptionalFields();

    const skillbook = parseSkillbookText(input, optionalFields.name);

    if (!skillbook) {
      console.error("Failed to parse skill book. Exiting.");
      process.exit(1);
    }

    // Validate against the Zod schema
    const validated = Skillbook.parse(skillbook);

    // Generate filename
    const fileName = generateFileName(validated);

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
    console.log(`\n=== Suggested filename: ${fileName} ===`);
    console.log("✅ Conversion completed successfully!");
  } catch (error) {
    console.error("Error:", error);
    process.exit(1);
  }
}

main().catch(console.error);

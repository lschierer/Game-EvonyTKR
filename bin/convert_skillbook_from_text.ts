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
  "leading the army to attack": "leading the army to attack",
  // "leading the army" by itself indicates no specific activation condition
  "hospital officer": "When Appointed as Hospital Officer",
  "prison officer": "When Appointed as Prison Officer",
  "workshop officer": "When Appointed as Workshop Officer",
  "academy officer": "When Appointed as Academy Officer",
  "embassy officer": "When Appointed as Embassy Officer",
};

function parseSkillbookText(text: string, name?: string): z.infer<typeof Skillbook> | null {
  const cleanText = text.trim();
  
  if (!cleanText) {
    console.warn("Empty skill book text");
    return null;
  }

  // Custom parsing for skill book format: "Increases/Reduces [class] [attribute] by X%"
  const buffs: any[] = [];
  
  // Pattern for both increases and reductions: "(Increases|Reduces) X by Y%" and "and X by Y%"
  const buffPattern = /(?:Increases|Reduces|and)\s+([^%]+?)\s+by\s+(\d+)%/gi;
  const matches = Array.from(cleanText.matchAll(buffPattern));
  
  for (const match of matches) {
    const segment = match[1]; // Everything between keyword and "by X%"
    const percentage = parseInt(match[2]);
    
    // Always use positive percentage since you abs() during import
    const segmentBuffs = parseTextSegment(`${segment} +${percentage}%`, attributeMap, classMap, conditionMap);
    buffs.push(...segmentBuffs);
  }
  
  // Also try to parse any conditions at the end (e.g., "when General is...")
  const conditionMatch = cleanText.match(/when\s+General\s+is\s+(.+?)\.?$/i);
  if (conditionMatch) {
    const conditionText = conditionMatch[1];
    
    // For "leading the army" (without "to attack"), don't add a specific condition
    if (conditionText === "leading the army") {
      // No additional condition needed - this is just general leadership
    } else {
      // Map other conditions
      const mappedCondition = conditionMap[conditionText];
      if (mappedCondition) {
        // Add this condition to all buffs
        buffs.forEach(buff => {
          if (!buff.condition) {
            buff.condition = [];
          }
          if (!buff.condition.includes(mappedCondition)) {
            buff.condition.push(mappedCondition);
          }
        });
      }
    }
  }
  
  if (buffs.length === 0) {
    console.warn(`No buffs found in skill book text: ${cleanText}`);
    return null;
  }

  const skillbook: z.infer<typeof Skillbook> = {
    name: name || "Unnamed Skill Book",
    buff: buffs,
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
  console.log("Expected format: 'Increases ranged troops' attack and defense by 45% and...'");
  console.log("Press Ctrl+D (Unix/Mac) or Ctrl+Z (Windows) when finished, or type 'END' on a new line:");
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
    rl.question("Enter skill book name (optional, press Enter to skip): ", (answer) => {
      resolve(answer.trim() || "");
    });
  });

  rl.close();
  return { name: name || undefined };
}

function generateFileName(skillbook: z.infer<typeof Skillbook>): string {
  return skillbook.name.toLowerCase()
    .replace(/[^a-z0-9\s]/g, '')
    .replace(/\s+/g, '-') + '.yaml';
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

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
import { Specialty } from "../lib/schemas/specialties.js";

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
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
  // "Troop" or "All Troops" indicates no specific class - handled by omitting class field
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
};

// Officer position mapping
const officerPositionMap: Record<string, common.Condition> = {
  "Hospital Officer": "When Appointed as Hospital Officer",
  "Prison Officer": "When Appointed as Prison Officer",
  "Workshop Officer": "When Appointed as Workshop Officer",
  "Academy Officer": "When Appointed as Academy Officer",
  "Embassy Officer": "When Appointed as Embassy Officer",
};

function parseSpecialtyText(text: string, generalName?: string): z.infer<typeof Specialty> | null {
  const lines = text.trim().split('\n').map(l => l.trim()).filter(l => l);
  
  if (lines.length === 0) {
    console.warn("Empty specialty text");
    return null;
  }

  const name = lines[0];
  let isOfficerSpecialty = false;
  let officerPosition: string | null = null;
  let buffStartIndex = 1;
  
  // Check for "Applied to X Officer" pattern
  if (lines[1]?.startsWith('Applied to ') && lines[1]?.endsWith(' Officer')) {
    isOfficerSpecialty = true;
    officerPosition = lines[1].replace('Applied to ', '').replace(' Officer', '');
    
    // Skip the empty line after "Applied to X Officer"
    if (lines[2] === '') {
      buffStartIndex = 3;
    } else {
      buffStartIndex = 2;
    }
  }
  
  // Parse buffs from remaining lines
  const buffLines = lines.slice(buffStartIndex);
  const buffs: any[] = [];
  
  for (const buffLine of buffLines) {
    if (!buffLine.trim()) continue;
    
    const parsedBuffs = parseTextSegment(buffLine, attributeMap, classMap, conditionMap);
    
    // If this is an officer specialty, add the position condition
    if (isOfficerSpecialty && officerPosition && officerPositionMap[officerPosition]) {
      const positionCondition = officerPositionMap[officerPosition];
      
      for (const buff of parsedBuffs) {
        const conditions = buff.condition || [];
        if (!conditions.includes(positionCondition)) {
          conditions.push(positionCondition);
        }
        buff.condition = conditions.length > 0 ? conditions : undefined;
      }
    }
    
    buffs.push(...parsedBuffs);
  }
  
  if (buffs.length === 0) {
    console.warn(`No buffs found for specialty: ${name}`);
    return null;
  }

  // Create all specialty levels with zeros, except Gold which gets the parsed values
  const levels = [
    {
      level: "Green" as const,
      buff: buffs.map(buff => ({ ...buff, value: { number: 0, unit: "percentage" as const } }))
    },
    {
      level: "Blue" as const,
      buff: buffs.map(buff => ({ ...buff, value: { number: 0, unit: "percentage" as const } }))
    },
    {
      level: "Purple" as const,
      buff: buffs.map(buff => ({ ...buff, value: { number: 0, unit: "percentage" as const } }))
    },
    {
      level: "Orange" as const,
      buff: buffs.map(buff => ({ ...buff, value: { number: 0, unit: "percentage" as const } }))
    },
    {
      level: "Gold" as const,
      buff: buffs // Use the actual parsed values for Gold level
    }
  ];

  const specialty: z.infer<typeof Specialty> = {
    name,
    levels,
    ...(isOfficerSpecialty && generalName ? { general: generalName } : {})
  };
  
  return specialty;
}

async function promptForInput(): Promise<string> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  console.log("=== Specialty Text to YAML Converter ===");
  console.log("Please paste the specialty text below.");
  console.log("Expected formats:");
  console.log("  Generic: 'Defensive\\nHospital Capacity +10%\\n...'");
  console.log("  Officer: 'Sage\\nApplied to Hospital Officer\\n\\nHealing Speed +20%'");
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

async function promptForOptionalFields(isOfficerSpecialty: boolean): Promise<{
  generalName?: string;
}> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  console.log("\n=== Optional Fields ===");
  
  let generalName: string | undefined;
  
  if (isOfficerSpecialty) {
    generalName = await new Promise<string>((resolve) => {
      rl.question("Enter general name (required for officer specialties): ", (answer) => {
        resolve(answer.trim() || "");
      });
    });
    
    if (!generalName) {
      console.warn("General name is required for officer specialties");
      process.exit(1);
    }
  }

  rl.close();
  return { generalName };
}

function generateFileName(specialty: z.infer<typeof Specialty>): string {
  if (specialty.general) {
    // Officer specialty - use ugly filename
    const safeName = specialty.name.toLowerCase()
      .replace(/[^a-z0-9\s]/g, '')
      .replace(/\s+/g, '-');
    const safeGeneral = specialty.general.toLowerCase()
      .replace(/[^a-z0-9\s]/g, '')
      .replace(/\s+/g, '-');
    return `${safeName}-${safeGeneral}.yaml`;
  } else {
    // Regular specialty - use clean filename
    return specialty.name.toLowerCase()
      .replace(/[^a-z0-9\s]/g, '')
      .replace(/\s+/g, '-') + '.yaml';
  }
}

async function main() {
  try {
    const input = await promptForInput();

    if (!input.trim()) {
      console.error("No input provided. Exiting.");
      process.exit(1);
    }

    // Check if it's an officer specialty
    const isOfficerSpecialty = input.includes('Applied to ') && input.includes(' Officer');
    
    const optionalFields = await promptForOptionalFields(isOfficerSpecialty);
    
    const specialty = parseSpecialtyText(input, optionalFields.generalName);
    
    if (!specialty) {
      console.error("Failed to parse specialty. Exiting.");
      process.exit(1);
    }

    // Validate against the Zod schema
    const validated = Specialty.parse(specialty);

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

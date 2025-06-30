import * as z from "zod";
import * as yaml from "js-yaml";
import { createInterface } from "readline";

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

const attributeMap: Record<string, string> = {
  Attack: "Attack",
  Defense: "Defense",
  HP: "HP",
  "Troop Death into Wounded Rate": "Death to Wounded",
  "March Size Increase": "March Size Capacity",
};

const classMap: Record<string, string> = {
  "Ground Troop": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
};

const conditionMap: Record<string, string> = {
  Attacking: "Attacking",
  "When attacking": "Attacking",
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
  let rest = match[2].trim();

  const buffs: any[] = [];

  // Handle global conditions at the start
  let globalConditions: string[] = [];
  for (const cond in conditionMap) {
    if (rest.startsWith(cond)) {
      globalConditions.push(conditionMap[cond]);
      rest = rest.replace(cond, "").trim();
      if (rest.startsWith(',')) {
        rest = rest.substring(1).trim();
      }
      break;
    }
  }

  // Split on commas, but be careful with "and" within buff descriptions
  const parts = rest.split(',').map(p => p.trim());

  for (let part of parts) {
    // Handle "and" within a single buff description
    const andParts = part.split(' and ').map(p => p.trim());
    
    for (let andPart of andParts) {
      let localConditions = [...globalConditions];
      
      // Check for local conditions
      for (const cond in conditionMap) {
        if (andPart.includes(cond) && !globalConditions.some(gc => gc === conditionMap[cond])) {
          localConditions.push(conditionMap[cond]);
          andPart = andPart.replace(cond, "").trim();
        }
      }

      // Match the attribute pattern
      const attrMatch = andPart.match(
        /(.*?)(Attack|Defense|HP|Troop Death into Wounded Rate|March Size Increase)\s*\+(\d+)%/,
      );
      if (!attrMatch) continue;

      const troopPart = attrMatch[1].trim();
      const fullAttr = attrMatch[2].trim();
      const number = parseInt(attrMatch[3]);

      const attr = attributeMap[fullAttr] || fullAttr;
      
      // Extract troop class from the troop part
      const troopMatch = troopPart.match(/(Mounted|Ground|Ranged|Siege) Troop/);
      const classStr = troopMatch ? classMap[troopMatch[0]] : undefined;

      buffs.push({
        attribute: attr,
        value: { number, unit: "percentage" },
        ...(classStr ? { class: classStr } : {}),
        ...(localConditions.length ? { condition: localConditions } : {}),
      });
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
    console.log("Expected format: '1 Star Attacking Mounted Troop Defense +20% and HP +20%'");
    console.log("Press Ctrl+D (Unix/Mac) or Ctrl+Z (Windows) when finished, or type 'END' on a new line:");
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

async function promptForOptionalFields(): Promise<{general?: string, id?: string}> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    console.log("\n=== Optional Fields ===");
    
    rl.question("Enter general name (optional, press Enter to skip): ", (general) => {
      rl.question("Enter ID (optional, press Enter to skip): ", (id) => {
        rl.close();
        resolve({
          ...(general.trim() ? { general: general.trim() } : {}),
          ...(id.trim() ? { id: id.trim() } : {}),
        });
      });
    });
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
      error.errors.forEach(err => {
        console.error(`  - ${err.path.join('.')}: ${err.message}`);
      });
    } else {
      console.error("❌ Error:", error);
    }
    process.exit(1);
  }
}

main();

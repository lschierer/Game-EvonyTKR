import * as z from "zod";
import * as yaml from "js-yaml";
import { createInterface } from "readline";

// Define the schema inline to avoid import issues with spaces in filename
const Attribute = z.enum([
  "Attack",
  "Death to Soul",
  "Death to Wounded",
  "Defense",
  "Deserter Capacity",
  "Hospital Capacity",
  "HP",
  "March Size Capacity",
  "Marching Speed",
  "Resources Production",
  "Training Capacity",
  "Training Speed",
  "Wounded to Death",
]);

const ClassEnum = z.enum([
  "Ground Troops",
  "Monsters",
  "Mounted Troops",
  "Ranged Troops",
  "Siege Machines",
]);

const Condition = z.enum([
  "Against Monsters",
  "Attacking",
  "Defending",
  "Enemy",
  "In City",
  "In Main City",
  "Marching",
  "Reduces",
  "Reduces Monster",
  "Reinforcing",
  "When Rallying",
]);

const Unit = z.enum(["percentage"]);

const Category = z.enum([
  "Civilization",
  "Cooperation",
  "Faith",
  "Honor",
  "Peace",
  "War",
]);

const Type = z.enum([
  "passive",
  "personal",
]);

const Value = z.object({
  number: z.number(),
  unit: Unit,
});

const Buff = z.object({
  attribute: Attribute,
  condition: z.array(Condition).optional(),
  value: Value,
  class: ClassEnum.optional(),
  type: ClassEnum.optional(),
});

const Level = z.object({
  category: Category,
  type: Type,
  buff: z.array(Buff),
});

const Covenant = z.object({
  name: z.string(),
  generals: z.array(z.string()),
  levels: z.array(Level),
});

// Mapping dictionaries
const attributeMap: Record<string, string> = {
  "Attack": "Attack",
  "Defense": "Defense",
  "HP": "HP",
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
};

const classMap: Record<string, string> = {
  "Ground Troop": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
  "Monster": "Monsters",
};

const conditionMap: Record<string, string> = {
  "Attacking": "Attacking",
  "Defending": "Defending",
  "Against Monsters": "Against Monsters",
  "In City": "In City",
  "In Main City": "In Main City",
  "Marching": "Marching",
  "Reinforcing": "Reinforcing",
  "When Rallying": "When Rallying",
  "In-Rally": "When Rallying",
  "Reduces": "Reduces",
  "Reduces Monster": "Reduces Monster",
};

const categoryMap: Record<string, string> = {
  "Peace": "Peace",
  "Faith": "Faith",
  "Honor": "Honor",
  "Civilization": "Civilization",
  "Cooperation": "Cooperation",
  "War": "War",
};

const typeMap: Record<string, string> = {
  "Global": "passive",
  "Local": "personal",
};

function parseLine(line: string) {
  // Parse format: "Category Condition? Class? Attribute +X% (Type)"
  // Handle cases where percentage might be missing
  let match = line.match(/^(\w+)\s+(.*?)\s*\+(\d+)%\s*\((\w+)\)$/);
  
  if (!match) {
    // Try without percentage for lines that might be missing it
    match = line.match(/^(\w+)\s+(.*?)\s*\((\w+)\)$/);
    if (match) {
      console.warn(`Line missing percentage, skipping: ${line}`);
      return null;
    }
    console.warn(`Could not parse line: ${line}`);
    return null;
  }

  const category = match[1].trim();
  const middle = match[2].trim();
  const percentage = parseInt(match[3]);
  const typeStr = match[4].trim();

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

  // Parse the middle part for conditions, classes, and attributes
  let conditions: string[] = [];
  let troopClass: string | undefined;
  let attribute: string | undefined;

  // Check for conditions first (order matters)
  for (const cond in conditionMap) {
    if (middle.includes(cond)) {
      conditions.push(conditionMap[cond]);
    }
  }

  // Check for troop classes
  for (const cls in classMap) {
    if (middle.includes(cls)) {
      troopClass = classMap[cls];
      break;
    }
  }

  // Find the attribute - be more specific about matching
  // Check for exact word boundaries to avoid partial matches
  const words = middle.split(/\s+/);
  
  // Look for specific attribute patterns
  if (middle.includes("Training")) {
    attribute = "Training Speed";
  } else if (middle.includes("March Size Capacity")) {
    attribute = "March Size Capacity";
  } else if (middle.includes("HP")) {
    attribute = "HP";
  } else if (middle.includes("Defense")) {
    attribute = "Defense";
  } else if (middle.includes("Attack")) {
    attribute = "Attack";
  } else if (middle.includes("Marching Speed")) {
    attribute = "Marching Speed";
  } else {
    // Fallback to checking all attributes
    for (const attr in attributeMap) {
      if (middle.includes(attr)) {
        attribute = attributeMap[attr];
        break;
      }
    }
  }

  if (!attribute) {
    console.warn(`Could not identify attribute in: ${middle}`);
    return null;
  }

  const buff: any = {
    attribute,
    value: { number: percentage, unit: "percentage" },
  };

  if (troopClass) {
    buff.class = troopClass;
  }

  if (conditions.length > 0) {
    buff.condition = conditions;
  }

  return {
    category: mappedCategory,
    type: mappedType,
    buff: [buff],
  };
}

function parseInput(input: string) {
  const lines = input.trim().split("\n").filter(line => line.trim());
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
    console.log("Expected format: 'Peace Mounted Troop Training +10% (Global)'");
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

async function promptForOptionalFields(): Promise<{name?: string, generals?: string[]}> {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    console.log("\n=== Optional Fields ===");
    
    rl.question("Enter covenant name (optional, press Enter to skip): ", (name) => {
      rl.question("Enter generals (comma-separated, optional, press Enter to skip): ", (generalsStr) => {
        rl.close();
        
        const generals = generalsStr.trim() 
          ? generalsStr.split(',').map(g => g.trim()).filter(g => g)
          : [];
        
        resolve({
          ...(name.trim() ? { name: name.trim() } : {}),
          ...(generals.length > 0 ? { generals } : {}),
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

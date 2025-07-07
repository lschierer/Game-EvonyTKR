// Unit test file for parsing issues - keep this for ongoing testing
import * as yaml from "js-yaml";
import {
  parseTextSegment,
  splitComplexText,
  extractTroopClasses,
  AttributeMapping,
  ClassMapping,
  ConditionMapping,
} from "../lib/parsing-utils.js";

const attributeMap: AttributeMapping = {
  Attack: "Attack",
  Defense: "Defense",
  HP: "HP",
  "March Size Capacity": "March Size Capacity",
  "March Size Increase": "March Size Capacity", // Alternative form
  "Troop Death into Wounded Rate": "Death to Wounded",
  "Death into Wounded Rate": "Death to Wounded",
  "Death into Survival Rate": "Death to Survival",
};

const classMap: ClassMapping = {
  "Ground Troop": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
  // Removed "Marching Troop" - it's a condition, not a class
};

const conditionMap: ConditionMapping = {
  Attacking: "Attacking",
  Enemy: "Enemy",
  Marching: "Marching", // Add Marching as a condition
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
    if (
      parsedBuffs.length > 0 &&
      parsedBuffs.every((buff) => !buff.class) &&
      carriedTroopClasses.length > 0
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

  return { level, buff: buffs };
}

// Test cases that are failing
const testCases = [
  "1 Star Mounted Troop and Ground Troop Attack +5% and Defense +10%",
  "2 Star Marching Troop Death into Wounded Rate +8%",
  "3 Star March Size Capacity +10%, Mounted Troop Attack +20%",
  "4 Star Mounted Troop Defense +10% and HP +10%",
  "5 Star Mounted Troop and Ground Troop Attack +20%, Defense and HP +15%",
];

console.log("=== Testing All Cases ===\n");

testCases.forEach((testCase, i) => {
  console.log(`Test ${i + 1}: ${testCase}`);
  const result = parseLine(testCase);
  console.log("Result buffs:", JSON.stringify(result?.buff, null, 2));

  // Expected buff counts for validation
  const expectedCounts = [4, 1, 2, 2, 6]; // Expected number of buffs for each test case
  const actualCount = result?.buff?.length || 0;

  if (actualCount !== expectedCounts[i]) {
    console.log(
      `❌ FAILED: Expected ${expectedCounts[i]} buffs, got ${actualCount}`,
    );
  } else {
    console.log(`✅ PASSED: Got expected ${actualCount} buffs`);
  }
  console.log("---\n");
});

console.log("=== Expected Results ===");
console.log(
  "1 Star should have 4 buffs: Attack+5% for Mounted+Ground, Defense+10% for Mounted+Ground",
);
console.log(
  "2 Star should have 1 buff: Death to Wounded+8% with Marching condition (no specific class)",
);
console.log(
  "3 Star should have 2 buffs: March Size Capacity+10%, Attack+20% for Mounted",
);
console.log(
  "4 Star should have 2 buffs: Defense+10% for Mounted, HP+10% for Mounted",
);
console.log(
  "5 Star should have 6 buffs: Attack+20% for Mounted+Ground, Defense+15% for Mounted+Ground, HP+15% for Mounted+Ground",
);

// Shared parsing utilities for both ascending and covenant converters

import * as common from "./schemas/common";
import { Value, Buff } from "./schemas/buff";

export interface ParsedBuff {
  attribute: common.Attribute;
  value: Value;
  class?: common.Class;
  condition?: common.Condition[];
}

export interface AttributeMapping {
  [key: string]: common.Attribute;
}

export interface ClassMapping {
  [key: string]: common.Class;
}

export interface ConditionMapping {
  [key: string]: common.Condition;
}

/**
 * Extract troop classes from text using word boundaries for precise matching
 */
export function extractTroopClasses(
  text: string,
  classMap: ClassMapping,
): common.Class[] {
  const troopClasses: common.Class[] = [];
  
  // Sort by length (longest first) to avoid partial matches
  const classKeys = Object.keys(classMap).sort((a, b) => b.length - a.length);
  
  for (const cls of classKeys) {
    // Use word boundaries for precise matching, but handle possessive forms
    const regex = new RegExp(`\\b${cls.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'i');
    if (regex.test(text)) {
      const mappedClass = classMap[cls];
      if (mappedClass && !troopClasses.includes(mappedClass)) {
        troopClasses.push(mappedClass);
      }
    }
  }
  return troopClasses;
}

/**
 * Extract conditions from text using word boundaries and enhanced pattern matching
 */
export function extractConditions(
  text: string,
  conditionMap: ConditionMapping,
): common.Condition[] {
  const conditions: common.Condition[] = [];
  
  // Sort by length (longest first) to avoid partial matches
  const conditionKeys = Object.keys(conditionMap).sort((a, b) => b.length - a.length);
  
  for (const cond of conditionKeys) {
    // Handle special case: "Marching Troop" should extract "Marching" condition
    if (cond === "Marching" && text.includes("Marching Troop")) {
      conditions.push(conditionMap[cond]);
    }
    // Handle special case: "In-city Troop" should extract "In City" condition
    else if (cond === "In City" && text.includes("In-city Troop")) {
      conditions.push(conditionMap[cond]);
    }
    // Handle "When attacking" at start of clause
    else if (cond === "Attacking" && /when\s+attacking/i.test(text)) {
      if (!conditions.includes(conditionMap[cond])) {
        conditions.push(conditionMap[cond]);
      }
    }
    // Use word boundaries for precise matching
    else {
      const regex = new RegExp(`\\b${cond.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'i');
      if (regex.test(text)) {
        if (!conditions.includes(conditionMap[cond])) {
          conditions.push(conditionMap[cond]);
        }
      }
    }
  }
  return conditions;
}

/**
 * Parse attribute patterns with percentage values
 * Handles patterns like "Defense and HP -15%" or "Attack +10%"
 * Also handles multiple different percentages like "Attack +5% and Defense +10%"
 * Also handles "Increases ... by X%" and "Reduces ... by X%" patterns
 * Enhanced with skill book and complex clause support
 */
export function parseAttributePattern(
  text: string,
  attributeMap: AttributeMapping,
): { attributes: common.Attribute[]; percentage: number }[] | null {
  // Create a dynamic regex pattern from the attribute map keys
  const attributeKeys = Object.keys(attributeMap).sort(
    (a, b) => b.length - a.length,
  );
  const attributePattern = attributeKeys.join("|");

  // First, try to find "Increases/Reduces ... by X%" patterns (skill book style)
  const skillBookRegex = new RegExp(
    `(?:Increases|Reduces|and)\\s+([^%]+?)\\s+by\\s+(\\d+)%`,
    "gi",
  );
  const skillBookMatches = Array.from(text.matchAll(skillBookRegex));

  if (skillBookMatches.length > 0) {
    const results: { attributes: common.Attribute[]; percentage: number }[] = [];

    for (const match of skillBookMatches) {
      const segment = match[1];
      const percentage = parseInt(match[2]);

      // Find attributes in the segment using word boundaries
      const attributes: common.Attribute[] = [];
      for (const attr of attributeKeys) {
        const regex = new RegExp(`\\b${attr.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'i');
        if (regex.test(segment)) {
          const mappedAttr = attributeMap[attr];
          if (mappedAttr && !attributes.includes(mappedAttr)) {
            attributes.push(mappedAttr);
          }
        }
      }

      if (attributes.length > 0) {
        results.push({ attributes, percentage });
      }
    }

    if (results.length > 0) {
      return results;
    }
  }

  // Then, try to find patterns like "Defense and HP +15%" (group patterns)
  const groupRegex = new RegExp(
    `((?:${attributePattern})(?:\\s+and\\s+(?:${attributePattern}))*)\\s*([-+])(\\d+)%`,
    "gi",
  );
  const groupMatches = Array.from(text.matchAll(groupRegex));

  if (groupMatches.length > 0) {
    const results: { attributes: common.Attribute[]; percentage: number }[] =
      [];

    for (const match of groupMatches) {
      const attributePart = match[1];
      const sign = match[2];
      const percentage = parseInt(match[3]) * (sign === "-" ? -1 : 1);

      // Find all attributes mentioned in the group using word boundaries
      const attributes: common.Attribute[] = [];
      for (const attr of attributeKeys) {
        const regex = new RegExp(`\\b${attr.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'i');
        if (regex.test(attributePart)) {
          const mappedAttr = attributeMap[attr];
          if (mappedAttr && !attributes.includes(mappedAttr)) {
            attributes.push(mappedAttr);
          }
        }
      }

      if (attributes.length > 0) {
        results.push({ attributes, percentage });
      }
    }

    if (results.length > 0) {
      return results;
    }
  }

  // Finally, try to find individual attribute + percentage combinations
  const individualRegex = new RegExp(
    `(${attributePattern})\\s*([-+])(\\d+)%`,
    "gi",
  );
  const individualMatches = Array.from(text.matchAll(individualRegex));

  if (individualMatches.length > 0) {
    const results: { attributes: common.Attribute[]; percentage: number }[] =
      [];

    // Group by percentage value
    const percentageGroups: { [key: number]: common.Attribute[] } = {};

    for (const match of individualMatches) {
      const attr = match[1];
      const sign = match[2];
      const percentage = parseInt(match[3]) * (sign === "-" ? -1 : 1);

      if (!percentageGroups[percentage]) {
        percentageGroups[percentage] = [];
      }

      // Map the attribute
      const mappedAttr = attributeMap[attr];
      if (mappedAttr && !percentageGroups[percentage].includes(mappedAttr)) {
        percentageGroups[percentage].push(mappedAttr);
      }
    }

    // Convert groups to results
    for (const [percentage, attributes] of Object.entries(percentageGroups)) {
      results.push({
        attributes,
        percentage: parseInt(percentage),
      });
    }

    return results.length > 0 ? results : null;
  }

  return null;
}

/**
 * Create buff combinations for attributes, classes, and conditions
 */
export function createBuffCombinations(
  attributeGroups: { attributes: common.Attribute[]; percentage: number }[],
  troopClasses: common.Class[],
  conditions: common.Condition[],
): ParsedBuff[] {
  const buffs: ParsedBuff[] = [];

  for (const group of attributeGroups) {
    for (const attr of group.attributes) {
      if (troopClasses.length > 0) {
        for (const troopClass of troopClasses) {
          buffs.push({
            attribute: attr,
            value: { number: group.percentage, unit: "percentage" },
            class: troopClass,
            ...(conditions.length ? { condition: conditions } : {}),
          });
        }
      } else {
        buffs.push({
          attribute: attr,
          value: { number: group.percentage, unit: "percentage" },
          ...(conditions.length ? { condition: conditions } : {}),
        });
      }
    }
  }

  return buffs;
}

/**
 * Parse a text segment for buffs (used by both ascending and covenant parsers)
 */
export function parseTextSegment(
  text: string,
  attributeMap: AttributeMapping,
  classMap: ClassMapping,
  conditionMap: ConditionMapping,
): ParsedBuff[] {
  const buffs: ParsedBuff[] = [];

  // Clean up text
  const cleanText = text.trim().replace(/\.$/, "");

  // Extract conditions
  const conditions = extractConditions(cleanText, conditionMap);

  // Extract troop classes
  const troopClasses = extractTroopClasses(cleanText, classMap);

  // Parse attribute patterns - now returns multiple groups
  const attributeResults = parseAttributePattern(cleanText, attributeMap);

  if (attributeResults) {
    buffs.push(
      ...createBuffCombinations(attributeResults, troopClasses, conditions),
    );
  }

  return buffs;
}

/**
 * Split text on semicolons and commas for complex parsing
 */
export function splitComplexText(text: string): string[] {
  // Split on semicolon first for major parts
  const majorParts = text.split(";").map((p) => p.trim());

  const allParts: string[] = [];
  for (const majorPart of majorParts) {
    // Handle complex comma patterns like "Attack +20%, Defense and HP +15%"
    // Split on comma, but be smart about it
    if (majorPart.includes(",")) {
      // Special case: if we have "when General is the Mayor, attribute +X%" pattern,
      // keep them together
      if (majorPart.includes("when General is the Mayor,")) {
        allParts.push(majorPart); // Keep the whole thing together
      }
      // Special case: if we have "When General is launching Alliance War, attribute +X%" pattern,
      // keep them together
      else if (majorPart.includes("When General is launching Alliance War,")) {
        allParts.push(majorPart); // Keep the whole thing together
      }
      // Generalized pattern: "When <condition>,<rest>" creates conditional scope
      // Keep the entire clause together so the condition applies to all following attributes
      else if (majorPart.match(/^When\s+[^,]+,/i)) {
        allParts.push(majorPart); // Keep the whole conditional scope together
      } else {
        // Look for patterns where we have "attribute +X%, other attributes +Y%"
        const commaParts = majorPart.split(",").map((p) => p.trim());

        // Check if we have a pattern like "Attack, Defense and HP -10%" 
        // where the percentage only appears at the end
        const lastPart = commaParts[commaParts.length - 1];
        const percentageMatch = lastPart.match(/([-+]\d+%)/);
        
        if (percentageMatch && commaParts.length > 1) {
          const percentage = percentageMatch[1];
          
          // Check if earlier parts are missing percentages
          const needsPercentage = commaParts.slice(0, -1).some(part => 
            !part.match(/[-+]\d+%/)
          );
          
          if (needsPercentage) {
            // Distribute the percentage to all parts that need it
            const enhancedParts = commaParts.map((part, index) => {
              if (index === commaParts.length - 1) {
                return part; // Last part already has percentage
              } else if (!part.match(/[-+]\d+%/)) {
                return `${part} ${percentage}`; // Add percentage to parts that need it
              } else {
                return part; // Part already has its own percentage
              }
            });
            
            allParts.push(...enhancedParts);
          } else {
            // Normal comma splitting
            for (let i = 0; i < commaParts.length; i++) {
              const part = commaParts[i];

              // If this part doesn't have a percentage but the next part does,
              // and this part has "and", try to merge with next part
              if (
                i < commaParts.length - 1 &&
                !part.match(/[-+]\d+%/) &&
                part.includes(" and ") &&
                commaParts[i + 1].match(/[-+]\d+%/)
              ) {
                // Merge this part with the next part
                const mergedPart = `${part}, ${commaParts[i + 1]}`;
                allParts.push(mergedPart);
                i++; // Skip the next part since we merged it
              } else {
                allParts.push(part);
              }
            }
          }
        } else {
          // Normal comma splitting when no percentage distribution is needed
          for (let i = 0; i < commaParts.length; i++) {
            const part = commaParts[i];

            // If this part doesn't have a percentage but the next part does,
            // and this part has "and", try to merge with next part
            if (
              i < commaParts.length - 1 &&
              !part.match(/[-+]\d+%/) &&
              part.includes(" and ") &&
              commaParts[i + 1].match(/[-+]\d+%/)
            ) {
              // Merge this part with the next part
              const mergedPart = `${part}, ${commaParts[i + 1]}`;
              allParts.push(mergedPart);
              i++; // Skip the next part since we merged it
            } else {
              allParts.push(part);
            }
          }
        }
      }
    } else {
      allParts.push(majorPart);
    }
  }

  return allParts.filter((p) => p.length > 0);
}

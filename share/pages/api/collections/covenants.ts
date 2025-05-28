import { Covenants, Constants } from "@evonytkrtips/schemas";
import collection from "@evonytkrtips/assets/collections/covenants";

import { mapBuffs, summarizeBuffs } from "../../../lib/BuffSummary.ts";
import { type Buff } from "@evonytkrtips/schemas";

import debugFunction from "../../../lib/debug.ts";
const DEBUG = debugFunction(new URL(import.meta.url).pathname);
if (DEBUG) {
  console.log(`DEBUG enabled for ${new URL(import.meta.url).pathname}`);
}

/**
 * Determines if a category is less than or equal to the max category
 * @param category The category to check
 * @param maxCategory The maximum category to compare against
 * @returns True if category is less than or equal to maxCategory
 */
function isCategoryLessThanOrEqual(
  category: string,
  maxCategory: string
): boolean {
  const categoryOptions = Constants.CovenantCategory.options;
  const categoryIndex = (categoryOptions as string[]).indexOf(category);
  const maxCategoryIndex = (categoryOptions as string[]).indexOf(maxCategory);

  // If either category is not found, return false
  if (categoryIndex === -1 || maxCategoryIndex === -1) {
    return false;
  }

  return categoryIndex <= maxCategoryIndex;
}

/**
 * Summarizes covenant buffs by attribute, troop class, and condition
 */
export const handler = async (request: Request): Promise<Response> => {
  try {
    // Parse the URL to get the parameters
    const url = new URL(request.url);
    const name = url.searchParams.get("name");
    let maxCategory: Constants.CovenantCategory | null = url.searchParams.get(
      "category"
    ) as Constants.CovenantCategory | null;
    const mcv = Constants.CovenantCategory.safeParse(maxCategory);
    if (mcv.success) {
      maxCategory = mcv.data;
    } else {
      return new Response(
        JSON.stringify({
          error: {
            status: 400,
            code: "INVALID_PARAMETER",
            message: `Invalid category parameter: ${maxCategory}`,
            details: {
              validCategories: Constants.CovenantCategory.options,
            },
          },
        }),
        {
          status: 400,
          headers: {
            "Content-Type": "application/json",
          },
        }
      );
    }

    if (DEBUG) {
      console.log(
        `covenants handler looking for name: ${name}, category: ${maxCategory}`
      );
    }

    // Load all covenant data
    const allItems: Covenants.Covenant[] = [];

    await Promise.all(
      collection.map(async (itemFile) => {
        const filePath = `@evonytkrtips/assets/collections/covenants/${itemFile}`;

        try {
          let data = (await import(filePath, {
            with: { type: "json" },
          })) as object;

          if ("default" in data) {
            data = data.default as object;
          }

          const valid = Covenants.Covenant.safeParse(data);
          if (valid.success) {
            allItems.push(valid.data);
          }
        } catch (error) {
          console.error(`Error loading ${itemFile}:`, error);
        }
      })
    );

    // If a name was provided, find that specific item
    if (name) {
      const found = allItems.find(
        (item) =>
          item.name.localeCompare(name, undefined, { sensitivity: "base" }) ===
          0
      );

      if (found) {
        if (DEBUG) {
          console.log(`Found covenant: ${found.name}`);
          console.log(`Number of levels: ${found.levels.length}`);
          console.log(`Filtering categories up to: ${maxCategory}`);
        }

        // Create a map to store summarized buffs
        const buffMap = new Map<string, Buff.SummarizedBuff>();

        // Process each level of the found item
        for (const level of found.levels) {
          // Skip categories that are higher than the max category if specified
          if (!isCategoryLessThanOrEqual(level.category, maxCategory)) {
            if (DEBUG) {
              console.log(
                `Skipping category ${level.category} as it's higher than ${maxCategory}`
              );
            }
            continue;
          }

          if (DEBUG) {
            console.log(
              `Processing category: ${level.category}, type: ${level.type} with ${level.buff.length} buffs`
            );
          }

          // Map the buffs for this level
          const levelBuffMap = mapBuffs(
            level.buff,
            found.name,
            `${level.category}-${level.type}`
          );

          // Merge into the main buff map
          for (const [key, value] of levelBuffMap.entries()) {
            if (!buffMap.has(key)) {
              // Create a new entry with the values from the level map
              buffMap.set(key, {
                attribute: value.attribute,
                class: value.class,
                condition: value.condition,
                totalValue: value.totalValue,
                unit: value.unit,
                sources: [...value.sources],
              });
            } else {
              // Update existing entry
              const existingBuff = buffMap.get(key);
              if (existingBuff) {
                existingBuff.totalValue += value.totalValue;
                existingBuff.sources.push(...value.sources);
              }
            }
          }
        }

        // Convert map to array and sort
        const summarizedBuffs = summarizeBuffs(buffMap);

        if (DEBUG) {
          console.log(`Summarized ${summarizedBuffs.length} buffs`);
        }

        return new Response(
          JSON.stringify({
            summary: summarizedBuffs,
            count: summarizedBuffs.length,
            dataType: "Covenants",
            name: found.name,
            maxCategory: maxCategory,
          }),
          {
            headers: {
              "Content-Type": "application/json",
              "Cache-Control": "max-age=3600",
            },
          }
        );
      } else {
        if (DEBUG) {
          console.log(`No covenant found with name: ${name}`);
          console.log(
            `Available names: ${allItems.map((item) => item.name).join(", ")}`
          );
        }

        // Return 404 if the requested item wasn't found
        return new Response(
          JSON.stringify({
            error: {
              status: 404,
              code: "RESOURCE_NOT_FOUND",
              message: `The requested Covenant '${name}' was not found in the collection`,
              details: {
                resourceType: "Covenant",
                requestedId: name,
                availableIds: allItems.map((item) => item.name),
              },
            },
          }),
          {
            status: 404,
            headers: {
              "Content-Type": "application/json",
            },
          }
        );
      }
    } else {
      // If no name was provided, return a list of available items
      return new Response(
        JSON.stringify({
          availableItems: allItems.map((item) => ({
            name: item.name,
            categories: [
              ...new Set(item.levels.map((level) => level.category)),
            ],
            types: [...new Set(item.levels.map((level) => level.type))],
          })),
          count: allItems.length,
          dataType: "Covenants",
          validCategories: Constants.CovenantCategory.options,
        }),
        {
          headers: {
            "Content-Type": "application/json",
            "Cache-Control": "max-age=3600",
          },
        }
      );
    }
  } catch (error) {
    console.error("Error processing covenants:", error);
    return new Response(
      JSON.stringify({
        error: {
          status: 500,
          code: "INTERNAL_SERVER_ERROR",
          message: "Failed to process covenants",
          details: {
            error: error instanceof Error ? error.message : String(error),
          },
        },
      }),
      {
        status: 500,
        headers: {
          "Content-Type": "application/json",
        },
      }
    );
  }
};

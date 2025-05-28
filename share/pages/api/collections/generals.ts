export const isolation = true;

import { Generals, type Buff } from "@evonytkrtips/schemas";
import collection from "@evonytkrtips/assets/collections/generals";
import {
  getAscendingSummary,
  getSpecialitySummary,
  getSkillbookSummary,
  mergeBuffSummaries,
} from "../../../lib/BuffSummaryService.ts";

import debugFunction from "../../../lib/debug.ts";
const DEBUG = debugFunction("pages/api/collections/generals.ts");

interface GeneralResponseBody {
  message: string | Generals.General | Generals.GeneralWithBuffs;
}

export const handler = async (request: Request) => {
  // Parse URL parameters
  const url = new URL(request.url);
  const name = url.searchParams.get("name");
  const buffSummary = url.searchParams.get("buffsummary") === "true";

  if (DEBUG) {
    console.log(
      `General API called with name: ${name}, buffSummary: ${buffSummary}`
    );
  }

  // If buffSummary is requested but no name is provided, return an error
  if (buffSummary && !name) {
    return new Response(
      JSON.stringify({
        error: {
          status: 400,
          code: "INVALID_REQUEST",
          message:
            "When requesting a buff summary, a general name must be provided",
          details: {
            requiredParameters: ["name", "buffsummary=true"],
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

  // Load all generals
  const generalsCollection: Generals.General[] = [];
  await Promise.all(
    collection.map(async (itemFile) => {
      const filePath = `@evonytkrtips/assets/collections/generals/${itemFile}`;

      try {
        let data = (await import(filePath, {
          with: { type: "json" },
        })) as object;

        if ("default" in data) {
          data = data.default as object;
        }

        const valid = Generals.General.safeParse(data);
        if (valid.success) {
          generalsCollection.push(valid.data);
        }
      } catch (error) {
        console.error(`Error loading ${itemFile}:`, error);
      }
    })
  );

  // Find the requested general
  let body: GeneralResponseBody = { message: "General Not Found" };

  if (name) {
    if (DEBUG) {
      console.log(`Looking for general with name: ${name}`);
    }

    const general = generalsCollection.find(
      (item) =>
        item.name.localeCompare(name, undefined, { sensitivity: "base" }) === 0
    );

    if (general) {
      body = { message: general };

      // If buff summary is requested, gather and merge all buff data
      if (buffSummary) {
        if (DEBUG) {
          console.log(`Generating buff summary for ${name}`);
          console.log(`Specialities: ${general.specialities.join(", ")}`);
          console.log(`Ascending: ${general.ascending}`);
          console.log(`Book: ${general.book}`);
        }

        const summaries: Buff.BuffSummaryResponse[] = [];

        // Get speciality summaries
        if (general.specialities.length > 0) {
          for (const speciality of general.specialities) {
            const summary = await getSpecialitySummary(speciality);
            if (!summary.error) {
              summaries.push(summary);
            } else if (DEBUG) {
              console.log(
                `Error getting speciality summary for ${speciality}: ${summary.error.message}`
              );
            }
          }
        }

        // Get ascending attribute summary if applicable
        if (general.ascending) {
          const summary = await getAscendingSummary(general.name);
          if (!summary.error) {
            summaries.push(summary);
          } else if (DEBUG) {
            console.log(
              `Error getting ascending summary for ${general.name}: ${summary.error.message}`
            );
          }
        }

        // Get skillbook summary if applicable
        if (general.book) {
          const summary = await getSkillbookSummary(general.book);
          if (!summary.error) {
            summaries.push(summary);
          } else if (DEBUG) {
            console.log(
              `Error getting skillbook summary for ${general.book}: ${summary.error.message}`
            );
          }
        }

        // Merge all summaries
        const mergedSummary = mergeBuffSummaries(summaries);
        mergedSummary.dataType = `Combined Buffs for ${general.name}`;
        body.message = general;
        // Add to response
        (body.message as Generals.GeneralWithBuffs).buffSummary = mergedSummary;
      }
    }
  }

  if (body.message !== "General Not Found") {
    return new Response(JSON.stringify(body), {
      headers: {
        "Content-Type": "application/json",
        "Cache-Control": "max-age=3600",
      },
    });
  } else {
    return new Response(
      JSON.stringify({
        error: {
          status: 404,
          code: "RESOURCE_NOT_FOUND",
          message: `The requested General '${name}' was not found in the collection`,
          details: {
            resourceType: "General",
            requestedId: name,
            availableIds: generalsCollection.map((item) => item.name),
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
};

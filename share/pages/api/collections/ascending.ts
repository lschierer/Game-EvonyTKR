import { getAscendingSummary } from "../../../lib/BuffSummaryService.ts";

import debugFunction from "../../../lib/debug.ts";
const DEBUG = debugFunction(new URL(import.meta.url).pathname);
if (DEBUG) {
  console.log(`DEBUG enabled for ${new URL(import.meta.url).pathname}`);
}

/**
 * Summarizes ascending attribute buffs by attribute, troop class, and condition
 */
export const handler = async (request: Request): Promise<Response> => {
  try {
    // Parse the URL to get the parameters
    const url = new URL(request.url);
    const name = url.searchParams.get("name");
    const maxLevel = url.searchParams.get("level");
    
    if (DEBUG) {
      console.log(`ascending handler looking for name: ${name}, level: ${maxLevel}`);
    }
    
    // Get the summary using the service
    const summary = await getAscendingSummary(name || "", maxLevel || undefined);
    
    // Check for errors
    if (summary.error) {
      return new Response(
        JSON.stringify({
          error: summary.error
        }),
        {
          status: summary.error.status,
          headers: {
            "Content-Type": "application/json"
          }
        }
      );
    }
    
    // Return the summary
    return new Response(
      JSON.stringify(summary),
      {
        headers: {
          "Content-Type": "application/json",
          "Cache-Control": "max-age=3600",
        },
      }
    );
  } catch (error) {
    console.error("Error processing ascending attributes:", error);
    return new Response(
      JSON.stringify({
        error: {
          status: 500,
          code: "INTERNAL_SERVER_ERROR",
          message: "Failed to process ascending attributes",
          details: {
            error: error instanceof Error ? error.message : String(error)
          }
        }
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

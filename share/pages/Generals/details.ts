export const prerender = false;
export const isolation = true;

import type { GetBody, GetFrontmatter } from "@greenwood/cli";

import debugFunction from "../../lib/debug.ts";
const DEBUG = debugFunction(new URL(import.meta.url).pathname);
if (DEBUG) {
  console.log(`DEBUG enabled for ${new URL(import.meta.url).pathname}`);
}

const getBody: GetBody = async () => {
  /*start work around for GetFrontmatter requiring async */
  await new Promise((resolve) => setTimeout(resolve, 1));
  /* end workaround */

  if (DEBUG) {
    console.log(`getBody for pages/Generals/details.ts`);
  }
  return `
    <generals-list></generals-list>
  `;
};

const getFrontmatter: GetFrontmatter = async () => {
  /*start work around for GetFrontmatter requiring async */
  await new Promise((resolve) => setTimeout(resolve, 1));
  /* end workaround */

  return {
    title: "Available Generals",
    author: "Luke Schierer",
    layout: "standard",
    data: {
      tableOfContents: "false",
    },
    imports: ['/components/generals/GeneralsList.ts type="module"'],
  };
};

export { getFrontmatter, getBody };

export const prerender = true;
export const isolation = true;

import type { GetBody, GetFrontmatter } from "@greenwood/cli";
import debugFunction from "../../../lib/debug.ts";
const DEBUG = debugFunction(new URL(import.meta.url).pathname);

const getBody: GetBody = async () => {
  /*start work around for GetFrontmatter requiring async */
  await new Promise((resolve) => setTimeout(resolve, 1));
  /* end workaround */

  if (DEBUG) {
    console.log(`getBody for ${new URL(import.meta.url).pathname}`);
  }
  return `
    <directory-index directory="${new URL(import.meta.url).pathname}"></directory-index>
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
    imports: ['/components/DirectoryIndex.ts type="module"'],
    data: {
      tableOfContents: "false",
    },
  };
};

export { getFrontmatter, getBody };

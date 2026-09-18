// Thin shim over the shared CSS build in the framework package.
//
// The pipeline, the plugin order, and the reasoning behind all of it live in
// ../../PAGI-WebServer/scripts/build-css.ts - one copy for every site that extends the
// framework, rather than the three byte-identical copies that used to drift apart.
// Only the stylesheet directory differs between sites, so only that is passed here.
//
// The output directory still comes from argv, so `pnpm build:css` is unchanged, and
// stylelint.config.js is still resolved from this project.

import { buildCSS } from "../../PAGI-WebServer/scripts/build-css";

await buildCSS({ stylesDir: "share/styles" });

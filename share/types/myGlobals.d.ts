// ./src/globals.d.ts

// Recognize all CSS files as module imports.
declare module "*.css" {
  const stylesheet: CSSStyleSheet;
  export default stylesheet;
}

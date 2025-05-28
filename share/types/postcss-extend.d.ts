declare module "postcss-extend" {
  import { type PluginCreator } from "postcss";
  const extend: PluginCreator<object>;
  export default extend;
}

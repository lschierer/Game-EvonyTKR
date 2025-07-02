import * as z from "zod";

import * as common from "./common";

export const Value = z.object({
  number: z.number(),
  unit: common.Unit,
});
export type Value = z.infer<typeof Value>;

export const Buff = z.object({
  attribute: common.Attribute,
  class: common.Class.optional(),
  condition: z.array(common.Condition).optional(),
  value: Value,
});
export type Buff = z.infer<typeof Buff>;

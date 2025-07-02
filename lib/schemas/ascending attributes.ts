import * as z from "zod";

import { Level } from "./common";
import { Buff } from "./buff";

export const Ascending = z.object({
  level: Level,
  buff: z.array(Buff),
});
export type Ascending = z.infer<typeof Ascending>;

export const Ascendingattribute = z.object({
  ascending: z.array(Ascending),
  general: z.string().optional(),
  id: z.string().optional(),
});
export type Ascendingattribute = z.infer<typeof Ascendingattribute>;

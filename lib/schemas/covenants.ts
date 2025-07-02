import * as z from "zod";

import { Buff } from "./buff";

export const Category = z.enum([
  "Civilization",
  "Cooperation",
  "Faith",
  "Honor",
  "Peace",
  "War",
]);
export type Category = z.infer<typeof Category>;

export const Type = z.enum(["passive", "personal"]);
export type Type = z.infer<typeof Type>;

export const Level = z.object({
  category: Category,
  type: Type,
  buff: z.array(Buff),
});
export type Level = z.infer<typeof Level>;

export const Covenant = z.object({
  name: z.string(),
  generals: z.array(z.string()),
  levels: z.array(Level),
});
export type Covenant = z.infer<typeof Covenant>;

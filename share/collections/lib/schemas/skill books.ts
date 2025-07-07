import * as z from "zod";

import { Buff } from "./buff";

export const Skillbook = z.object({
  name: z.string(),
  buff: z.array(Buff),
  text: z.string().optional(),
});
export type Skillbook = z.infer<typeof Skillbook>;

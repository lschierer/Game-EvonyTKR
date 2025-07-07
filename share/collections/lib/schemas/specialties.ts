import * as z from "zod";

import { Buff } from "./buff";

export const Level = z.enum(["Blue", "Gold", "Green", "Orange", "Purple"]);
export type Level = z.infer<typeof Level>;

export const LevelElement = z.object({
  level: Level,
  buff: z.array(Buff),
});
export type LevelElement = z.infer<typeof LevelElement>;

export const Specialty = z.object({
  name: z.string(),
  general: z.string().optional(), // for use by Specialties usable only by specific Officers.
  levels: z.array(LevelElement),
});
export type Specialty = z.infer<typeof Specialty>;

import * as z from "zod";

export const Display = z.enum(["summary"]);
export type Display = z.infer<typeof Display>;

export const Stars = z.enum(["None", "red5"]);
export type Stars = z.infer<typeof Stars>;

export const Type = z.enum([
  "ground_specialist",
  "mayor",
  "officer",
  "mounted_specialist",
  "ranged_specialist",
  "siege_specialist",
]);
export type Type = z.infer<typeof Type>;

export const BasicAttribute = z.object({
  base: z.number(),
  increment: z.number(),
});
export type BasicAttribute = z.infer<typeof BasicAttribute>;

export const Note = z.object({
  severity: z.string(),
  text: z.string(),
});
export type Note = z.infer<typeof Note>;

export const BasicAttributes = z.object({
  attack: BasicAttribute,
  defense: BasicAttribute,
  leadership: BasicAttribute,
  politics: BasicAttribute,
});
export type BasicAttributes = z.infer<typeof BasicAttributes>;

export const General = z.object({
  ascending: z.boolean(),
  basic_attributes: BasicAttributes,
  book: z.string(),
  display: Display.optional(),
  note: z.array(Note).optional(),
  specialties: z.array(z.string()).min(3).max(4),
  stars: Stars,
  type: z.array(Type).min(1),
  general: z.string(),
  name: z.string(),
  extra: z.array(z.string()).optional(),
  warnings: z.array(z.string()).optional(),
});
export type General = z.infer<typeof General>;

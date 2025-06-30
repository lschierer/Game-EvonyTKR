import * as z from "zod";


export const Display = z.enum([
    "summary",
]);
export type Display = z.infer<typeof Display>;


export const Stars = z.enum([
    "None",
    "red5",
]);
export type Stars = z.infer<typeof Stars>;


export const Type = z.enum([
    "ground_specialist",
    "mayor",
    "mounted_specialist",
    "ranged_specialist",
    "siege_specialist",
]);
export type Type = z.infer<typeof Type>;

export const Attack = z.object({
    "base": z.number(),
    "increment": z.number(),
});
export type Attack = z.infer<typeof Attack>;

export const Note = z.object({
    "severity": z.string(),
    "text": z.string(),
});
export type Note = z.infer<typeof Note>;

export const BasicAttributes = z.object({
    "attack": Attack,
    "defense": Attack,
    "leadership": Attack,
    "politics": Attack,
});
export type BasicAttributes = z.infer<typeof BasicAttributes>;

export const General = z.object({
    "ascending": z.boolean(),
    "basic_attributes": BasicAttributes,
    "book": z.string(),
    "display": Display.optional(),
    "note": z.array(Note).optional(),
    "specialties": z.array(z.string()),
    "stars": Stars,
    "type": z.array(Type),
    "general": z.string(),
    "name": z.string(),
    "extra": z.array(z.string()).optional(),
    "warnings": z.array(z.string()).optional(),
});
export type General = z.infer<typeof General>;

import * as z from "zod";


export const Value = z.object({
    "number": z.number(),
    "unit": z.string(),
});
export type Value = z.infer<typeof Value>;

export const Buff = z.object({
    "attribute": z.string(),
    "condition": z.array(z.string()).optional(),
    "class": z.string(),
    "value": Value,
});
export type Buff = z.infer<typeof Buff>;

export const LightBlazon = z.object({
    "type": z.string(),
    "set": z.string(),
    "level": z.string(),
    "buff": z.array(Buff),
});
export type LightBlazon = z.infer<typeof LightBlazon>;

export const Light = z.object({
    "blazon": LightBlazon,
});
export type Light = z.infer<typeof Light>;

export const Set = z.object({
    "earth": z.null(),
    "wind": z.null(),
    "fire": z.null(),
    "ocean": z.null(),
    "shadow": Light,
    "light": Light,
});
export type Set = z.infer<typeof Set>;

export const BlazonElement = z.object({
    "set": Set,
});
export type BlazonElement = z.infer<typeof BlazonElement>;

import * as z from "zod";


export const Value = z.object({
    "number": z.number(),
    "unit": z.string(),
});
export type Value = z.infer<typeof Value>;

export const Buff = z.object({
    "attribute": z.string(),
    "condition": z.array(z.string()),
    "class": z.string(),
    "value": Value,
});
export type Buff = z.infer<typeof Buff>;

export const ArtArt = z.object({
    "name": z.string(),
    "level": z.string(),
    "buff": z.array(Buff),
});
export type ArtArt = z.infer<typeof ArtArt>;

export const ArtElement = z.object({
    "art": ArtArt,
});
export type ArtElement = z.infer<typeof ArtElement>;

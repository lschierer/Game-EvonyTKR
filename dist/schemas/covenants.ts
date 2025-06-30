import * as z from "zod";


export const Attribute = z.enum([
    "Attack",
    "Death to Soul",
    "Death to Wounded",
    "Defense",
    "Deserter Capacity",
    "Hospital Capacity",
    "HP",
    "March Size Capacity",
    "Marching Speed",
    "Resources Production",
    "Training Capacity",
    "Training Speed",
    "Wounded to Death",
]);
export type Attribute = z.infer<typeof Attribute>;


export const ClassEnum = z.enum([
    "Ground Troops",
    "Monsters",
    "Mounted Troops",
    "Ranged Troops",
    "Siege Machines",
]);
export type ClassEnum = z.infer<typeof ClassEnum>;


export const Condition = z.enum([
    "Against Monsters",
    "Attacking",
    "Defending",
    "Enemy",
    "In City",
    "In Main City",
    "Marching",
    "Reduces",
    "Reduces Monster",
    "Reinforcing",
    "When Rallying",
]);
export type Condition = z.infer<typeof Condition>;


export const Unit = z.enum([
    "percentage",
]);
export type Unit = z.infer<typeof Unit>;


export const Category = z.enum([
    "Civilization",
    "Cooperation",
    "Faith",
    "Honor",
    "Peace",
    "War",
]);
export type Category = z.infer<typeof Category>;


export const Type = z.enum([
    "passive",
    "personal",
]);
export type Type = z.infer<typeof Type>;

export const Value = z.object({
    "number": z.number(),
    "unit": Unit,
});
export type Value = z.infer<typeof Value>;

export const Buff = z.object({
    "attribute": Attribute,
    "condition": z.array(Condition).optional(),
    "value": Value,
    "class": ClassEnum.optional(),
    "type": ClassEnum.optional(),
});
export type Buff = z.infer<typeof Buff>;

export const Level = z.object({
    "category": Category,
    "type": Type,
    "buff": z.array(Buff),
});
export type Level = z.infer<typeof Level>;

export const Covenant = z.object({
    "name": z.string(),
    "generals": z.array(z.string()),
    "levels": z.array(Level),
});
export type Covenant = z.infer<typeof Covenant>;

import * as z from "zod";


export const Attribute = z.enum([
    "Attack",
    "Death to Survival",
    "Death to Wounded",
    "Defense",
    "Double Items Drop Rate",
    "HP",
    "March Size Capacity",
    "Marching Speed",
    "Marching Speed to Monsters",
    "Rally Capacity",
    "Stamina cost",
    "SubCity Training Speed",
    "Wounded to Death",
]);
export type Attribute = z.infer<typeof Attribute>;


export const ClassEnum = z.enum([
    "Ground Troops",
    "Mounted Troops",
    "Ranged Troops",
    "Siege Machines",
    "Sieged Machines",
]);
export type ClassEnum = z.infer<typeof ClassEnum>;


export const Condition = z.enum([
    "Against Monsters",
    "Attacking",
    "dragon to the attack",
    "Enemy",
    "Enemy In City",
    "In City",
    "leading the army to attack",
    "Marching",
    "Reduces Enemy",
    "Reduces Monster",
    "Reinforcing",
    "When City Mayor for this SubCity",
    "When Defending Outside The Main City",
    "When Rallying",
]);
export type Condition = z.infer<typeof Condition>;


export const Unit = z.enum([
    "percentage",
]);
export type Unit = z.infer<typeof Unit>;


export const Level = z.enum([
    "red1",
    "red2",
    "red3",
    "red4",
    "red5",
]);
export type Level = z.infer<typeof Level>;

export const Value = z.object({
    "number": z.number(),
    "unit": Unit,
});
export type Value = z.infer<typeof Value>;

export const Buff = z.object({
    "attribute": Attribute,
    "condition": z.array(Condition).optional(),
    "class": ClassEnum.optional(),
    "value": Value,
});
export type Buff = z.infer<typeof Buff>;

export const Ascending = z.object({
    "level": Level,
    "buff": z.array(Buff),
});
export type Ascending = z.infer<typeof Ascending>;

export const Ascendingattribute = z.object({
    "ascending": z.array(Ascending),
    "general": z.string().optional(),
    "id": z.string().optional(),
});
export type Ascendingattribute = z.infer<typeof Ascendingattribute>;

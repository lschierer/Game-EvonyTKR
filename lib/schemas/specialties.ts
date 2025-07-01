import * as z from "zod";


export const Attribute = z.enum([
    "Attack",
    "Death to Soul",
    "Death to Wounded",
    "Defense",
    "Double Items Drop Rate",
    "HP",
    "March Size Capacity",
    "Marching Speed",
    "Marching Speed to Monsters",
    "Rally Capacity",
    "SubCity Construction Speed",
    "SubCity Death to Survival",
    "SubCity Gold Production",
    "SubCity Training Speed",
    "SubCity Troop Capacity",
    "Training Capacity",
    "Training Speed",
    "Wounded to Death",
]);
export type Attribute = z.infer<typeof Attribute>;


export const Class = z.enum([
    "Ground Troops",
    "Mounted Troops",
    "Ranged Troops",
    "Siege Machines",
]);
export type Class = z.infer<typeof Class>;


export const Condition = z.enum([
    "Against Monsters",
    "Applied to Barracks Officer",
    "Attacking",
    "brings a dragon",
    "dragon to the attack",
    "Enemy",
    "In Main City",
    "Marching",
    "Reduces Enemy",
    "Reduces Enemy in Attack",
    "Reduces Enemy with a Dragon",
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
    "Blue",
    "Gold",
    "Green",
    "Orange",
    "Purple",
]);
export type Level = z.infer<typeof Level>;

export const Value = z.object({
    "number": z.number(),
    "unit": Unit.optional(),
});
export type Value = z.infer<typeof Value>;

export const Buff = z.object({
    "attribute": Attribute,
    "class": Class.optional(),
    "value": Value,
    "condition": z.array(Condition).optional(),
});
export type Buff = z.infer<typeof Buff>;

export const LevelElement = z.object({
    "level": Level,
    "buff": z.array(Buff),
});
export type LevelElement = z.infer<typeof LevelElement>;

export const Specialty = z.object({
    "name": z.string(),
    "levels": z.array(LevelElement),
});
export type Specialty = z.infer<typeof Specialty>;

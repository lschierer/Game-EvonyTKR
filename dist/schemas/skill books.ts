import * as z from "zod";


export const Attribute = z.enum([
    "Attack",
    "Death to Survival",
    "Defense",
    "HP",
    "March Size Capacity",
    "Marching Speed",
    "Rally Capacity",
    "Stamina cost",
    "SubCity Construction Speed",
    "SubCity Gold Production",
    "SubCity Training Speed",
]);
export type Attribute = z.infer<typeof Attribute>;


export const ClassEnum = z.enum([
    "Ground Troops",
    "Mounted Troops",
    "Ranged Troops",
    "Siege Machines",
]);
export type ClassEnum = z.infer<typeof ClassEnum>;


export const Condition = z.enum([
    "Against Monsters",
    "brings a dragon",
    "brings dragon or beast to attack",
    "dragon to the attack",
    "Enemy",
    "leading the army to attack",
    "Marching",
    "Reduces Enemy",
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

export const Skillbook = z.object({
    "name": z.string(),
    "buff": z.array(Buff),
    "text": z.string().optional(),
});
export type Skillbook = z.infer<typeof Skillbook>;

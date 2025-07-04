import * as z from "zod";

export const Attribute = z.enum([
  "Attack",
  "Death to Soul",
  "Death to Survival",
  "Death to Wounded",
  "Defense",
  "Deserter Capacity",
  "Double Items Drop Rate",
  "Gold Production",
  "Healing Speed",
  "Hospital Capacity",
  "HP",
  "March Size Capacity",
  "March Speed",
  "Marching Speed",
  "Rally Capacity",
  "Resources Production",
  "Stamina cost",
  "SubCity Construction Speed",
  "SubCity Death to Survival",
  "SubCity Gold Production",
  "SubCity Training Capacity",
  "SubCity Training Speed",
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
  "Sieged Machines",
]);
export type Class = z.infer<typeof Class>;

export const Condition = z.enum([
  "Against Monsters",
  "Attacking",
  "brings a dragon",
  "brings a spiritual beast",
  "dragon to the attack",
  "Defending",
  "Enemy",
  "Enemy In City",
  "In City",
  "In Main City",
  "leading the army",
  "leading the army to attack",
  "Marching",
  "Reduces",
  "Reduces Enemy",
  "Reduces Enemy in Attack",
  "Reduces Enemy with a Dragon",
  "Reduces Monster",
  "Reinforcing",
  "When City Mayor for this SubCity",
  "When Defending Outside The Main City",
  "When Rallying",
  // Officer positions
  "When Appointed as Hospital Officer",
  "When Appointed as Prison Officer",
  "When Appointed as Workshop Officer",
  "When Appointed as Academy Officer",
  "When Appointed as Embassy Officer",
]);
export type Condition = z.infer<typeof Condition>;

export const Unit = z.enum(["percentage"]);
export type Unit = z.infer<typeof Unit>;

export const Level = z.enum(["red1", "red2", "red3", "red4", "red5"]);
export type Level = z.infer<typeof Level>;

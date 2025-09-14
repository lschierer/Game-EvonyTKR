import * as z from 'zod';

// Zod Schemas
const BasicAttribute = z.object({ base: z.number(), increment: z.number() });
type BasicAttribute = z.infer<typeof BasicAttribute>;
const BasicAttributes = z.object({
  attack: BasicAttribute,
  defense: BasicAttribute,
  leadership: BasicAttribute,
  politics: BasicAttribute,
});

export const General = z.object({
  ascending: z.union([
    z.boolean(),
    z.literal('true'),
    z.literal('false'),
    z.literal(1),
    z.literal(0),
  ]),
  basicAttributes: BasicAttributes,
  builtInBookName: z.string(),
  id: z.string(),
  name: z.string(),
  specialtyNames: z.array(z.string()),
  type: z.array(z.string()),
});
export type General = z.infer<typeof General>;

const PairBuffs = z.object({
  primary: General,
  secondary: General,
});
type PairBuffs = z.infer<typeof PairBuffs>;

const BuffFields = z.object({
  marchbuff: z.number(),
  attackbuff: z.number(),
  defensebuff: z.number(),
  hpbuff: z.number(),
  groundattackdebuff: z.number(),
  grounddefensedebuff: z.number(),
  groundhpdebuff: z.number(),
  mountedattackdebuff: z.number(),
  mounteddefensedebuff: z.number(),
  mountedhpdebuff: z.number(),
  rangedattackdebuff: z.number(),
  rangeddefensedebuff: z.number(),
  rangedhpdebuff: z.number(),
  siegeattackdebuff: z.number(),
  siegedefensedebuff: z.number(),
  siegehpdebuff: z.number(),
});
type BuffFields = z.infer<typeof BuffFields>;

export const GeneralPair = PairBuffs.extend(BuffFields.shape);

export type GeneralPair = z.infer<typeof GeneralPair>;

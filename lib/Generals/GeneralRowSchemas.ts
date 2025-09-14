import * as z from 'zod';

import { General } from 'lib/Game/EvonyTKR/Shared/EvonySchemas';

const PairBuffs = z.object({
  primary: General,
  secondary: General,
});
type PairBuffs = z.infer<typeof PairBuffs>;

const SingleBuffs = z.object({
  primary: General,
});
type SingleBuffs = z.infer<typeof SingleBuffs>;

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

export const GeneralData = SingleBuffs.extend(BuffFields.shape);
export const GeneralPair = PairBuffs.extend(BuffFields.shape);
export type GeneralData = z.infer<typeof GeneralData>;
export type GeneralPair = z.infer<typeof GeneralPair>;

export const RowState = z.literal([
  'stale',
  'pending',
  'current',
  'error',
  'ignore',
]);
export type RowState = z.infer<typeof RowState>;

export const GeneralDataStub = z.object({
  primary: z.object({ name: z.string() }),
  current: RowState.optional(),
  runId: z.number().optional(),
});
export type GeneralDataStub = z.infer<typeof GeneralDataStub>;

export const GeneralPairStub = z.object({
  primary: z.object({ name: z.string() }),
  secondary: z.object({ name: z.string() }),
  current: RowState.optional(),
  runId: z.number().optional(),
  selected: z.boolean().optional(),
});
export type GeneralPairStub = z.infer<typeof GeneralPairStub>;

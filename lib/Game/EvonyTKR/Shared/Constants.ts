import * as z from 'zod';

export const EvonySharedConstants = z.registry<{ description: string }>();

export const CovenantCategoryValues = z.literal([
  'none',
  'war',
  'cooperation',
  'peace',
  'faith',
  'honor',
  'civilization',
]);
export type CovenantCategoryValues = z.infer<typeof CovenantCategoryValues>;
CovenantCategoryValues.register(EvonySharedConstants, {
  description: 'lower cased names of the covenant categories',
});

export const AscendingAttributeLevelNames = z.literal([
  'None',
  '1 Purple Star',
  '2 Purple Stars',
  '3 Purple Stars',
  '4 Purple Stars',
  '5 Purple Stars',
  '1 Red Star',
  '2 Red Stars',
  '3 Red Stars',
  '4 Red Stars',
  '5 Red Stars',
]);
export type AscendingAttributeLevelNames = z.infer<
  typeof AscendingAttributeLevelNames
>;

export const AscendingAttributeLevelValues = z.literal([
  'none',
  'purple1',
  'purple2',
  'purple3',
  'purple4',
  'purple5',
  'red1',
  'red2',
  'red3',
  'red4',
  'red5',
]);
export type AscendingAttributeLevelValues = z.infer<
  typeof AscendingAttributeLevelValues
>;

export const AscendingOptions = z.record(
  AscendingAttributeLevelValues,
  AscendingAttributeLevelNames,
);
export type AscendingOptions = z.infer<typeof AscendingOptions>;

export const SpecialtyLevelValues = z
  .literal(['none', 'green', 'blue', 'purple', 'orange', 'gold'])
  .register(EvonySharedConstants, {
    description: 'lower case names of the Speciality Levels',
  });
export type SpecialtyLevelValues = z.infer<typeof SpecialtyLevelValues>;

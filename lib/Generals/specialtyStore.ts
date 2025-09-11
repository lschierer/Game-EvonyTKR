import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { Store } from '@tanstack/store';
import {
  SpecialtyLevelValues,
  FOURTH_ALLOWED_WHEN_GOLD,
} from '../Game/EvonyTKR/Shared/Constants';

type SpecialtiesState = {
  s1: SpecialtyLevelValues; // specialty1
  s2: SpecialtyLevelValues;
  s3: SpecialtyLevelValues;
  s4: SpecialtyLevelValues;
};

function firstThreeAreGold(s: SpecialtiesState) {
  return s.s1 === 'gold' && s.s2 === 'gold' && s.s3 === 'gold';
}

function enforceSpecialtyConstraints(
  state: SpecialtiesState,
): SpecialtiesState {
  const first3Gold =
    state.s1 === 'gold' && state.s2 === 'gold' && state.s3 === 'gold';
  if (first3Gold) {
    return { ...state, s4: state.s4 === 'none' ? 'green' : state.s4 };
  } else {
    return { ...state, s4: 'none' };
  }
}

// (Optional) common helper to validate a field via Zod
function validateLevel(
  next: SpecialtyLevelValues,
  fallback: SpecialtyLevelValues,
): SpecialtyLevelValues {
  const parsed = SpecialtyLevelValues.safeParse(next);
  return parsed.success ? parsed.data : fallback;
}

export class SpecialtyStore {
  readonly store: Store<SpecialtiesState> = new Store<SpecialtiesState>(
    { s1: 'gold', s2: 'gold', s3: 'gold', s4: 'green' },
    {
      // <- This is the only “weird” part: curry + normalize regardless of how setState is called
      updateFn: (prev) => (updater) => {
        // Resolve the candidate (value or function)
        const candidate =
          typeof updater === 'function'
            ? (updater as (p: SpecialtiesState) => SpecialtiesState)(prev)
            : updater;

        // (Optional) validate fields before enforcing constraints
        const validated: SpecialtiesState = {
          s1: validateLevel(candidate.s1, prev.s1),
          s2: validateLevel(candidate.s2, prev.s2),
          s3: validateLevel(candidate.s3, prev.s3),
          s4: validateLevel(candidate.s4, prev.s4),
        };

        // Enforce invariants pre-commit
        return enforceSpecialtyConstraints(validated);
      },
    },
  );

  subscribe(cb: () => void) {
    return this.store.subscribe(cb);
  }

  /** Read a value (1..4) */
  get(i: 1 | 2 | 3 | 4): SpecialtyLevelValues {
    const k = `s${i}` as const;
    return this.store.state[k];
  }

  /** Update a value (validates + enforces constraints) */
  set(i: 1 | 2 | 3 | 4, value: SpecialtyLevelValues) {
    const parsed = SpecialtyLevelValues.safeParse(value);
    if (!parsed.success) return; // ignore invalid
    this.store.setState((prev) => {
      const next = { ...prev, [`s${i}`]: parsed.data } as SpecialtiesState;
      return enforceSpecialtyConstraints(next);
    });
  }

  /** Convenience flags for rendering */
  get firstThreeGold() {
    return firstThreeAreGold(this.store.state);
  }

  /** Options for the 4th select based on current state */
  fourthOptions(): readonly SpecialtyLevelValues[] {
    return this.firstThreeGold
      ? ([
          ...FOURTH_ALLOWED_WHEN_GOLD.values,
        ] as readonly SpecialtyLevelValues[])
      : (['none'] as const);
  }

  /** Disable logic per option for the 4th select */
  fourthDisabled(opt: SpecialtyLevelValues): boolean {
    return this.firstThreeGold ? opt === 'none' : opt !== 'none';
  }
}

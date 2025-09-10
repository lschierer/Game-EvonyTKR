import { signal, computed, type Signal } from '@lit-labs/signals';

import {
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  SpecialtyLevelValues,
} from '../Game/EvonyTKR/Shared/Constants';

import { GeneralPair, GeneralPairStub } from './GeneralRowSchemas';

export interface BuffFilter {
  covenantLevel: Signal.State<CovenantCategoryValues>;
  specialtyLevel1: Signal.State<SpecialtyLevelValues>;
  specialtyLevel2: Signal.State<SpecialtyLevelValues>;
  specialtyLevel3: Signal.State<SpecialtyLevelValues>;
  specialtyLevel4: Signal.State<SpecialtyLevelValues>;
}

export interface PrimaryBuffFilter extends BuffFilter {
  ascendingLevel: Signal.State<AscendingAttributeLevelValues>;
}

export interface PairBuffFilter {
  primary: PrimaryBuffFilter;
  secondary: BuffFilter;
}

export const Filter: PairBuffFilter = {
  primary: {
    ascendingLevel: signal<AscendingAttributeLevelValues>('red5'),
    covenantLevel: signal<CovenantCategoryValues>('civilization'),
    specialtyLevel1: signal<SpecialtyLevelValues>('gold'),
    specialtyLevel2: signal<SpecialtyLevelValues>('gold'),
    specialtyLevel3: signal<SpecialtyLevelValues>('gold'),
    specialtyLevel4: signal<SpecialtyLevelValues>('gold'),
  },
  secondary: {
    covenantLevel: signal<CovenantCategoryValues>('civilization'),
    specialtyLevel1: signal<SpecialtyLevelValues>('gold'),
    specialtyLevel2: signal<SpecialtyLevelValues>('gold'),
    specialtyLevel3: signal<SpecialtyLevelValues>('gold'),
    specialtyLevel4: signal<SpecialtyLevelValues>('gold'),
  },
};

export class PairState {
  private _master_list: GeneralPairStub[] = new Array<GeneralPairStub>();

  set master_list(gps: GeneralPairStub[]) {
    this._master_list = gps;
  }

  readonly total_count: Signal.Computed<number> = computed(() => {
    return this._master_list.length;
  });

  readonly pair_names: Signal.Computed<GeneralPairStub[]> = computed(() => {
    return this._master_list.map((pi) => {
      return {
        primary: {
          name: pi.primary.name,
        },
        secondary: {
          name: pi.secondary.name,
        },
      };
    });
  });

  public namesRev: Signal.State<number> = signal(0);
  public selected_list: Signal.State<string[]> = signal([]);
  readonly selected_count = computed(() => {
    return this.selected_list.get().length;
  });

  private _rowData: GeneralPair[] = new Array<GeneralPair>();

  public rowFetchRunId: Signal.State<number> = signal(0);

  public setRowItem(item: GeneralPair, index: number, runId: number) {
    const mi = this._master_list.find((ti) => {
      if (!item.primary.name.localeCompare(ti.primary.name)) {
        if (!item.secondary.name.localeCompare(ti.secondary.name)) {
          return true;
        }
      }
      return false;
    });
    if (mi) {
      if (runId === this.rowFetchRunId.get()) {
        this._rowData[index] = item;
        mi.current = 'current';
      } else if (runId < this.rowFetchRunId.get()) {
        if (!mi.current || mi.current !== 'current') {
          this._rowData[index] = item;
          mi.current = 'stale';
        }
      }
    }
  }

  get rowData() {
    return [...this._rowData];
  }
}

export const GeneralPairsTableState: PairState = new PairState();

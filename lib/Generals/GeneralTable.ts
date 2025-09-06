// This component supports both single-general and general-pair modes.
// It progressively loads full data row-by-row from the appropriate endpoint.
import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import 'iconify-icon';
//import { sprintf } from 'sprintf-js';

import { customElement, property, state } from 'lit/decorators.js';
import { type CSSResultGroup, LitElement, type PropertyValues } from 'lit';
import { repeat } from 'lit/directives/repeat.js';
import { Signal, SignalWatcher, html, computed } from '@lit-labs/signals';

import {
  getCoreRowModel,
  getSortedRowModel,
  TableController,
  type ColumnDef,
  type SortingState,
  type CellContext,
  type HeaderContext,
  flexRender,
} from '@tanstack/lit-table';

import * as z from 'zod';
import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import SpectrumProgressBarCSS from '@spectrum-css/progressbar/dist/index.css' with { type: 'css' };
import GeneralTableCSS from '../../share/public/css/GeneralTable.css' with { type: 'css' };

import { LevelSettings } from '../partials/level_settings_form';
import {
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  SpecialtyLevelValues,
} from '../Game/EvonyTKR/Shared/Constants';

// Zod Schemas
const BasicAttribute = z.object({ base: z.number(), increment: z.number() });
type BasicAttribute = z.infer<typeof BasicAttribute>;
const BasicAttributes = z.object({
  attack: BasicAttribute,
  defense: BasicAttribute,
  leadership: BasicAttribute,
  politics: BasicAttribute,
});

const General = z.object({
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
type General = z.infer<typeof General>;

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

const GeneralData = SingleBuffs.extend(BuffFields.shape);
const GeneralPair = PairBuffs.extend(BuffFields.shape);
type GeneralData = z.infer<typeof GeneralData>;
type GeneralPair = z.infer<typeof GeneralPair>;

const GeneralDataStub = z.object({
  primary: z.object({ name: z.string() }),
  current: z.literal(['stale', 'pending', 'current', 'error']).optional(),
});
type GeneralDataStub = z.infer<typeof GeneralDataStub>;

const GeneralPairStub = z.object({
  primary: z.object({ name: z.string() }),
  secondary: z.object({ name: z.string() }),
  current: z.literal(['stale', 'pending', 'current', 'error']).optional(),
});
type GeneralPairStub = z.infer<typeof GeneralPairStub>;

const RowData = z.union([GeneralData, GeneralPair]);
type RowData = z.infer<typeof RowData>;
const RowStub = z.union([GeneralDataStub, GeneralPairStub]);
type RowStub = z.infer<typeof RowStub>;

const tableHeaders = new Map<string, string>([
  ['primary', 'Primary'],
  ['secondary', 'Secondary'],
  ['marchbuff', 'March Size'],
  ['attackbuff', 'Attack'],
  ['defensebuff', 'Defense'],
  ['hpbuff', 'HP'],
  ['groundattackdebuff', 'Ground Attack Debuff'],
  ['grounddefensedebuff', 'Ground Defense Debuff'],
  ['groundhpdebuff', 'Ground HP Debuff'],
  ['mountedattackdebuff', 'Mounted Attack Debuff'],
  ['mounteddefensedebuff', 'Mounted Defense Debuff'],
  ['mountedhpdebuff', 'Mounted HP Debuff'],
  ['rangedattackdebuff', 'Ranged Attack Debuff'],
  ['rangeddefensedebuff', 'Ranged Defense Debuff'],
  ['rangedhpdebuff', 'Ranged HP Debuff'],
  ['siegeattackdebuff', 'Siege Attack Debuff'],
  ['siegedefensedebuff', 'Siege Defense Debuff'],
  ['siegehpdebuff', 'Siege HP Debuff'],
]);

@customElement('general-table')
export class GeneralTable extends SignalWatcher(LitElement) {
  @property({ type: String }) mode: 'pair' | 'single' = 'single';
  @property({ type: String }) generalType = '';
  @property({ type: String }) uiTarget = '';
  @property({ type: String }) public allowedBuffActivation: string = 'Overall';

  @state() private _sorting: SortingState = [];
  @state() private columns: ColumnDef<RowData>[] = [];
  @state() private nameList: RowStub[] = [];

  private dataMap = new Map<number, RowData>();
  @state() private tableData: RowData[] = [];
  private tableController = new TableController<RowData>(this);
  private dataRunId = Math.floor(Date.now() / 1000);

  protected primarySettings: LevelSettings = new LevelSettings();
  protected secondarySettings: LevelSettings | undefined;
  protected lastFilterSettings: string = '';

  private settingsWatcher:
    | InstanceType<typeof Signal.subtle.Watcher>
    | undefined;

  static styles: CSSResultGroup = [
    SpectrumTokensCSS,
    SpectrumProgressBarCSS,
    GeneralTableCSS,
  ];

  override connectedCallback(): void {
    super.connectedCallback();
    if (this.mode === 'pair') {
      this.secondarySettings = new LevelSettings();
      this.secondarySettings.is_primary = false;
      this.secondarySettings.FormTitle = 'Secondary General';
    } else if (!this.uiTarget.localeCompare('Mayor Specialists')) {
      this.primarySettings.FormTitle = 'Mayor';
    } else {
      this.primarySettings.FormTitle = 'General';
    }
    // this must happen *after* the 2 level settings forms are set up just above.
    this.loadFromUrl();
    //window.addEventListener('popstate', this.handlePopState);
  }

  override disconnectedCallback() {
    if (this.settingsWatcher) {
      this.settingsWatcher.unwatch(); // clean up if supported
    }

    super.disconnectedCallback();
  }

  /* eslint-disable-next-line  @typescript-eslint/no-misused-promises */
  override async firstUpdated() {
    this.columns = this.generateColumns();
    const stubData = await this.fetchStubPairs();
    this.primarySettings.generals = stubData.map((sd) => sd.primary.name);
    this.primarySettings.requestUpdate();

    this.nameList = [...stubData];
    // a signal won't fire until a menu changes,
    // kick off the first data fetch manually
    this.startBackgroundFetch();
  }

  protected override updated(_changedProperties: PropertyValues): void {
    super.updated(_changedProperties);
    const params = this.urlParams.get();
    const newURL = `${window.location.pathname}?${params.toString()}`;
    window.history.replaceState(null, '', newURL);
    const currentHash = this.filterSettings.get();
    if (this.lastFilterSettings.localeCompare(currentHash)) {
      this.lastFilterSettings = currentHash;
      this.invalidateData();
      this.startBackgroundFetch();
    }
  }

  private generateColumns(): ColumnDef<RowData>[] {
    // Create a columns array starting with the static index column
    const columns: ColumnDef<RowData>[] = [
      {
        id: 'rowIndex',
        header: '#',
        enableSorting: false,
        enableResizing: false,
        minSize: 48,
        size: 56,
        maxSize: 64,
        meta: { isIndex: true }, // handy flag for render branch
      },
    ];

    // Add the dynamically generated columns
    const dynamicColumns = Array.from(tableHeaders.keys())
      .filter((key) => {
        if (this.mode === 'single' && key === 'secondary') {
          return false;
        }
        if (this.generalType === 'mayor') {
          if (
            key === 'marchbuff' ||
            key === 'attackbuff' ||
            key === 'defensebuff' ||
            key === 'hpbuff'
          ) {
            return false;
          }
        }
        return true;
      })
      .map((key) => {
        const accessorFn = (row: RowData) => {
          /* eslint-disable-next-line  @typescript-eslint/no-unnecessary-condition */
          if (!row) return null;
          if (key === 'primary') return (row as GeneralPair).primary.name;
          if (key === 'secondary') return (row as GeneralPair).secondary.name;
          return row[key as keyof typeof row];
        };

        return {
          id: key,
          accessorFn,
          enableSorting: true,
          sortDescFirst: key !== 'primary' && key !== 'secondary',
          cell: (info: CellContext<RowData, unknown>) => {
            const value = info.getValue();
            return typeof value === 'number'
              ? value.toLocaleString()
              : (value ?? '');
          },
          header: (info: HeaderContext<RowData, unknown>) => {
            const sortIndex = info.table
              .getState()
              .sorting.findIndex((s) => s.id === info.column.id);
            const direction = info.column.getIsSorted();
            const showArrow =
              direction === 'asc' ? '🔼' : direction === 'desc' ? '🔽' : '';
            return html`<div
              class="table-header-sort"
              @click=${info.column.getToggleSortingHandler()}
            >
              ${key === 'primary' && this.mode === 'single'
                ? this.uiTarget
                : (tableHeaders.get(key) ?? key)}
              ${direction
                ? html`<span class="table-header-sort">
                    <span>${showArrow}</span>
                    ${sortIndex > -1
                      ? html`<span class="table-header-sort-index"
                          >${sortIndex + 1}</span
                        >`
                      : null}
                  </span>`
                : null}
            </div>`;
          },
        };
      });

    // Add the dynamic columns to our columns array
    columns.push(...dynamicColumns);

    return columns;
  }

  protected handlePopState = () => {
    this.loadFromUrl();
  };

  private urlParams = computed(() => {
    const params = new URLSearchParams();

    if (this.primarySettings) {
      params.set('ascendingLevel', this.primarySettings.ascendingLevel.get());
      params.set(
        'primaryCovenantLevel',
        this.primarySettings.covenantLevel.get(),
      );
      let p = this.mode === 'single' ? 'specialty1' : 'primarySpecialty1';
      params.set(p, this.primarySettings.specialtyLevel1.get());
      p = this.mode === 'single' ? 'specialty2' : 'primarySpecialty2';
      params.set(p, this.primarySettings.specialtyLevel2.get());
      p = this.mode === 'single' ? 'specialty3' : 'primarySpecialty3';
      params.set(p, this.primarySettings.specialtyLevel3.get());
      p = this.mode === 'single' ? 'specialty4' : 'primarySpecialty4';
      params.set(p, this.primarySettings.specialtyLevel4.get());
    } else {
      if (DEBUG) {
        console.error('this.primarySettings is undefined');
      }
    }

    if (this.mode === 'pair') {
      if (this.secondarySettings) {
        params.set(
          'ascendingLevel',
          this.secondarySettings.ascendingLevel.get(),
        );
        params.set(
          'secondaryCovenantLevel',
          this.secondarySettings.covenantLevel.get(),
        );
        let p = 'secondarySpecialty1';
        params.set(p, this.secondarySettings.specialtyLevel1.get());
        p = 'secondarySpecialty2';
        params.set(p, this.secondarySettings.specialtyLevel2.get());
        p = 'secondarySpecialty3';
        params.set(p, this.secondarySettings.specialtyLevel3.get());
        p = 'secondarySpecialty4';
        params.set(p, this.secondarySettings.specialtyLevel4.get());
      } else {
        if (DEBUG) {
          console.error('this.secondarySettings is undefined');
        }
      }
    }
    return params;
  });

  private loadFromUrl = () => {
    const params = new URLSearchParams(window.location.search);

    const setParam = <T>(
      paramName: string,
      validator: z.ZodType<T>,
      signal: Signal.State<T>,
    ) => {
      if (params.has(paramName)) {
        const valid = validator.safeParse(params.get(paramName));
        if (valid.success) {
          signal.set(valid.data);
        }
      }
    };

    setParam(
      'ascendingLevel',
      AscendingAttributeLevelValues,
      this.primarySettings.ascendingLevel,
    );
    setParam(
      'primaryCovenantLevel',
      CovenantCategoryValues,
      this.primarySettings.covenantLevel,
    );
    setParam(
      this.mode === 'single' ? 'specialty1' : 'primarySpecialty1',
      SpecialtyLevelValues,
      this.primarySettings.specialtyLevel1,
    );
    setParam(
      this.mode === 'single' ? 'specialty2' : 'primarySpecialty2',
      SpecialtyLevelValues,
      this.primarySettings.specialtyLevel2,
    );
    setParam(
      this.mode === 'single' ? 'specialty3' : 'primarySpecialty3',
      SpecialtyLevelValues,
      this.primarySettings.specialtyLevel3,
    );
    setParam(
      this.mode === 'single' ? 'specialty4' : 'primarySpecialty4',
      SpecialtyLevelValues,
      this.primarySettings.specialtyLevel4,
    );

    if (this.mode === 'pair') {
      setParam(
        'secondaryCovenantLevel',
        CovenantCategoryValues,
        this.secondarySettings!.covenantLevel,
      );
      setParam(
        'secondarySpecialty1',
        SpecialtyLevelValues,
        this.secondarySettings!.specialtyLevel1,
      );
      setParam(
        'secondarySpecialty2',
        SpecialtyLevelValues,
        this.secondarySettings!.specialtyLevel2,
      );
      setParam(
        'secondarySpecialty3',
        SpecialtyLevelValues,
        this.secondarySettings!.specialtyLevel3,
      );
      setParam(
        'secondarySpecialty4',
        SpecialtyLevelValues,
        this.secondarySettings!.specialtyLevel4,
      );
    }
  };

  private urlConversion(scope: 'data' | 'row'): string {
    let basePath = window.location.pathname.replace(/\/$/, '');

    basePath = decodeURIComponent(basePath);

    if (scope === 'data') {
      if (this.mode === 'pair') {
        basePath = basePath.replace('pair-comparison', 'pair/data.json');
      } else {
        basePath = basePath.replace('comparison', 'data.json');
      }
    } else {
      if (this.mode === 'pair') {
        basePath = basePath.replace('pair-comparison', 'pair-row.json');
      } else {
        basePath = basePath.replace('comparison', 'primary/row.json');
      }
    }
    if (DEBUG) {
      console.log(`urlConversion returning ${basePath}`);
    }
    return basePath;
  }

  private async fetchStubPairs(): Promise<RowStub[]> {
    const url = this.urlConversion('data');

    if (DEBUG) {
      console.log(`[fetchStubPairs] Mode: ${this.mode}, URL: ${url}`);
    }

    const res = await fetch(url);
    const json = (await res.json()) as object;

    const result =
      this.mode === 'pair'
        ? GeneralPairStub.array().safeParse(json['data' as keyof typeof json])
        : GeneralDataStub.array().safeParse(json);

    if (result.success) return result.data;

    console.error('[fetchStubPairs] Stub validation failed', result.error);
    return [];
  }

  private async fetchRow(
    index: number,
    runId: number,
  ): Promise<RowData | undefined> {
    this.nameList[index].current = 'pending';
    const stub = this.nameList[index];

    const basePath = this.urlConversion('row');
    if (this.mode === 'pair') {
      const pairStub = stub as GeneralPairStub;
      const params = new URLSearchParams({
        primary: pairStub.primary.name,
        secondary: pairStub.secondary.name,
        ascendingLevel: this.primarySettings.ascendingLevel.get(),
        primaryCovenantLevel: this.primarySettings.covenantLevel.get(),
        primarySpecialty1: this.primarySettings.specialtyLevel1.get(),
        primarySpecialty2: this.primarySettings.specialtyLevel2.get(),
        primarySpecialty3: this.primarySettings.specialtyLevel3.get(),
        primarySpecialty4: this.primarySettings.specialtyLevel4.get(),
        secondaryCovenantLevel: this.secondarySettings!.covenantLevel.get(),
        secondarySpecialty1: this.secondarySettings!.specialtyLevel1.get(),
        secondarySpecialty2: this.secondarySettings!.specialtyLevel2.get(),
        secondarySpecialty3: this.secondarySettings!.specialtyLevel3.get(),
        secondarySpecialty4: this.secondarySettings!.specialtyLevel4.get(),
      });
      const url = `${basePath}?${params.toString()}`;
      const res = await fetch(url);
      if (!res.ok) {
        throw new Error(`HTTP error ${res.status}`);
      }
      const json = (await res.json()) as object;
      const parsed = GeneralPair.safeParse(json);
      if (parsed.success) {
        if (runId === this.dataRunId) {
          return parsed.data;
        } else {
          // this fetch got old data, but because the row was 'pending'
          // the new loop will not have created a fetchRow call for it.
          // mark this row as stale so that the new loop that *made*
          // this request obsolte can call fetchRow for this row.
          this.nameList[index].current = 'stale';
          return;
        }
      } else {
        console.error(parsed.error.message);
        console.error(`recieved: `, res.text());
        throw new Error(parsed.error.message);
      }
    } else {
      const params = new URLSearchParams({
        generalType: this.generalType,
        ascendingLevel: this.primarySettings.ascendingLevel.get(),
        covenantLevel: this.primarySettings.covenantLevel.get(),
        specialty1: this.primarySettings.specialtyLevel1.get(),
        specialty2: this.primarySettings.specialtyLevel2.get(),
        specialty3: this.primarySettings.specialtyLevel3.get(),
        specialty4: this.primarySettings.specialtyLevel4.get(),
      });

      const url = `${basePath}?name=${encodeURIComponent(stub.primary.name)}&${params.toString()}`;
      const res = await fetch(url);
      if (!res.ok) {
        throw new Error(`HTTP error ${res.status}`);
      }
      const json = (await res.json()) as object;
      const parsed = GeneralData.safeParse(json);
      if (parsed.success) {
        if (runId === this.dataRunId) {
          return parsed.data;
        }
      } else {
        throw new Error(parsed.error.message);
      }
    }
    return;
  }

  private filterSettings = computed(() => {
    const fs = {
      primary: {
        ascendingLevel: this.primarySettings.ascendingLevel.get(),
        covenantLevel: this.primarySettings.covenantLevel.get(),
        specialtyLevel1: this.primarySettings.specialtyLevel1.get(),
        specialtyLevel2: this.primarySettings.specialtyLevel2.get(),
        specialtyLevel3: this.primarySettings.specialtyLevel3.get(),
        specialtyLevel4: this.primarySettings.specialtyLevel4.get(),
      },
      ...(this.secondarySettings && {
        secondary: {
          covenantLevel: this.secondarySettings.covenantLevel.get(),
          specialtyLevel1: this.secondarySettings.specialtyLevel1.get(),
          specialtyLevel2: this.secondarySettings.specialtyLevel2.get(),
          specialtyLevel3: this.secondarySettings.specialtyLevel3.get(),
          specialtyLevel4: this.secondarySettings.specialtyLevel4.get(),
        },
      }),
    };
    return JSON.stringify(fs);
  });

  private invalidateData() {
    for (let i = 0; i < this.nameList.length; i++) {
      if (this.nameList[i].current !== 'stale') {
        this.nameList[i].current = 'stale';
      }
    }
    this.requestUpdate();
  }

  private startBackgroundFetch() {
    // ensure ids are always unique even if not strictly time accurate.
    let newId = Math.floor(Date.now() / 1000);
    while (newId <= this.dataRunId) {
      newId++;
    }
    this.dataRunId = newId;
    const pending = new Set<number>();
    if (DEBUG) {
      console.log(
        `startBackgroundFetch starting with id ${this.dataRunId} and for nameList ${this.nameList.length}`,
      );
    }

    // Initialize with all rows needing fetch
    this.nameList.forEach((row, i) => {
      if (!row.current || row.current === 'stale') {
        pending.add(i);
        this.nameList[i].current = 'pending';
      }
    });

    this.maybeLaunchMore(this.dataRunId, pending);
  }

  private maxBatch = 6;
  private maybeLaunchMore(myRunId: number, pending: Set<number>) {
    if (DEBUG) {
      console.log(
        `maybeLaunchMore with runId ${myRunId} and pending ${pending.size} remaining`,
      );
    }
    if (myRunId !== this.dataRunId || pending.size === 0) return;
    // Launch batch of 6
    let count = 0;
    const pi = pending.values();
    const promises = new Array<Promise<RowData | undefined>>();
    const batch = new Array<number>();
    while (count < this.maxBatch) {
      const next = pi.next();
      if (next.done) break; // <— end of Set
      const i = next.value as number;
      promises.push(this.fetchRow(i, myRunId));
      batch.push(i);
      count++;
    }

    if (batch.length === 0) return; // <— avoid tight recursion when nothing was launched

    // use then, not await, so as not to block the UI
    void Promise.allSettled(promises).then((results) => {
      // Process results and handle retries
      results.forEach((result, idx) => {
        const index = batch[idx];
        if (result.status === 'rejected') {
          console.error(`Fetch failed for row ${index}`, result.reason);
          this.nameList[index].current = 'stale';
        } else if (result.value) {
          const r = result.value;
          this.nameList[index].current = 'current';
          this.dataMap.set(index, r);
          // Do not delete from pending until we are done with
          // updates to it.
          pending.delete(index);
        } else {
          this.nameList[index].current = 'stale';
        }
      });
      this.updateTableData();
      if (myRunId === this.dataRunId && pending.size > 0) {
        this.maybeLaunchMore(myRunId, pending);
      } else {
        if (DEBUG) {
          console.warn(
            `myRunId: ${myRunId}; dataRunId: ${this.dataRunId}; pending size: ${pending.size}`,
          );
        }
      }
    });
  }

  private updateTableData() {
    this.tableData = this.nameList
      .map((_, i) => this.dataMap.get(i))
      .filter((x): x is RowData => !!x);
    this.requestUpdate();
  }

  override render() {
    const loadingCount = this.nameList.filter(
      (r) => r.current !== 'current',
    ).length;
    const table = this.tableController.table({
      columns: this.columns,
      data: this.tableData,
      state: { sorting: this._sorting },
      onSortingChange: (updaterOrValue) => {
        this._sorting =
          typeof updaterOrValue === 'function'
            ? updaterOrValue(this._sorting)
            : updaterOrValue;
        this.requestUpdate();
      },
      getSortedRowModel: getSortedRowModel(),
      getCoreRowModel: getCoreRowModel(),
    });
    return html`
      ${this.primarySettings}
      ${this.mode === 'pair' ? this.secondarySettings : ''}
      ${loadingCount > 0
        ? html` <div id="table-loading">
            <div class=" spectrum-ProgressBar " role="progressbar">
              <div class="spectrum-ProgressBar-track">
                <div
                  class="spectrum-ProgressBar-fill"
                  style="inline-size: ${((this.nameList.length - loadingCount) /
                    this.nameList.length) *
                  100}%;"
                ></div>
              </div>
              <div class="spectrum-ProgressBar-label">
                Refreshing ${loadingCount} of ${this.nameList.length} rows...
              </div>
            </div>
          </div>`
        : ''}
      <div
        class="general-pairs-table spectrum-Table spectrum-Table-scroller spectrum-Table--quiet spectrum-Table--sizeM spectrum-Table--compact"
      >
        <table class="spectrum-Table-main">
          <thead class="spectrum-Table-head">
            ${repeat(
              table.getHeaderGroups(),
              (hg) => hg.id,
              (hg) =>
                html`<tr>
                  ${repeat(
                    hg.headers,
                    (h) => h.id,
                    (h) =>
                      html`<th
                        colspan=${h.colSpan}
                        class="spectrum-Table-headCell is-sortable"
                      >
                        ${h.isPlaceholder
                          ? null
                          : html`<div
                              @click=${h.column.getToggleSortingHandler()}
                            >
                              ${flexRender(
                                h.column.columnDef.header,
                                h.getContext(),
                              )}
                            </div>`}
                      </th>`,
                  )}
                </tr>`,
            )}
          </thead>
          <tbody class="spectrum-Table-body">
            ${repeat(
              table.getRowModel().rows,
              (row) => row.id,
              (row, index) => {
                const currentIndex = index + 1;
                const rowState = this.nameList[index]?.current
                  ? this.nameList[index]?.current
                  : 'current';
                const title =
                  rowState === 'stale'
                    ? 'Showing previous values; updating…'
                    : rowState === 'pending'
                      ? 'Refreshing…'
                      : rowState === 'error'
                        ? 'Failed to refresh; will retry on next change'
                        : '';

                return html`<tr
                  class="spectrum-Table-row ${rowState}"
                  title=${title}
                >
                  ${repeat(
                    row.getVisibleCells(),
                    (cell) => cell.id,
                    (cell) => {
                      if (cell.column.id === 'rowIndex') {
                        return html`
                          <span
                            class="index-column spectrum-Body spectrum-Body--sizeM"
                            >${currentIndex}</span
                          >
                        `;
                      } else {
                        return html`<td class="spectrum-Table-cell">
                          ${flexRender(
                            cell.column.columnDef.cell,
                            cell.getContext(),
                          )}
                        </td>`;
                      }
                    },
                  )}
                </tr>`;
              },
            )}
          </tbody>
        </table>
      </div>
    `;
  }
}

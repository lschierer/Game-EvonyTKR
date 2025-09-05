// This component supports both single-general and general-pair modes.
// It progressively loads full data row-by-row from the appropriate endpoint.
import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import 'iconify-icon';
import { customElement, property, state } from 'lit/decorators.js';
import { type CSSResultGroup, LitElement } from 'lit';
import { repeat } from 'lit/directives/repeat.js';
import { Signal, SignalWatcher, html } from '@lit-labs/signals';

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
  current: z.enum(['stale', 'pending', 'current']).optional(),
});
type GeneralDataStub = z.infer<typeof GeneralDataStub>;

const GeneralPairStub = z.object({
  primary: z.object({ name: z.string() }),
  secondary: z.object({ name: z.string() }),
  current: z.enum(['stale', 'pending', 'current']).optional(),
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
    this.nameList = [...stubData];
    // a signal won't fire until a menu changes,
    // kick off the first data fetch manually
    this.startBackgroundFetch();
    this.setupDataWatcher();
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
        primarySpecialty1: this.primarySettings.specialtyLevels[0].get(),
        primarySpecialty2: this.primarySettings.specialtyLevels[1].get(),
        primarySpecialty3: this.primarySettings.specialtyLevels[2].get(),
        primarySpecialty4: this.primarySettings.specialtyLevels[3].get(),
        secondaryCovenantLevel: this.secondarySettings!.covenantLevel.get(),
        secondarySpecialty1: this.secondarySettings!.specialtyLevels[0].get(),
        secondarySpecialty2: this.secondarySettings!.specialtyLevels[1].get(),
        secondarySpecialty3: this.secondarySettings!.specialtyLevels[2].get(),
        secondarySpecialty4: this.secondarySettings!.specialtyLevels[3].get(),
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
        specialty1: this.primarySettings.specialtyLevels[0].get(),
        specialty2: this.primarySettings.specialtyLevels[1].get(),
        specialty3: this.primarySettings.specialtyLevels[2].get(),
        specialty4: this.primarySettings.specialtyLevels[3].get(),
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

  private setupDataWatcher() {
    if (this.settingsWatcher) return;

    const fetchData = async () => {
      if (DEBUG) console.log('[watcher] settings changed → refetch');

      if (this.nameList.length) {
        this.invalidateData();
      }

      await this.updateComplete;
      this.startBackgroundFetch();
    };

    this.settingsWatcher = new Signal.subtle.Watcher(fetchData);
    if (DEBUG) {
      console.log(`setting watchers on settings`);
    }
    this.settingsWatcher.watch(this.primarySettings.covenantLevel);
    this.settingsWatcher.watch(this.primarySettings.ascendingLevel);
    for (let i = 0; i < 4; i++) {
      this.settingsWatcher.watch(this.primarySettings.specialtyLevels[i]);
    }
    if (this.mode === 'pair') {
      this.settingsWatcher.watch(this.secondarySettings!.covenantLevel);
      for (let i = 0; i < 4; i++) {
        this.settingsWatcher.watch(this.secondarySettings!.specialtyLevels[i]);
      }
    }
  }

  private invalidateData() {
    for (let i = 0; i < this.nameList.length; i++) {
      if (this.nameList[i].current !== 'stale') {
        this.nameList[i].current = 'stale';
      }
    }
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
      const i = pi.next().value;
      if (i) {
        promises.push(this.fetchRow(i, myRunId));
        batch.push(i);
      }
      count++;
    }

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
                const isStale = this.nameList[index]?.current === 'stale';

                return html`<tr
                  class="spectrum-Table-row ${isStale ? 'stale-row' : ''}"
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

// This component supports both single-general and general-pair modes.
// It progressively loads full data row-by-row from the appropriate endpoint.
import debugFunction from '../localDebug';
const DEBUG = debugFunction(new URL(import.meta.url).pathname);

import 'iconify-icon';
import { customElement, property, state } from 'lit/decorators.js';
import {
  html,
  type CSSResultGroup,
  LitElement,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';

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
import GeneralTableCSS from '../css/GeneralTable.css' with { type: 'css' };

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
export class GeneralTable extends LitElement {
  @property({ type: String }) mode: 'pair' | 'single' = 'single';
  @property({ type: String }) generalType = '';
  @property({ type: String }) uiTarget = '';
  @property({ type: String }) public ascendingLevel: string = 'red5';
  @property({ type: String }) public primaryCovenantLevel: string =
    'civilization';
  @property({ type: String }) public primarySpecialty1: string = 'gold';
  @property({ type: String }) public primarySpecialty2: string = 'gold';
  @property({ type: String }) public primarySpecialty3: string = 'gold';
  @property({ type: String }) public primarySpecialty4: string = 'gold';
  @property({ type: String }) public secondaryCovenantLevel: string =
    'civilization';
  @property({ type: String }) public secondarySpecialty1: string = 'gold';
  @property({ type: String }) public secondarySpecialty2: string = 'gold';
  @property({ type: String }) public secondarySpecialty3: string = 'gold';
  @property({ type: String }) public secondarySpecialty4: string = 'gold';
  @property({ type: String }) public allowedBuffActivation: string = 'Overall';

  @state() private _sorting: SortingState = [];
  @state() private columns: ColumnDef<RowData>[] = [];
  @state() private nameList: RowStub[] = [];

  private dataMap = new Map<number, RowData>();
  private tableData: RowData[] = [];
  private _bgFetchTimer?: number;
  private tableController = new TableController<RowData>(this);

  static styles: CSSResultGroup = [
    SpectrumTokensCSS,
    SpectrumProgressBarCSS,
    GeneralTableCSS,
  ];

  /* eslint-disable-next-line  @typescript-eslint/no-misused-promises */
  override async firstUpdated() {
    this.columns = this.generateColumns();
    const stubData = await this.fetchStubPairs();
    this.nameList = [...stubData];
    this.startBackgroundFetch();
  }

  protected override willUpdate(_changedProperties: PropertyValues) {
    if (
      _changedProperties.has('ascendingLevel') ||
      _changedProperties.has('primaryCovenantLevel') ||
      _changedProperties.has('primarySpecialty1') ||
      _changedProperties.has('primarySpecialty2') ||
      _changedProperties.has('primarySpecialty3') ||
      _changedProperties.has('primarySpecialty4') ||
      _changedProperties.has('secondaryCovenantLevel') ||
      _changedProperties.has('secondarySpecialty1') ||
      _changedProperties.has('secondarySpecialty2') ||
      _changedProperties.has('secondarySpecialty3') ||
      _changedProperties.has('secondarySpecialty4') ||
      _changedProperties.has('allowedBuffActivation')
    ) {
      this.nameList.forEach((nameStub) => {
        nameStub.current = 'stale';
        if (this.batchSize >= 10) {
          if (DEBUG) {
            console.log('resetting batch size');
          }
          this.batchSize = this.maxBatch;
        }
        this.startBackgroundFetch();
      });
    }
  }

  private generateColumns(): ColumnDef<RowData>[] {
    // Create a columns array starting with the static index column
    const columns: ColumnDef<RowData>[] = [
      {
        id: 'rowIndex',
        header: '#',
        enableSorting: false,
        size: 50,
        cell: ({ table, row }) => {
          const index = table.getRowModel().rows.indexOf(row) + 1;
          return html`${index}`;
        },
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

  private async fetchRow(index: number): Promise<RowData> {
    this.nameList[index].current = 'pending';
    const stub = this.nameList[index];

    const basePath = this.urlConversion('row');
    if (this.mode === 'pair') {
      const pairStub = stub as GeneralPairStub;
      const params = new URLSearchParams({
        primary: pairStub.primary.name,
        secondary: pairStub.secondary.name,
        ascendingLevel: this.ascendingLevel,
        primaryCovenantLevel: this.primaryCovenantLevel,
        primarySpecialty1: this.primarySpecialty1,
        primarySpecialty2: this.primarySpecialty2,
        primarySpecialty3: this.primarySpecialty3,
        primarySpecialty4: this.primarySpecialty4,
        secondaryCovenantLevel: this.secondaryCovenantLevel,
        secondarySpecialty1: this.secondarySpecialty1,
        secondarySpecialty2: this.secondarySpecialty2,
        secondarySpecialty3: this.secondarySpecialty3,
        secondarySpecialty4: this.secondarySpecialty4,
      });
      const url = `${basePath}?${params.toString()}`;
      const res = await fetch(url);
      if (!res.ok) {
        throw new Error(`HTTP error ${res.status}`);
      }
      const json = (await res.json()) as object;
      const parsed = GeneralPair.safeParse(json);
      if (parsed.success) {
        return parsed.data;
      } else {
        console.error(parsed.error.message);
        console.error(`recieved: `, res.text());
        throw new Error(parsed.error.message);
      }
    } else {
      const params = new URLSearchParams({
        generalType: this.generalType,
        ascendingLevel: this.ascendingLevel,
        covenantLevel: this.primaryCovenantLevel,
        specialty1: this.primarySpecialty1,
        specialty2: this.primarySpecialty2,
        specialty3: this.primarySpecialty3,
        specialty4: this.primarySpecialty4,
      });

      const url = `${basePath}?name=${encodeURIComponent(stub.primary.name)}&${params.toString()}`;
      const res = await fetch(url);
      if (!res.ok) {
        throw new Error(`HTTP error ${res.status}`);
      }
      const json = (await res.json()) as object;
      const parsed = GeneralData.safeParse(json);
      if (parsed.success) return parsed.data;
      throw new Error(parsed.error.message);
    }
  }

  private maxBatch = 6;
  private batchSize = this.maxBatch;
  private startBackgroundFetch() {
    if (this._bgFetchTimer) return;
    this._bgFetchTimer = 1;

    const maxConcurrency = this.batchSize;
    const pending = new Set<number>();

    // Initialize with all rows needing fetch
    this.nameList.forEach((row, i) => {
      if (!row.current || row.current === 'stale') {
        pending.add(i);
        this.nameList[i].current = 'pending';
      }
    });

    let active = 0;

    const maybeLaunchMore = () => {
      while (active < maxConcurrency && pending.size > 0) {
        const [i] = pending; // grab one from the set
        pending.delete(i);
        active++;

        this.fetchRow(i)
          .then((full) => {
            this.nameList[i].current = 'current';
            this.dataMap.set(i, full);
          })
          .catch((err: unknown) => {
            console.error(`Fetch failed for row ${i}`, err);
            this.nameList[i].current = 'stale';
            pending.add(i); // Retry later
          })
          .finally(() => {
            active--;
            maybeLaunchMore(); // continue the pipeline
            this.updateTableData();
          });
      }

      if (active === 0 && pending.size === 0) {
        console.log('✅ All rows fetched');
        this._bgFetchTimer = undefined;
        this.updateTableData();
      }
    };

    maybeLaunchMore();
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
    let rowIndex = 1;
    return html`
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
                const currentIndex = rowIndex++;
                const isStale = this.nameList[index]?.current === 'stale';
                return html`<tr
                  class="spectrum-Table-row ${isStale ? 'stale-row' : ''}"
                >
                  ${repeat(
                    row.getVisibleCells(),
                    (cell) => cell.id,
                    (cell) => {
                      if (cell.column.id === 'rowIndex') {
                        rowIndex++;
                        return html`
                          <span class="spectrum-Body spectrum-Body--sizeM"
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

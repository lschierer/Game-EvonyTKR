// This component supports both single-general and general-pair modes.
// It progressively loads full data row-by-row from the appropriate endpoint.

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
  ascending: z.boolean(),
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

const GeneralData = SingleBuffs.merge(BuffFields);
const GeneralPair = PairBuffs.merge(BuffFields);
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
  @property({ type: String }) typeHeader = '';
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

  @state() private _sorting: SortingState = [];
  @state() private columns: ColumnDef<RowData>[] = [];
  @state() private nameList: RowStub[] = [];

  private dataMap = new Map<number, RowData>();
  private tableData: RowData[] = [];
  private _bgFetchTimer?: number;
  private tableController = new TableController<RowData>(this);

  static styles: CSSResultGroup = [SpectrumTokensCSS, GeneralTableCSS];

  override async firstUpdated(_changed: PropertyValues) {
    this.columns = this.generateColumns();
    const stubData = await this.fetchStubPairs();
    this.nameList = [...stubData];
    this.startBackgroundFetch();
  }

  protected override async willUpdate(_changedProperties: PropertyValues) {
    if (
      _changedProperties.has('ascendingLevel') ||
      _changedProperties.has('primarySpecialty1') ||
      _changedProperties.has('primarySpecialty2') ||
      _changedProperties.has('primarySpecialty3') ||
      _changedProperties.has('primarySpecialty4') ||
      _changedProperties.has('secondarySpecialty1') ||
      _changedProperties.has('secondarySpecialty2') ||
      _changedProperties.has('secondarySpecialty3') ||
      _changedProperties.has('secondarySpecialty4')
    ) {
      this.nameList.forEach((nameStub, index) => {
        nameStub.current = 'stale';
        this.dataMap.delete(index);
        this.startBackgroundFetch();
      });
    }
  }
  private generateColumns(): ColumnDef<RowData>[] {
    return Array.from(tableHeaders.keys())
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
          if (!row) return null;
          if (key === 'primary') return (row as any).primary?.name;
          if (key === 'secondary') return (row as any).secondary?.name;
          return (row as any)[key];
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
                ? this.typeHeader
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
  }

  private async fetchStubPairs(): Promise<RowStub[]> {
    let url = window.location.pathname.replace(/\/$/, '') + '/data.json';
    url = url + `?ascendingLevel=${this.ascendingLevel}`;
    if (this.mode === 'single') {
      url = url + `&covenantLevel=${this.primaryCovenantLevel}`;
      url = url + `&specialty1=${this.primarySpecialty1}`;
      url = url + `&specialty2=${this.primarySpecialty2}`;
      url = url + `&specialty3=${this.primarySpecialty3}`;
      url = url + `&specialty4=${this.primarySpecialty4}`;
    } else {
      url = url.replace('/comparison', '');
      url = url.replace('Cavalry', 'Mounted');
      url = url.replace('Archer', 'Ranged');
      url = url.replace('Infantry', 'Ground');
      url = url + `&primaryCovenantLevel=${this.primaryCovenantLevel}`;
      url = url + `&primarySpecialty1=${this.primarySpecialty1}`;
      url = url + `&primarySpecialty2=${this.primarySpecialty2}`;
      url = url + `&primarySpecialty3=${this.primarySpecialty3}`;
      url = url + `&primarySpecialty4=${this.primarySpecialty4}`;
      url = url + `&secondaryCovenantLevel=${this.secondaryCovenantLevel}`;
      url = url + `&secondarySpecialty1=${this.secondarySpecialty1}`;
      url = url + `&secondarySpecialty2=${this.secondarySpecialty2}`;
      url = url + `&secondarySpecialty3=${this.secondarySpecialty3}`;
      url = url + `&secondarySpecialty4=${this.secondarySpecialty4}`;
    }
    const res = await fetch(url);
    const json = await res.json();
    const result =
      this.mode === 'pair'
        ? GeneralPairStub.array().safeParse(json.data)
        : GeneralDataStub.array().safeParse(json);
    if (result.success) return result.data;
    console.error('Stub validation failed', result.error);
    return [];
  }

  private async fetchRow(index: number): Promise<RowData> {
    this.nameList[index].current = 'pending';
    const stub = this.nameList[index];

    let basePath = window.location.pathname.replace(/\/comparison\/?$/, '');
    if (this.mode === 'pair') {
      basePath = basePath.replace('Cavalry', 'Mounted');
      basePath = basePath.replace('Archer', 'Ranged');
      basePath = basePath.replace('Infantry', 'Ground');
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
      const url = `${basePath}/pair?${params.toString()}`;
      const res = await fetch(url);
      const json = await res.json();
      const parsed = GeneralPair.safeParse(json);
      if (parsed.success) return parsed.data;
      console.error(parsed.error.message);
      throw new Error(parsed.error.message);
    } else {
      const parts = basePath.split('/');
      const lastPart = parts.pop() ?? '';
      const normalizedType =
        lastPart === 'Mayors'
          ? 'Mayor Specialist'
          : lastPart.replace(/s$/, '') + ' Specialist';
      const newBasePath = [...parts, normalizedType].join('/');

      const params = new URLSearchParams({
        ascendingLevel: this.ascendingLevel,
        covenantLevel: this.primaryCovenantLevel,
        specialty1: this.primarySpecialty1,
        specialty2: this.primarySpecialty2,
        specialty3: this.primarySpecialty3,
        specialty4: this.primarySpecialty4,
      });

      const url = `${newBasePath}/primary/${encodeURIComponent(stub.primary.name)}?${params.toString()}`;
      const res = await fetch(url);
      const json = await res.json();
      const parsed = GeneralData.safeParse(json);
      if (parsed.success) return parsed.data;
      throw new Error(parsed.error.message);
    }
  }

  private startBackgroundFetch() {
    if (this._bgFetchTimer) return;

    const loadNext = () => {
      const nextIndex = this.nameList.findIndex(
        (stub) => stub.current !== 'current',
      );
      if (nextIndex === -1) {
        this._bgFetchTimer = undefined;
        console.log(
          `nextIndex is -1, namelist has ${this.nameList.length} and tableData has ${this.tableData.length}`,
        );
        this.requestUpdate();
        return;
      }
      if (
        !this.nameList[nextIndex].current ||
        (this.nameList[nextIndex].current !== 'current' &&
          this.nameList[nextIndex].current !== 'pending')
      ) {
        this.nameList[nextIndex].current = 'pending';
        this.fetchRow(nextIndex)
          .then((full) => {
            this.nameList[nextIndex].current = 'current';
            this.dataMap.set(nextIndex, full);
            this.tableData = this.nameList
              .map((_, i) => this.dataMap.get(i))
              .filter(Boolean) as RowData[];
            this.requestUpdate();
          })
          .catch((e) => {
            console.error('Fetch failed at index', nextIndex, e);
            this.nameList[nextIndex].current = 'stale';
          });
      }

      this._bgFetchTimer = window.setTimeout(loadNext, 50);
    };

    loadNext();
  }

  override render() {
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
      ${this.tableData.length !== this.nameList.length
        ? html` <div class="spectrum-Overlay is-open loading-overlay">
            <div class="spectrum-Overlay-wrapper">
              <div class="spectrum-Overlay-content">Loading data...</div>
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
              (row) =>
                html`<tr class="spectrum-Table-row">
                  ${repeat(
                    row.getVisibleCells(),
                    (cell) => cell.id,
                    (cell) =>
                      html`<td class="spectrum-Table-cell">
                        ${flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </td>`,
                  )}
                </tr>`,
            )}
          </tbody>
        </table>
      </div>
    `;
  }
}

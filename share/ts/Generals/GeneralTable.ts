import 'iconify-icon';
import { customElement, state, property } from 'lit/decorators.js';
import {
  html,
  LitElement,
  type CSSResultGroup,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';
//import { styleMap } from 'lit/directives/style-map.js';
import { type Ref, createRef, ref } from 'lit/directives/ref.js';

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

// Import CSS
import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import GeneralDatasIndexCSS from '../css/GeneralTable.css' with { type: 'css' };

// Zod Schemas
export const Attack = z.object({
  base: z.number(),
  increment: z.number(),
});
export type Attack = z.infer<typeof Attack>;

export const BasicAttributes = z.object({
  attack: Attack,
  defense: Attack,
  leadership: Attack,
  politics: Attack,
});
export type BasicAttributes = z.infer<typeof BasicAttributes>;

export const General = z.object({
  ascending: z.boolean(),
  basicAttributes: BasicAttributes,
  builtInBookName: z.string(),
  id: z.string(),
  name: z.string(),
  specialityNames: z.array(z.string()),
  type: z.array(z.string()),
});
export type General = z.infer<typeof General>;

export const GeneralData = z.object({
  primary: General,
  attackbuff: z.number(),
  defensebuff: z.number(),
  groundattackdebuff: z.number(),
  grounddefensedebuff: z.number(),
  groundhpdebuff: z.number(),
  hpbuff: z.number(),
  marchbuff: z.number(),
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
export type GeneralData = z.infer<typeof GeneralData>;

const RowData = z.union([GeneralData, z.null()]);
type RowData = z.infer<typeof RowData>;

const GeneralDataStub = z.object({
  primary: z.object({ name: z.string() }),
  current: z.enum(['stale', 'pending', 'current']).optional(),
});
type GeneralDataStub = z.infer<typeof GeneralDataStub>;

// Columns
const tableHeaders = new Map<string, string>([
  ['primary', 'Primary'],
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

// Main Component
@customElement('general-table')
export default class GeneralTable extends LitElement {
  @property({ type: String })
  public generalType: string = '';

  @property({ type: String })
  public typeHeader: string = '';

  @state()
  private _sorting: SortingState = [];

  private tableData = new Array<RowData>();
  private GeneralDatas = new Map<number, GeneralData>();

  @state() private nameList: GeneralDataStub[] = new Array<GeneralDataStub>();

  private scrollElementRef: Ref<HTMLDivElement> = createRef();
  private _bgFetchTimer?: number; //purposefully not reactive.

  private tableController = new TableController<RowData>(this);

  static styles?: CSSResultGroup = [SpectrumTokensCSS, GeneralDatasIndexCSS];

  @state()
  private columns: ColumnDef<RowData>[] = new Array<ColumnDef<RowData>>();

  protected async firstUpdated(_changedProperties: PropertyValues) {
    super.firstUpdated(_changedProperties);
    console.log(
      `firstUpdated start with keys ${Array.from(_changedProperties.keys()).join(', ')}`,
    );

    this.columns = this.generateColumns();
    const stubData = await this.fetchStubPairs();
    this.nameList = [...stubData]; // ensure lit notices

    this.requestUpdate();
    this.startBackgroundFetch();
  }

  protected columnVisibilityRules: Record<
    string,
    { allow: (key: string) => boolean }
  > = {
    mayor: {
      allow: (key) => key === 'primary' || key.endsWith('debuff'),
    },
    // Future rule: wall generals
    wall_general: {
      allow: (key) =>
        ['primary', 'attackbuff', 'defensebuff', 'hpbuff'].includes(key),
    },
  };

  private generateColumns() {
    const typeKey = this.generalType?.toLowerCase?.() ?? '';
    console.log(
      `Resolved typeKey: '${typeKey}' from generalType: '${this.generalType}'`,
    );

    const rules = this.columnVisibilityRules[typeKey];
    if (!rules) {
      console.warn(
        `No column visibility rules defined for type '${typeKey}', using defaults.`,
      );
    }

    return Array.from(tableHeaders.keys())
      .filter((key) => (rules ? rules.allow(key) : true))
      .map((key) => {
        return {
          id: key,
          accessorFn:
            key === 'primary'
              ? (row: RowData) =>
                  row && typeof row === 'object' && 'primary' in row
                    ? row.primary.name
                    : ''
              : (row: RowData) =>
                  row && typeof row === 'object'
                    ? (row as GeneralData)[key as keyof GeneralData]
                    : null,
          enableSorting: true,
          sortDescFirst: key !== 'primary',
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
              ${info.column.id === 'primary'
                ? this.typeHeader
                : (tableHeaders.get(info.column.id) ?? info.column.id)}
              ${direction
                ? html`
                    <span class="table-header-sort">
                      <span>${showArrow}</span>
                      ${sortIndex > -1
                        ? html`<span class="table-header-sort-index">
                            ${sortIndex + 1}
                          </span>`
                        : null}
                    </span>
                  `
                : null}
            </div>`;
          },
        };
      });
  }

  protected override willUpdate(_changedProperties: PropertyValues) {
    super.willUpdate(_changedProperties);
    console.log(
      `willUpdate has keys ${Array.from(_changedProperties.keys()).join(', ')}`,
    );
    if (
      _changedProperties.has('generalType') ||
      _changedProperties.has('typeHeader')
    ) {
      this.columns = this.generateColumns();
    }
  }

  updated(_changedProperties: PropertyValues) {
    super.updated(_changedProperties);
    console.log(
      `updated changed has keys ${Array.from(_changedProperties.keys()).join(', ')}`,
    );
  }

  private async fetchStubPairs(): Promise<GeneralDataStub[]> {
    const url = window.location.pathname.replace(/\/$/, '') + '/data.json';
    const res = await fetch(url);
    const json = await res.json();
    console.log('json =', json);
    const valid = GeneralDataStub.array().safeParse(json);
    if (valid.success) {
      console.log('parsed GeneralDataStub:', valid.data);
      return valid.data;
    }
    console.error('Zod validation failed', valid.error);
    return [];
  }

  private async fetchPair(stub: GeneralDataStub): Promise<GeneralData> {
    const basePath = window.location.pathname.replace(/\/comparison\/?$/, '');
    const parts = basePath.split('/');
    const lastPart = parts.pop() ?? '';

    // Convert plural to singular and add " Specialist"
    const normalizedType =
      lastPart === 'Mayors'
        ? 'Mayor Specialist'
        : lastPart.replace(/s$/, '') + ' Specialist';

    const newBasePath = [...parts, normalizedType].join('/');
    const pairBuffUrl = `${newBasePath}/primary/${encodeURIComponent(stub.primary.name)}`;
    console.log(`fetchPair requesting url ${pairBuffUrl}`);
    const result = await fetch(pairBuffUrl)
      .then((res) => {
        if (!res.ok)
          throw new Error(
            `Failed to fetch buffs for pair ${stub.primary.name}`,
          );
        return res.json();
      })
      .then((data) => {
        const valid = GeneralData.safeParse(data);
        if (valid.success) {
          return valid.data;
        } else {
          console.error(valid.error.message);
          throw new Error(valid.error.message);
        }
      });
    return result;
  }

  private startBackgroundFetch() {
    if (this._bgFetchTimer) return;

    const loadNext = () => {
      const nextIndex = this.nameList.findIndex((stub) => {
        if (stub.current === undefined || stub.current === 'stale') {
          return true;
        }
        return false;
      });
      if (nextIndex === -1) {
        this._bgFetchTimer = undefined;
        return;
      }
      this.nameList[nextIndex].current = 'pending';
      this.updatePair_at_index(nextIndex);
      this._bgFetchTimer = window.setTimeout(loadNext, 100); // 10 rows per second
    };

    loadNext();
  }

  protected updatePair_at_index(nextIndex: number) {
    this.fetchPair(this.nameList[nextIndex])
      .then((pair) => {
        this.nameList[nextIndex].current = 'current';
        this.GeneralDatas.set(nextIndex, pair);
        this.tableData = Array.from(this.GeneralDatas.values());

        this.requestUpdate();
      })
      .catch((err) => {
        console.error(`Failed to fetch pair at index ${nextIndex}:`, err);
      });
  }

  // Called when form inputs change
  protected async handleFormUpdate(newParams: unknown) {
    console.log(`handleFormUpdate called with ${JSON.stringify(newParams)}`);
    this.nameList.map((stub) => (stub.current = 'stale'));
    this.nameList = [...this.nameList]; // trigger a reactive update;

    this.requestUpdate();
    this.startBackgroundFetch(); // start the background pair update;
  }

  protected render() {
    console.log(`render start with ${this.tableData.length}`);

    let returnable = html``;

    if (!this.tableData.length)
      returnable = html`${returnable}<span>Loading...</span>`;
    else {
      console.log(`render has ${this.tableData.length} items`);

      // Step 3: create the table
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

      returnable = html`${returnable}
        <div
          class="general-pairs-table spectrum-Table spectrum-Table-scroller spectrum-Table--sizeM"
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
            <tbody class="spectrum-Table-body" ${ref(this.scrollElementRef)}>
              ${repeat(
                table.getRowModel().rows,
                (row) => row.id,
                (row) => html`
                  <tr class="spectrum-Table-row">
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
                  </tr>
                `,
              )}
            </tbody>
          </table>
        </div> `;
    }

    return returnable;
  }
}

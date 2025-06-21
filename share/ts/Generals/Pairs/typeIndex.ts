import 'iconify-icon';
import { customElement, state } from 'lit/decorators.js';
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
import { VirtualizerController } from '@tanstack/lit-virtual';

import * as z from 'zod';

// Import CSS
import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import GeneralPairsIndexCSS from '../../css/GeneralPairsIndex.css' with { type: 'css' };

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

export const GeneralPair = z.object({
  primary: z.union([General, z.string()]),
  secondary: z.union([General, z.string()]),
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
export type GeneralPair = z.infer<typeof GeneralPair>;

const RowData = z.union([GeneralPair, z.null()]);
type RowData = z.infer<typeof RowData>;

const GeneralPairStub = z.object({
  primary: z.object({ name: z.string() }),
  secondary: z.object({ name: z.string() }),
  current: z.enum(['stale', 'pending', 'current']).optional(),
});
type GeneralPairStub = z.infer<typeof GeneralPairStub>;

// Columns
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

const columns: ColumnDef<RowData>[] = Array.from(tableHeaders.keys()).map(
  (key) => {
    const accessorKey =
      key === 'primary' || key === 'secondary' ? `${key}.name` : key;
    return {
      id: key,
      accessorKey,
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
          @click=${info.column.getToggleSortingHandler()}
          style="display: flex; align-items: center; gap: 0.4em; cursor: pointer;"
        >
          ${tableHeaders.get(info.column.id) ?? info.column.id}
          ${direction
            ? html`
                <span
                  style="display: inline-flex; align-items: center; gap: 0.2em;"
                >
                  <span>${showArrow}</span>
                  ${sortIndex > -1
                    ? html`<span class="table-header-sort">
                        ${sortIndex + 1}
                      </span>`
                    : null}
                </span>
              `
            : null}
        </div>`;
      },
    };
  },
);

// Main Component
@customElement('pairs-table')
export default class PairsTable extends LitElement {
  @state()
  private _sorting: SortingState = [];

  private tableData = new Array<RowData>();
  private generalPairs = new Map<number, GeneralPair>();

  @state() private nameList: GeneralPairStub[] = new Array<GeneralPairStub>();

  private scrollElementRef: Ref<HTMLDivElement> = createRef();
  private _bgFetchTimer?: number; //purposefully not reactive.

  private tableController = new TableController<RowData>(this);
  private virtualizerController = new VirtualizerController(this, {
    getScrollElement: () => this.scrollElementRef.value!,
    count: 0, // will be set when data is ready
    estimateSize: () => 45,
    overscan: 5,
  });

  static styles?: CSSResultGroup = [SpectrumTokensCSS, GeneralPairsIndexCSS];

  protected async firstUpdated() {
    console.log(`firstUpdated start`);
    const stubData = await this.fetchStubPairs();
    this.nameList = [...stubData]; // ensure lit notices

    this.virtualizerController.getVirtualizer().setOptions({
      ...this.virtualizerController.getVirtualizer().options,
      count: this.tableData.length,
    });
    this.requestUpdate();
    this.startBackgroundFetch();
  }

  updated(changed: PropertyValues) {
    console.log(
      `updated changed has keys ${Array.from(changed.keys()).join(', ')}`,
    );
  }

  protected willUpdate(changed: PropertyValues) {
    console.log(
      `willUpdate changed has keys ${Array.from(changed.keys()).join(', ')}`,
    );
  }

  private async fetchStubPairs(): Promise<GeneralPairStub[]> {
    const url = window.location.pathname.replace(/\/$/, '') + '/data.json';
    const res = await fetch(url);
    const json = await res.json();
    const valid = GeneralPairStub.array().safeParse(json.data);
    if (valid.success) {
      console.log(
        `fetchStubPairs returning ${valid.data.length} GeneralPairStub objects`,
      );
      return valid.data;
    }
    return [];
  }

  private async fetchPair(stub: GeneralPairStub): Promise<GeneralPair> {
    const basePath = window.location.pathname.replace(/\/$/, '');
    const pairBuffUrl = `${basePath}/pair?primary=${encodeURIComponent(stub.primary.name)}&secondary=${encodeURIComponent(stub.secondary.name)}`;
    console.log(`fetchPair requesting url ${pairBuffUrl}`);
    const result = await fetch(pairBuffUrl)
      .then((res) => {
        if (!res.ok)
          throw new Error(
            `Failed to fetch buffs for pair ${stub.primary.name}/${stub.secondary.name}`,
          );
        return res.json();
      })
      .then((data) => {
        const valid = GeneralPair.safeParse(data);
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
        this.generalPairs.set(nextIndex, pair);
        this.tableData = Array.from(this.generalPairs.values());
        this.virtualizerController.getVirtualizer().setOptions({
          ...this.virtualizerController.getVirtualizer().options,
          count: this.tableData.length,
        });
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
        columns,
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

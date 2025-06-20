import 'iconify-icon';

import { customElement } from 'lit/decorators.js';
import {
  html,
  LitElement,
  type CSSResultGroup,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';
import { until } from 'lit/directives/until.js';

import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import GeneralPairsIndexCSS from '../../css/GeneralPairsIndex.css' with { type: 'css' };

import {
  getCoreRowModel,
  TableController,
  type SortingState,
  type ColumnDef,
  getSortedRowModel,
  type CellContext,
  type HeaderContext,
} from '@tanstack/lit-table';

/**
 * TypeScript for enhanced General Pairs table sorting functionality
 */

export type SortDirection = 'asc' | 'desc';

import * as z from 'zod';

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
export type Ary = z.infer<typeof General>;

export const GeneralPair = z.object({
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
  primary: General,
  rangedattackdebuff: z.number(),
  rangeddefensedebuff: z.number(),
  rangedhpdebuff: z.number(),
  secondary: General,
  siegeattackdebuff: z.number(),
  siegedefensedebuff: z.number(),
  siegehpdebuff: z.number(),
});
export type GeneralPair = z.infer<typeof GeneralPair>;

export const flexRender = <TProps extends object>(comp: any, props: TProps) => {
  if (typeof comp === 'function') {
    return comp(props);
  }
  return comp;
};

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

const columns: ColumnDef<GeneralPair>[] = Array.from(tableHeaders.keys()).map(
  (key) => {
    const sortDescFirst = key !== 'primary' && key !== 'secondary';
    const accessorKey =
      key === 'primary' || key === 'secondary' ? `${key}.name` : key;

    return {
      id: key,
      accessorKey,
      enableSorting: true,
      cell: (info: CellContext<GeneralPair, unknown>) => {
        const value = info.getValue();
        return typeof value === 'number'
          ? value.toLocaleString()
          : (value ?? '');
      },
      header: (info: HeaderContext<GeneralPair, unknown>) =>
        html`<span>${tableHeaders.get(info.column.id)}</span>`,
      sortDescFirst,
    };
  },
);

@customElement('pairs-table')
export default class PairsTable extends LitElement {
  private tableController = new TableController<GeneralPair>(this);

  static properties = {
    mode: { type: String },
    data: { attribute: false },
  };

  private _sorting: SortingState = [];
  private data: GeneralPair[] = [];

  protected async firstUpdated(
    _changedProperties: PropertyValues,
  ): Promise<void> {
    this.data = await this.fetchData();
    this.requestUpdate();
  }

  protected willUpdate(_changedProperties: PropertyValues): void {}

  async fetchData(): Promise<GeneralPair[]> {
    const url = window.location.pathname.replace(/\/$/, '') + '/data.json';
    const response = await fetch(url);
    if (!response.ok) throw new Error('Failed to fetch data');

    const json = await response.json();
    const valid = GeneralPair.array().safeParse(json.data as object);
    if (valid.success) {
      return valid.data;
    }
    console.error('error getting data: ', valid.error.message);
    return [];
  }

  static styles?: CSSResultGroup | undefined = [
    SpectrumTokensCSS,
    GeneralPairsIndexCSS,
  ];

  private async renderTable() {
    const table = this.tableController.table({
      columns,
      data: this.data,
      state: {
        sorting: this._sorting,
      },
      onSortingChange: (updaterOrValue) => {
        this._sorting =
          typeof updaterOrValue === 'function'
            ? updaterOrValue(this._sorting)
            : updaterOrValue;

        this.requestUpdate(); // Important!
      },
      getSortedRowModel: getSortedRowModel(),
      getCoreRowModel: getCoreRowModel(),
    });

    return html`
      <table class="spectrum-Table spectrum-Table--sizeM">
        <thead class="spectrum-Table-head">
          ${repeat(
            table.getHeaderGroups(),
            (headerGroup) => headerGroup.id,
            (headerGroup) => html`
              ${repeat(
                headerGroup.headers,
                (header) => header.id,
                (header) => html`
                  <th
                    colspan="${header.colSpan}"
                    class="spectrum-Table-headCell is-sortable"
                  >
                    ${header.isPlaceholder
                      ? null
                      : html`<div
                          title=${header.column.getCanSort()
                            ? header.column.getNextSortingOrder() === 'asc'
                              ? 'Sort ascending'
                              : header.column.getNextSortingOrder() === 'desc'
                                ? 'Sort descending'
                                : 'Clear sort'
                            : undefined}
                          @click="${header.column.getToggleSortingHandler()}"
                          style="cursor: ${header.column.getCanSort()
                            ? 'pointer'
                            : 'not-allowed'}"
                        >
                          ${flexRender(
                            header.column.columnDef.header,
                            header.getContext(),
                          )}
                          ${{ asc: ' 🔼', desc: ' 🔽' }[
                            header.column.getIsSorted() as string
                          ] ?? null}
                        </div>`}
                  </th>
                `,
              )}
            `,
          )}
        </thead>
        <tbody class="spectrum-Table-body">
          ${repeat(
            table.getRowModel().rows,
            (row) => row.id,
            (row) => html`
              <tr class="spectrum-Table-row">
                ${repeat(
                  row.getVisibleCells(),
                  (cell) => cell.id,
                  (cell) =>
                    html` <td class="spectrum-Table-cell">
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
    `;
  }
  protected render() {
    return html`
      <div id="wrapper" class="general-pairs-table">
        ${until(this.renderTable(), html`<span>Loading...</span>`)}
      </div>
    `;
  }
}

import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import 'iconify-icon';
//import { sprintf } from 'sprintf-js';

import { customElement, property, state } from 'lit/decorators.js';
import {
  html,
  type CSSResultGroup,
  LitElement,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';

import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import SpectrumProgressBarCSS from '@spectrum-css/progressbar/dist/index.css' with { type: 'css' };
import GeneralTableCSS from '../../../share/public/css/GeneralTable.css' with { type: 'css' };

import {
  createSortedRowModel,
  rowSortingFeature,
  sortFns,
  tableFeatures,
  TableController,
  type ColumnDef,
  type SortingState,
  type CellContext,
  type HeaderContext,
  type Table,
  flexRender,
} from '@tanstack/lit-table';

import type * as data from './data';
import type { SingleGeneralState } from '../GeneralRowSchemas';

const features = tableFeatures({ rowSortingFeature });
type Features = typeof features;
const rowModels = {
  sortedRowModel: createSortedRowModel<Features, SingleGeneralState>(sortFns),
};

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

@customElement('single-table')
export class SingleTable extends LitElement {
  @property({ type: String }) generalType = '';
  @property({ type: String }) uiTarget = '';
  @property({ type: String }) public allowedBuffActivation: string = 'Overall';

  @state() private _sorting: SortingState = [];
  @state() private columns: ColumnDef<Features, SingleGeneralState>[] = [];

  private table?: Table<Features, SingleGeneralState>;
  @state() private tableData: SingleGeneralState[] = [];
  private tableController = new TableController<Features, SingleGeneralState>(
    this,
  );

  private data?: data.SingleData;

  static styles: CSSResultGroup = [
    SpectrumTokensCSS,
    SpectrumProgressBarCSS,
    GeneralTableCSS,
  ];

  override connectedCallback(): void {
    super.connectedCallback();
    const qr = this.querySelector('single-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as data.SingleData;
    }
    if (this.data) {
      this.data.generalStore.store.subscribe(() => {
        this.updateTableData();
        this.requestUpdate();
      });
      // this second one *shouldn't* be necessary, the store subscription
      // should be sufficient since the filters changing should change
      // the store.
      this.data.queryParams.subscribe(() => {
        this.requestUpdate();
      });
    }
  }

  protected override firstUpdated(_changedProperties: PropertyValues): void {
    super.firstUpdated(_changedProperties);
    this.columns = this.generateColumns();
  }

  protected generateColumns: () => ColumnDef<Features, SingleGeneralState>[] =
    () => {
      const columns: ColumnDef<Features, SingleGeneralState>[] = [
        {
          id: 'rowIndex',
          header: '#',
          enableSorting: false,
          meta: { isIndex: true }, // handy flag for render branch
        },
      ];

      const dynamicColumns = Array.from(tableHeaders.keys())
        .filter((key) => {
          if (this.generalType === 'mayor') {
            if (
              key === 'marchbuff' ||
              key === 'attackbuff' ||
              key === 'defensebuff' ||
              key === 'hpbuff'
            ) {
              return false;
            }
          } else if (this.generalType === 'wall') {
            if (key === 'marchbuff') {
              return false;
            }
          }
          return true;
        })
        .map((key): ColumnDef<Features, SingleGeneralState> => {
          const accessorFn = (row: SingleGeneralState) => {
            if (key === 'primary') return row.primary;
            else {
              if (!row.data) {
                return undefined;
              }
              return row.data[key as keyof typeof row.data];
            }
          };
          return {
            id: key,
            accessorFn,
            enableSorting: true,
            sortUndefined: 'last',
            sortDescFirst: key !== 'primary',
            cell: (info: CellContext<Features, SingleGeneralState>) => {
              const value = info.getValue();
              return typeof value === 'number'
                ? value.toLocaleString()
                : (value ?? '');
            },
            header: (info: HeaderContext<Features, SingleGeneralState>) => {
              const sortIndex = info.table.store.state.sorting.findIndex(
                (s) => s.id === info.column.id,
              );
              const direction = info.column.getIsSorted();
              const showArrow =
                direction === 'asc' ? '🔼' : direction === 'desc' ? '🔽' : '';
              return html`<div
                class="table-header-sort"
                @click=${info.column.getToggleSortingHandler()}
              >
                ${tableHeaders.get(key) ?? key}
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

      columns.push(...dynamicColumns);
      return columns;
    };

  protected updateTableData = () => {
    if (!this.data) return;
    this.tableData = this.data.generalStore.store.state.catalog
      .map((ce) => {
        const key = ce.primary;
        const entry = this.data?.generalStore.store.state.rows[key];
        return entry;
      })
      .filter((e) => !!e)
      .filter((e) => {
        if (e.state === 'ignore') {
          return false;
        }
        return true;
      })
      .sort((a, b) => {
        return a.primary.localeCompare(b.primary);
      });
  };

  override render() {
    if (!this.data) {
      return html` <span>Pending Data</span> `;
    }

    this.table = this.tableController.table({
      features,
      rowModels,
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
    });

    return html`
      ${DEBUG ? html` table is ${typeof this.table} ` : ''}
      <div
        class="generals-table spectrum-Table spectrum-Table-scroller spectrum-Table--quiet spectrum-Table--sizeM spectrum-Table--compact"
      >
        <table class="spectrum-Table-main">
          <thead class="spectrum-Table-head">
            ${repeat(
              this.table.getHeaderGroups(),
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
              this.table.getRowModel().rows,
              (row) => row.id,
              (row, index) => {
                const currentIndex = index + 1;
                const rowEntry = row.original;

                const title =
                  rowEntry.state === 'stale'
                    ? 'Showing previous values; updating…'
                    : rowEntry.state === 'pending'
                      ? 'Refreshing…'
                      : rowEntry.state === 'error'
                        ? 'Failed to refresh; will retry on next change'
                        : '';

                return html`<tr
                  class="spectrum-Table-row ${rowEntry.state}"
                  title=${title}
                >
                  ${repeat(
                    row.getAllCells(),
                    (cell) => cell.id,
                    (cell) =>
                      cell.column.id === 'rowIndex'
                        ? html`<span
                            class="index-column spectrum-Body spectrum-Body--sizeM"
                          >
                            ${currentIndex}
                          </span>`
                        : html`<td
                            class="spectrum-Table-cell ${rowEntry.state}"
                          >
                            ${flexRender(
                              cell.column.columnDef.cell,
                              cell.getContext(),
                            )}
                          </td>`,
                  )}
                </tr>`;
              },
            )}
          </tbody>
        </table>
      </div>
      <slot></slot>
    `;
  }
}

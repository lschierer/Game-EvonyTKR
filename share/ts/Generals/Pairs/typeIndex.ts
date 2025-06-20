import 'iconify-icon';
import { customElement } from 'lit/decorators.js';
import {
  html,
  LitElement,
  type PropertyDeclarations,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';
import { createRef, ref, type Ref } from 'lit/directives/ref.js';
import { styleMap } from 'lit/directives/style-map.js';

// Import CSS
import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import GeneralPairsIndexCSS from '../../css/GeneralPairsIndex.css' with { type: 'css' };

// Import Tanstack Table with virtualization
import {
  type Row,
  getCoreRowModel,
  TableController,
  type SortingState,
  type ColumnDef,
  getSortedRowModel,
  flexRender,
} from '@tanstack/lit-table';

import { VirtualizerController } from '@tanstack/lit-virtual';

// Define types
interface General {
  name: string;
}

interface GeneralPair {
  primary: General;
  secondary: General;
  [key: string]: any;
}

// Cache for buff values
const buffCache = new Map<string, Record<string, number>>();

@customElement('pairs-table')
export default class PairsTable extends LitElement {
  static properties: PropertyDeclarations = {
    _sorting: { type: Array, attribute: false },
    data: { type: Array, attribute: false },
    isLoading: { type: Boolean, attribute: false },
  };

  private tableController = new TableController<GeneralPair>(this);
  private tableContainerRef: Ref = createRef();

  private rowVirtualizerController:
    | VirtualizerController<Element, Element>
    | undefined;

  // Properties
  private _sorting: SortingState = [];
  private data: GeneralPair[] = [];
  private isLoading = false;
  private estimatedHeight = 35;

  // Column definitions
  private tableHeaders = new Map<string, string>([
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

  static styles = [SpectrumTokensCSS, GeneralPairsIndexCSS];

  protected async firstUpdated(
    _changedProperties: PropertyValues,
  ): Promise<void> {}

  async connectedCallback() {
    super.connectedCallback(); // Call super first
    this.data = await this.fetchPairData();
    this.rowVirtualizerController = new VirtualizerController(this, {
      count: this.data.length,
      getScrollElement: () => this.tableContainerRef.value!,
      estimateSize: () => this.estimatedHeight,
      overscan: 5,
    });
    this.requestUpdate();
  }

  async fetchPairData(): Promise<GeneralPair[]> {
    const url = window.location.pathname.replace(/\/$/, '') + '/data.json';
    const response = await fetch(url);
    if (!response.ok) throw new Error('Failed to fetch data');

    const json = await response.json();
    return json.data;
  }

  async fetchBuffData(pair: GeneralPair): Promise<Record<string, number>> {
    const pairKey = `${pair.primary.name}-${pair.secondary.name}`;

    if (buffCache.has(pairKey)) {
      return buffCache.get(pairKey)!;
    }

    this.isLoading = true;
    this.requestUpdate();

    try {
      const basePath = window.location.pathname.replace(/\/$/, '');
      const pathParts = basePath.split('/');
      const generalType = pathParts[pathParts.length - 1];
      const baseGeneralPath = `/Generals/${generalType.replace('Pairs', 'Specialist')}`;

      // Fetch primary general buffs
      const primaryUrl = `${baseGeneralPath}/primary/${encodeURIComponent(pair.primary.name)}`;
      const primaryResponse = await fetch(primaryUrl);
      const primaryData = await primaryResponse.json();

      // Fetch secondary general buffs
      const secondaryUrl = `${baseGeneralPath}/secondary/${encodeURIComponent(pair.secondary.name)}`;
      const secondaryResponse = await fetch(secondaryUrl);
      const secondaryData = await secondaryResponse.json();

      // Combine the buff values
      const combinedBuffs: Record<string, number> = {};
      for (const key of Object.keys(primaryData)) {
        if (key in secondaryData) {
          combinedBuffs[key] =
            (primaryData[key] || 0) + (secondaryData[key] || 0);
        }
      }

      // Cache the result
      buffCache.set(pairKey, combinedBuffs);
      return combinedBuffs;
    } catch (error) {
      console.error('Error fetching buff data:', error);
      return {};
    } finally {
      this.isLoading = false;
      this.requestUpdate();
    }
  }

  createColumns(): ColumnDef<GeneralPair>[] {
    return Array.from(this.tableHeaders.entries()).map(([key, label]) => {
      const sortDescFirst = key !== 'primary' && key !== 'secondary';
      const accessorKey =
        key === 'primary' || key === 'secondary' ? `${key}.name` : key;

      return {
        id: key,
        accessorKey,
        enableSorting: true,
        cell: (info) => {
          const value = info.getValue();
          const rowData = info.row.original;

          // For buff values that haven't been loaded yet
          if (key !== 'primary' && key !== 'secondary' && value === undefined) {
            // Fetch data for this row if it's visible
            this.fetchBuffData(rowData).then((buffData) => {
              Object.assign(rowData, buffData);
              this.requestUpdate();
            });

            return html`<span class="loading-placeholder">Loading...</span>`;
          }

          return typeof value === 'number'
            ? value.toLocaleString()
            : (value ?? '');
        },
        header: () => html`<span>${label}</span>`,
        sortDescFirst,
      };
    });
  }

  render() {
    const table = this.tableController.table({
      columns: this.createColumns(),
      data: this.data,
      state: {
        sorting: this._sorting,
      },
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

    const { rows } = table.getRowModel();

    if (this.rowVirtualizerController) {
      const virtualizer = this.rowVirtualizerController.getVirtualizer();
      return html`
        <div class="general-pairs-table">
          ${this.isLoading
            ? html`<div class="loading-indicator">Loading data...</div>`
            : ''}

          <div class="table-container" style="height: 600px; overflow-y: auto;">
            <div
              class=" container spectrum-Table spectrum-Table-scroller spectrum-Table--sizeM spectrum-Table--emphasized "
              ${ref(this.tableContainerRef)}
            >
              <table
                class=" spectrum-Table-main spectrum-Table--sizeM spectrum-Table--emphasized "
              >
                <thead class="spectrum-Table-head">
                  ${repeat(
                    table.getHeaderGroups(),
                    (headerGroup) => headerGroup.id,
                    (headerGroup) => html`
                      <tr>
                        ${repeat(
                          headerGroup.headers,
                          (header) => header.id,
                          (header) => html`
                            <th
                              colspan="${header.colSpan}"
                              class="spectrum-Table-headCell is-sortable ${header.column.getIsSorted()
                                ? `is-sorted-${header.column.getIsSorted()}`
                                : ''}"
                              @click="${header.column.getToggleSortingHandler()}"
                            >
                              ${header.isPlaceholder
                                ? null
                                : html`
                                    <div>
                                      ${flexRender(
                                        header.column.columnDef.header,
                                        header.getContext(),
                                      )}
                                      ${header.column.getIsSorted()
                                        ? html`<span
                                            >${header.column.getIsSorted() ===
                                            'asc'
                                              ? ' 🔼'
                                              : ' 🔽'}</span
                                          >`
                                        : null}
                                    </div>
                                  `}
                            </th>
                          `,
                        )}
                      </tr>
                    `,
                  )}
                </thead>
                <tbody
                  class="spectrum-Table-body"
                  style=${styleMap({
                    height: `${virtualizer.getTotalSize()}px`,
                  })}
                >
                  ${repeat(
                    this.rowVirtualizerController
                      .getVirtualizer()
                      .getVirtualItems(),
                    (item) => item.key,
                    (item) => {
                      const row = rows[item.index] as Row<GeneralPair>;
                      return html`
                        <tr
                          data-index="${item.index}"
                          style=${styleMap({
                            transform: `translateY(${item.start}px)`,
                            position: 'absolute',
                            top: 0,
                            left: 0,
                            width: '100%',
                          })}
                          ${ref((node) => {
                            if (this.rowVirtualizerController) {
                              return this.rowVirtualizerController
                                .getVirtualizer()
                                .measureElement(node);
                            }
                          })}
                        >
                          ${repeat(
                            row.getVisibleCells(),
                            (cell) => cell.id,
                            (cell) => html`
                              <td
                                class="spectrum-Table-cell"
                                style=${styleMap({
                                  width: `${cell.column.getSize()}px`,
                                })}
                              >
                                ${flexRender(
                                  cell.column.columnDef.cell,
                                  cell.getContext(),
                                )}
                              </td>
                            `,
                          )}
                        </tr>
                      `;
                    },
                  )}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      `;
    }
    return html`<span>Loading...</span>`;
  }
}

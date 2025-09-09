// This component supports both single-general and general-pair modes.
// It progressively loads full data row-by-row from the appropriate endpoint.
import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import 'iconify-icon';
//import { sprintf } from 'sprintf-js';

import { customElement, property, state } from 'lit/decorators.js';
import { LitElement, type CSSResultGroup } from 'lit';
//import { repeat } from 'lit/directives/repeat.js';
import { SignalWatcher, html, computed } from '@lit-labs/signals';

import {
  //getCoreRowModel,
  //getSortedRowModel,
  TableController,
  type ColumnDef,
  type SortingState,
  type CellContext,
  type HeaderContext,
  //flexRender,
} from '@tanstack/lit-table';

import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import SpectrumProgressBarCSS from '@spectrum-css/progressbar/dist/index.css' with { type: 'css' };
import GeneralTableCSS from '../../share/public/css/GeneralTable.css' with { type: 'css' };

// Zod Schemas
import { GeneralPairStub, GeneralPair } from './GeneralRowSchemas';

import { PairTableData } from './PairTableData';

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

  //@state() private _sorting: SortingState = [];
  //@state() private columns: ColumnDef<GeneralPair>[] = [];
  //@state() private nameList: GeneralPairStub[] = [];

  //private dataMap = new Map<number, GeneralPair>();
  //@state() private tableData: GeneralPair[] = [];
  //private tableController = new TableController<GeneralPair>(this);
  private TableDataController: PairTableData | null = null;

  protected lastFilterSettings: string = '';

  static styles: CSSResultGroup = [
    SpectrumTokensCSS,
    SpectrumProgressBarCSS,
    GeneralTableCSS,
  ];

  override async connectedCallback() {
    void super.connectedCallback();
    if (DEBUG) {
      console.log(`GeneralTable mode is ${this.mode}`);
    }
    this.TableDataController = new PairTableData(this);
    if (this.mode === 'pair') {
      if (!this.TableDataController.secondarySettings) {
        console.error('error creating secondary settings form');
      } else {
        this.TableDataController.secondarySettings!.is_primary = false;
        this.TableDataController.secondarySettings!.FormTitle =
          'Secondary General';
        this.TableDataController.secondarySettings.requestUpdate();
      }
    } else if (!this.uiTarget.localeCompare('Mayor Specialists')) {
      this.TableDataController.primarySettings.FormTitle = 'Mayor';
    } else {
      this.TableDataController.primarySettings.FormTitle = 'General';
      this.TableDataController.primarySettings.requestUpdate();
    }

    // this must happen *after* the 2 level settings forms are set up just above.
    this.addEventListener('form_updated', (e) => {
      if (DEBUG) {
        console.log(`got a form_updated event`);
      }
      const detail = (e as CustomEvent).detail;
      if (detail['is_primary' as keyof typeof detail]) {
        if (DEBUG) {
          console.log(`primary form changed`);
        }
      } else {
        if (DEBUG) {
          console.log(`secondary form changed`);
        }
      }
      const currentHash = this.filterSettings.get();
      if (this.lastFilterSettings.localeCompare(currentHash)) {
        this.lastFilterSettings = currentHash;

        // Debounce the actual SSE request
      }
    });
  }

  override disconnectedCallback() {
    super.disconnectedCallback();
  }

  /* eslint-disable-next-line  @typescript-eslint/no-misused-promises */
  override async firstUpdated() {
    this.columns = this.generateColumns();
  }

  private generateColumns(): ColumnDef<GeneralPair>[] {
    // Create a columns array starting with the static index column
    const columns: ColumnDef<GeneralPair>[] = [
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
        const accessorFn = (row: GeneralPair) => {
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
          cell: (info: CellContext<GeneralPair, unknown>) => {
            const value = info.getValue();
            return typeof value === 'number'
              ? value.toLocaleString()
              : (value ?? '');
          },
          header: (info: HeaderContext<GeneralPair, unknown>) => {
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

  private filterSettings = computed(() => {
    const fs = {
      primary: {
        ascendingLevel:
          this.TableDataController!.primarySettings.ascendingLevel.get(),
        covenantLevel:
          this.TableDataController!.primarySettings.covenantLevel.get(),
        specialtyLevel1:
          this.TableDataController!.primarySettings.specialtyLevel1.get(),
        specialtyLevel2:
          this.TableDataController!.primarySettings.specialtyLevel2.get(),
        specialtyLevel3:
          this.TableDataController!.primarySettings.specialtyLevel3.get(),
        specialtyLevel4:
          this.TableDataController!.primarySettings.specialtyLevel4.get(),
        selectedCount:
          this.TableDataController!.primarySettings.selectedCount.get(),
        selected:
          this.TableDataController!.primarySettings.selectedGenerals.get().join(
            '|',
          ),
      },
      ...(this.TableDataController!.secondarySettings && {
        secondary: {
          covenantLevel:
            this.TableDataController!.secondarySettings.covenantLevel.get(),
          specialtyLevel1:
            this.TableDataController!.secondarySettings.specialtyLevel1.get(),
          specialtyLevel2:
            this.TableDataController!.secondarySettings.specialtyLevel2.get(),
          specialtyLevel3:
            this.TableDataController!.secondarySettings.specialtyLevel3.get(),
          specialtyLevel4:
            this.TableDataController!.secondarySettings.specialtyLevel4.get(),
        },
      }),
    };
    return JSON.stringify(fs);
  });

  // add a field:
  //private _visibleIndexMap: number[] = [];

  override render() {
    this.tableData = this.TableDataController!.rows.get().filter(
      (gp): gp is GeneralPair => gp !== undefined,
    );
    const totalCount = this.TableDataController!.rows.get().length;
    const loadingCount = this.TableDataController!.master_row_list.filter(
      (r) => r.current !== 'current' && r.current !== 'ignore',
    ).length;
    //const table = this.tableController.table({
    //  columns: this.columns,
    //  data: this.tableData,
    //  state: { sorting: this._sorting },
    //  onSortingChange: (updaterOrValue) => {
    //    this._sorting =
    //      typeof updaterOrValue === 'function'
    //        ? updaterOrValue(this._sorting)
    //        : updaterOrValue;
    //    this.requestUpdate();
    //  },
    //  getSortedRowModel: getSortedRowModel(),
    //  getCoreRowModel: getCoreRowModel(),
    //});
    const cfs = this.filterSettings.get();
    return html`
      <span style="display: none">${cfs}</span>
      ${this.TableDataController
        ? this.TableDataController.display()
        : DEBUG
          ? html`<span>No Controller</span>`
          : ''}
      ${loadingCount > 0
        ? html` <div id="table-loading">
            <div class=" spectrum-ProgressBar " role="progressbar">
              <div class="spectrum-ProgressBar-track">
                <div
                  class="spectrum-ProgressBar-fill"
                  style="inline-size: ${((totalCount - loadingCount) /
                    totalCount) *
                  100}%;"
                ></div>
              </div>
              <div class="spectrum-ProgressBar-label">
                Refreshing ${loadingCount} of ${totalCount} rows...
              </div>
            </div>
          </div>`
        : ''}
      <div
        class="general-pairs-table spectrum-Table spectrum-Table-scroller spectrum-Table--quiet spectrum-Table--sizeM spectrum-Table--compact"
      ></div>
    `;
  }
}
//public async fetchRow(
//  index: number,
//  runId: number,
//): Promise<GeneralPair | undefined> {
//  this.nameList[index].current = 'pending';
//  const stub = this.nameList[index];
//
//  const basePath = this.urlConversion('row');
//  if (this.mode === 'pair') {
//    const pairStub = stub as GeneralPairStub;
//    const params = new URLSearchParams({
//      primary: pairStub.primary.name,
//      secondary: pairStub.secondary.name,
//      ascendingLevel: this.TableDataController.primarySettings.ascendingLevel.get(),
//      primaryCovenantLevel: this.TableDataController.primarySettings.covenantLevel.get(),
//      primarySpecialty1: this.TableDataController.primarySettings.specialtyLevel1.get(),
//      primarySpecialty2: this.TableDataController.primarySettings.specialtyLevel2.get(),
//      primarySpecialty3: this.TableDataController.primarySettings.specialtyLevel3.get(),
//      primarySpecialty4: this.TableDataController.primarySettings.specialtyLevel4.get(),
//      secondaryCovenantLevel: this.TableDataController.secondarySettings!.covenantLevel.get(),
//      secondarySpecialty1: this.TableDataController.secondarySettings!.specialtyLevel1.get(),
//      secondarySpecialty2: this.TableDataController.secondarySettings!.specialtyLevel2.get(),
//      secondarySpecialty3: this.TableDataController.secondarySettings!.specialtyLevel3.get(),
//      secondarySpecialty4: this.TableDataController.secondarySettings!.specialtyLevel4.get(),
//    });
//    const url = `${basePath}?${params.toString()}`;
//    const res = await fetch(url);
//    if (!res.ok) {
//      throw new Error(`HTTP error ${res.status}`);
//    }
//    const json = (await res.json()) as object;
//    const parsed = GeneralPair.safeParse(json);
//    if (parsed.success) {
//      if (runId === this.dataRunId) {
//        return parsed.data;
//      } else {
//        // this fetch got old data, but because the row was 'pending'
//        // the new loop will not have created a fetchRow call for it.
//        // mark this row as stale so that the new loop that *made*
//        // this request obsolte can call fetchRow for this row.
//        this.nameList[index].current = 'stale';
//        return;
//      }
//    } else {
//      console.error(parsed.error.message);
//      console.error(`recieved: `, res.text());
//      throw new Error(parsed.error.message);
//    }
//  } else {
//    const params = new URLSearchParams({
//      generalType: this.generalType,
//      ascendingLevel: this.TableDataController.primarySettings.ascendingLevel.get(),
//      covenantLevel: this.TableDataController.primarySettings.covenantLevel.get(),
//      specialty1: this.TableDataController.primarySettings.specialtyLevel1.get(),
//      specialty2: this.TableDataController.primarySettings.specialtyLevel2.get(),
//      specialty3: this.TableDataController.primarySettings.specialtyLevel3.get(),
//      specialty4: this.TableDataController.primarySettings.specialtyLevel4.get(),
//    });

//    const url = `${basePath}?name=${encodeURIComponent(stub.primary.name)}&${params.toString()}`;
//    const res = await fetch(url);
//    if (!res.ok) {
//      throw new Error(`HTTP error ${res.status}`);
//    }
//    const json = (await res.json()) as object;
//    const parsed = GeneralData.safeParse(json);
//    if (parsed.success) {
//      if (runId === this.dataRunId) {
//        return parsed.data;
//      }
//    } else {
//      throw new Error(parsed.error.message);
//    }
//  }
//  return;
//}

//private startBackgroundFetch() {
//  // ensure ids are always unique even if not strictly time accurate.
//  let newId = Math.floor(Date.now() / 1000);
//  while (newId <= this.dataRunId) {
//    newId++;
//  }
//  this.dataRunId = newId;
//  if (DEBUG) {
//    console.log(
//      `startBackgroundFetch starting with id ${this.dataRunId} and for nameList ${this.nameList.length}`,
//    );
//  }
//  const params = this.urlParams.get();
//  const url = `${this.urlConversion('details')}?${params.toString()}`;
//  if (DEBUG) {
//    console.log(`data url is ${url.toString}`);
//  }
//
//  this.currentEventSource = new EventSource(url);
//  this.currentEventSource.onmessage = (event) => {
//    const message: Object = JSON.parse(event.data) as Object;
//    // typescript keeps insisting that message['runId' as keyof typeof message] is a function type.
//    // force it to be a number with zod.
//    const valid = z
//      .number()
//      .safeParse(message['runId' as keyof typeof message]);
//    if (!valid.success) {
//      console.error(`invalid event from ${url}`);
//    }
//    const runId = valid.data;
//
//    if (DEBUG) {
//      console.log(`Received SSE event with runId ${runId}:`, message);
//    }
//
//    // Ignore messages from old streams
//    if (runId !== this.dataRunId) {
//      if (DEBUG) {
//        console.log(
//          `Ignoring old runId ${runId}, current is ${this.dataRunId}`,
//        );
//      }
//      return;
//    }
//
//    // Pass the actual row data, not the wrapper
//    this.updateRowFromStream(message['data' as keyof typeof message]);
//  };
//
//  this.currentEventSource.onerror = (e) => {
//    console.error('EventSource failed:', e);
//    this.currentEventSource?.close();
//    this.currentEventSource = undefined;
//  };
//  this.currentEventSource.addEventListener('complete', () => {
//    this.currentEventSource?.close();
//    this.currentEventSource = undefined;
//  });
//}
//
//private updateTableData() {
//  this.tableData = this.nameList
//    .map((row, i) =>
//      row.current !== 'ignore' ? this.dataMap.get(i) : undefined,
//    )
//    .filter((x): x is GeneralPair => !!x);
//
//  this._visibleIndexMap = this.nameList
//    .map((row, i) =>
//      row.current !== 'ignore' && this.dataMap.has(i) ? i : -1,
//    )
//    .filter((i) => i !== -1);
//
//  this.requestUpdate();
//}
//
// private updateRowFromStream = (data: Object) => {
//   if (this.mode === 'pair') {
//     const parsed = GeneralPair.safeParse(data);
//
//     if (parsed.success) {
//       const index = this.nameList.findIndex((nle) => {
//         if (!nle.primary.name.localeCompare(parsed.data.primary.name)) {
//           return !(nle as GeneralPair).secondary.name.localeCompare(
//             parsed.data.secondary.name,
//           );
//         }
//         return false;
//       });
//       if (DEBUG) {
//         console.log(
//           `updating row ${this.nameList[index].primary.name}/${(this.nameList[index] as GeneralPair).secondary.name}`,
//         );
//       }
//       if (index >= 0) {
//         this.nameList[index] = parsed.data;
//         this.nameList[index].current = 'current';
//       }
//     } else {
//       console.error(parsed.error.message);
//       throw new Error(parsed.error.message);
//     }
//   }
//   this.updateTableData();
// };
//
//private invalidateData() {
//  for (let i = 0; i < this.nameList.length; i++) {
//    //const name = this.nameList[i].primary.name;
//    const selected = new Array<string>();
//    //const selected = this.TableDataController.primarySettings.selectedGenerals
//    //  .get()
//    //  .find((n) => n === name);
//    if (selected) {
//      if (this.nameList[i].current !== 'stale') {
//        this.nameList[i].current = 'stale';
//      }
//    } else {
//      this.nameList[i].current = 'ignore';
//    }
//  }
//  this.requestUpdate();
//}

//<table class="spectrum-Table-main">
//
//  <thead class="spectrum-Table-head">
//
//    ${repeat(
//
//      table.getHeaderGroups(),
//
//      (hg) => hg.id,
//
//      (hg) =>
//
//        html`<tr>
//
//          ${repeat(
//
//            hg.headers,
//
//            (h) => h.id,
//
//            (h) =>
//
//              html`<th
//
//                colspan=${h.colSpan}
//
//                class="spectrum-Table-headCell is-sortable"
//
//              >
//
//                ${h.isPlaceholder
//
//                  ? null
//
//                  : html`<div
//
//                      @click=${h.column.getToggleSortingHandler()}
//
//                    >
//
//                      ${flexRender(
//
//                        h.column.columnDef.header,
//
//                        h.getContext(),
//
//                      )}
//
//                    </div>`}
//
//              </th>`,
//
//          )}
//
//        </tr>`,
//
//    )}
//
//  </thead>
//
//  <tbody class="spectrum-Table-body">
//
//    ${repeat(
//
//      table.getRowModel().rows,
//
//      (row) => row.id,
//
//      (row, index) => {
//
//        const originalIndex = this._visibleIndexMap[index];
//
//        const currentIndex = index + 1;
//
//        const rowState =
//
//          this.nameList[originalIndex]?.current ?? 'current';
//
//        const title =
//
//          rowState === 'stale'
//
//            ? 'Showing previous values; updating…'
//
//            : rowState === 'pending'
//
//              ? 'Refreshing…'
//
//              : rowState === 'error'
//
//                ? 'Failed to refresh; will retry on next change'
//
//                : '';
//
//        return html`<tr
//
//          class="spectrum-Table-row ${rowState}"
//
//          title=${title}
//
//        >
//
//          ${repeat(
//
//            row.getVisibleCells(),
//
//            (cell) => cell.id,
//
//            (cell) =>
//
//              cell.column.id === 'rowIndex'
//
//                ? html`<span
//
//                    class="index-column spectrum-Body spectrum-Body--sizeM"
//
//                  >
//
//                    ${currentIndex}
//
//                  </span>`
//
//                : html`<td class="spectrum-Table-cell">
//
//                    ${flexRender(
//
//                      cell.column.columnDef.cell,
//
//                      cell.getContext(),
//
//                    )}
//
//                  </td>`,
//
//          )}
//
//        </tr>`;
//
//      },
//
//    )}
//
//  </tbody>
//
//</table>
//

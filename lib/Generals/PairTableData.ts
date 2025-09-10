import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import {
  html,
  LitElement,
  type PropertyValues,
  type TemplateResult,
} from 'lit';
import { customElement, property } from 'lit/decorators.js';
import { consume } from '@lit/context';

import { signal, SignalWatcher, type Signal } from '@lit-labs/signals';

import { reaction } from 'signal-utils/subtle/reaction';

import {
  type PairState,
  type PairBuffFilter,
  filterContext,
  rowContext,
} from './PairTableState';

//import { GeneralPair, GeneralPairStub } from './GeneralRowSchemas';

@customElement('table-data')
export class PairTableData extends SignalWatcher(LitElement) {
  @consume({ context: filterContext, subscribe: true })
  @property({ attribute: false })
  private filterState?: PairBuffFilter;

  @consume({ context: rowContext, subscribe: true })
  @property({ attribute: false })
  private rowState?: PairState;

  private updateDebounceTimer: NodeJS.Timeout | undefined;
  //private currentEventSource: EventSource | undefined;

  protected baseUrl: URL = new URL('http://localhost');

  protected stateParams: Signal.State<URLSearchParams> = signal(
    new URLSearchParams(),
  );

  public async hostConnected() {
    if (DEBUG) {
      console.log(`PairTableData hostConnected `);
    }
    this.baseUrl = new URL(document.URL);

    //const stubData = await this.fetchStubPairs();
    //this.rowState!.master_list = [...stubData];
  }

  override firstUpdated() {
    // ensure we get the details at least once,
    // even if hte form is never touched.
    if (!this.updateDebounceTimer && this.rowState) {
      if (this.rowState.rowFetchRunId.get() == 0) {
        //this.getRowDetails();
      }
    }
  }

  protected override updated(_changedProperties: PropertyValues): void {
    super.updated(_changedProperties);
    // update the host's url parameters as needed
    const params = this.stateParams.get();
    if (params.toString().length) {
      const newURL = `${this.baseUrl}?${params.toString()}`;
      window.history.replaceState(null, '', newURL);
    }
  }

  hostDisconnected(): void {
    if (this.updateDebounceTimer) {
      clearTimeout(this.updateDebounceTimer);
    }
  }

  readonly urlParams = reaction(
    () => {
      return {
        primary: {
          ascendingLevel: this.filterState?.primary.ascendingLevel.get(),
          covenantLevel: this.filterState?.primary.covenantLevel.get(),
          specialtyLevel1: this.filterState?.primary.specialtyLevel1.get(),
          specialtyLevel2: this.filterState?.primary.specialtyLevel2.get(),
          specialtyLevel3: this.filterState?.primary.specialtyLevel3.get(),
          specialtyLevel4: this.filterState?.primary.specialtyLevel4.get(),
        },
        secondary: {
          covenantLevel: this.filterState?.secondary.covenantLevel.get(),
          specialtyLevel1: this.filterState?.secondary.specialtyLevel1.get(),
          specialtyLevel2: this.filterState?.secondary.specialtyLevel2.get(),
          specialtyLevel3: this.filterState?.secondary.specialtyLevel3.get(),
          specialtyLevel4: this.filterState?.secondary.specialtyLevel4.get(),
        },
      };
    },
    (
      value: Record<string, unknown>,
      previousValue: Record<string, unknown>,
    ) => {
      const params = new URLSearchParams();
      const primary = value['primary'] as Record<string, unknown>;
      const secondary = value['secondary'] as Record<string, unknown>;

      params.set('ascendingLevel', primary['ascendingLevel'] as string);
      params.set('primaryCovenantLevel', primary['covenantLevel'] as string);
      params.set('primarySpecialty1', primary['specialtyLevel1'] as string);
      params.set('primarySpecialty2', primary['specialtyLevel2'] as string);
      params.set('primarySpecialty3', primary['specialtyLevel3'] as string);
      params.set('primarySpecialty4', primary['specialtyLevel4'] as string);
      params.set(
        'secondaryCovenantLevel',
        secondary['covenantLevel'] as string,
      );
      params.set('secondarySpecialty1', secondary['specialtyLevel1'] as string);
      params.set('secondarySpecialty2', secondary['specialtyLevel2'] as string);
      params.set('secondarySpecialty3', secondary['specialtyLevel3'] as string);
      params.set('secondarySpecialty4', secondary['specialtyLevel4'] as string);
      this.stateParams.set(params);
    },
  );

  public urlConversion = (scope: 'data' | 'row' | 'details') => {
    let basePath = window.location.pathname.replace(/\/$/, '');

    basePath = decodeURIComponent(basePath);

    if (scope === 'data') {
      basePath = basePath.replace('pair-comparison', 'pair/data.json');
    } else if (scope === 'details') {
      const runId = this.rowState!.rowFetchRunId.get();
      this.rowState!.rowFetchRunId.set(runId + 1);
      basePath = basePath.replace(
        'pair-comparison',
        `${runId}/pair-details-stream`,
      );
    } else {
      basePath = basePath.replace('pair-comparison', 'pair-row.json');
    }
    if (DEBUG) {
      console.log(`urlConversion returning ${basePath}`);
    }
    return basePath;
  };

  public render(): TemplateResult {
    if (DEBUG) {
      console.log(`PairTableData render called`);
    }
    return html` <div class="hidden">
        <h3>Filter Settings</h3>
        <ul>
          <li>
            Primary:
            ${this.filterState
              ? html`
                  <ul>
                    <li>
                      ascendingLevel:
                      ${this.filterState.primary.ascendingLevel.get()}
                    </li>
                    <li>
                      covenantLevel:
                      ${this.filterState.primary.covenantLevel.get()}
                    </li>
                    <li>
                      specialtyLevel1:
                      ${this.filterState.primary.specialtyLevel1.get()}
                    </li>
                    <li>
                      specialtyLevel2:
                      ${this.filterState.primary.specialtyLevel2.get()}
                    </li>
                    <li>
                      specialtyLevel3:
                      ${this.filterState.primary.specialtyLevel3.get()}
                    </li>
                    <li>
                      specialtyLevel4:
                      ${this.filterState.primary.specialtyLevel4.get()}
                    </li>
                  </ul>
                `
              : DEBUG
                ? html`<span>Undefined Filter State</span>`
                : ''}
          </li>
          <li>
            Secondary:
            ${this.filterState
              ? html`
                  <ul>
                    <li>
                      covenantLevel:
                      ${this.filterState.secondary.covenantLevel.get()}
                    </li>
                    <li>
                      specialtyLevel1:
                      ${this.filterState.secondary.specialtyLevel1.get()}
                    </li>
                    <li>
                      specialtyLevel2:
                      ${this.filterState.secondary.specialtyLevel2.get()}
                    </li>
                    <li>
                      specialtyLevel3:
                      ${this.filterState.secondary.specialtyLevel3.get()}
                    </li>
                    <li>
                      specialtyLevel4:
                      ${this.filterState.secondary.specialtyLevel4.get()}
                    </li>
                  </ul>
                `
              : DEBUG
                ? html`<span>Undefined Filter State</span>`
                : ''}
          </li>
        </ul>
        <h3>Row State</h3>
        ${this.rowState
          ? html`
              <ul>
                <li>total count: ${this.rowState.total_count.get()}</li>
                <li>names Revision: ${this.rowState.namesRev.get()}</li>
                <li>row Fetch Run ID: ${this.rowState.rowFetchRunId.get()}</li>
                <li>selected count: ${this.rowState.selected_count.get()}</li>
              </ul>
            `
          : DEBUG
            ? html`<span>Undefined Row State</span>`
            : ''}
      </div>
      <slot></slot>`;
  }
}

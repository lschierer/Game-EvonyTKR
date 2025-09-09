import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import z from 'zod';
import { html, LitElement, type TemplateResult } from 'lit';
import { type ReactiveController, type ReactiveControllerHost } from 'lit';

import { signal, computed, type Signal } from '@lit-labs/signals';

import { LevelSettings } from '../partials/level_settings_form';
import {
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  SpecialtyLevelValues,
} from '../Game/EvonyTKR/Shared/Constants';

import { GeneralPair, GeneralPairStub, RowStub } from './GeneralRowSchemas';

export class PairTableData implements ReactiveController {
  host: ReactiveControllerHost;

  constructor(host: ReactiveControllerHost) {
    (this.host = host).addController(this);
  }

  readonly primarySettings: LevelSettings = new LevelSettings();
  readonly secondarySettings: LevelSettings = new LevelSettings();

  private updateDebounceTimer: NodeJS.Timeout | undefined;
  private currentEventSource: EventSource | undefined;
  private dataRunId: number = 0;

  private _master_row_list: RowStub[] = new Array<RowStub>();
  protected row_count: Signal.State<number> = signal(0);
  private _row_data: GeneralPair[] = new Array<GeneralPair>();

  public rows = computed(() => {
    const r = new Array<GeneralPair>();

    this.primarySettings.selectedGenerals.get().forEach((name) => {
      r.push(
        ...this._row_data.filter((gp) => !gp.primary.name.localeCompare(name)),
      );
    });
    return r;
  });

  get master_row_list(): RowStub[] {
    return [...this._master_row_list];
  }

  protected baseUrl: URL = new URL('http://localhost');

  public async hostConnected() {
    if (DEBUG) {
      console.log(`PairTableData hostConnected `);
    }
    this.baseUrl = new URL(document.URL);

    if (!this.primarySettings) {
      console.error('setup must be called immediately!!');
      return;
    }

    this.loadFromUrl();

    const stubData = await this.fetchStubPairs();
    this._master_row_list = [...stubData];
    this.row_count.set(this._master_row_list.length);
    const primaries = Array.from(
      new Map(stubData.map((sd) => [sd.primary.name, true])).keys(),
    );
    this.primarySettings.generals = primaries;
    this.primarySettings.requestUpdate();

    (this.host as LitElement).addEventListener('form_updated', (e) => {
      if (DEBUG) {
        console.log(`PairTableData got a form_updated event`);
      }
      this.updateDebounceTimer = setTimeout(() => {
        this.getRowDetails();
        clearTimeout(this.updateDebounceTimer);
      }, 300); // 300ms debounce
    });
    window.addEventListener('popstate', this.handlePopState);
  }

  hostUpdate() {
    // ensure we get the details at least once,
    // even if hte form is never touched.
    if (!this.updateDebounceTimer && this.dataRunId == 0) {
      this.getRowDetails();
    }

    // update the host's url parameters as needed
    const params = this.urlParams.get();
    const newURL = `${this.baseUrl}?${params.toString()}`;
    window.history.replaceState(null, '', newURL);
  }

  hostDisconnected(): void {
    if (this.updateDebounceTimer) {
      clearTimeout(this.updateDebounceTimer);
    }
  }

  protected handlePopState = () => {
    this.loadFromUrl();
  };

  readonly loadFromUrl = () => {
    const params = new URLSearchParams(window.location.search);

    const setParam = <T>(
      paramName: string,
      validator: z.ZodType<T>,
      signal: Signal.State<T>,
    ) => {
      if (params.has(paramName)) {
        const valid = validator.safeParse(params.get(paramName));
        if (valid.success) {
          signal.set(valid.data);
        }
      }
    };

    setParam(
      'ascendingLevel',
      AscendingAttributeLevelValues,
      this.primarySettings!.ascendingLevel,
    );
    setParam(
      'primaryCovenantLevel',
      CovenantCategoryValues,
      this.primarySettings!.covenantLevel,
    );
    setParam(
      'primarySpecialty1',
      SpecialtyLevelValues,
      this.primarySettings!.specialtyLevel1,
    );
    setParam(
      'primarySpecialty2',
      SpecialtyLevelValues,
      this.primarySettings!.specialtyLevel2,
    );
    setParam(
      'primarySpecialty3',
      SpecialtyLevelValues,
      this.primarySettings!.specialtyLevel3,
    );
    setParam(
      'primarySpecialty4',
      SpecialtyLevelValues,
      this.primarySettings!.specialtyLevel4,
    );

    setParam(
      'secondaryCovenantLevel',
      CovenantCategoryValues,
      this.secondarySettings!.covenantLevel,
    );
    setParam(
      'secondarySpecialty1',
      SpecialtyLevelValues,
      this.secondarySettings!.specialtyLevel1,
    );
    setParam(
      'secondarySpecialty2',
      SpecialtyLevelValues,
      this.secondarySettings!.specialtyLevel2,
    );
    setParam(
      'secondarySpecialty3',
      SpecialtyLevelValues,
      this.secondarySettings!.specialtyLevel3,
    );
    setParam(
      'secondarySpecialty4',
      SpecialtyLevelValues,
      this.secondarySettings!.specialtyLevel4,
    );
  };

  private getRowDetails = () => {
    const url = this.urlConversion('details');
    const params = this.urlParams.get();
    if (DEBUG) {
      console.log(`getRowDetails url ${url}?${params.toString()}`);
    }
    if (this.currentEventSource) {
      this.currentEventSource.removeEventListener('row', this.rowDetailHandler);
      this.currentEventSource.close();
    }
    this.currentEventSource = new EventSource(`${url}?${params.toString()}`);
    this.currentEventSource.addEventListener('row', this.rowDetailHandler);
  };

  private rowDetailHandler = (e: Event | MessageEvent) => {
    const data = JSON.parse((e as MessageEvent).data) as Record<
      string,
      unknown
    >;
    const runId = data['runId'] as number;
    const valid = GeneralPair.safeParse(data['data']);
    if (DEBUG) {
      console.log(
        `rowDetailHandler got a ${valid.success ? 'valid' : 'invalid'} GeneralPair from ${runId}; raw: ${JSON.stringify(data)}`,
      );
    }
    if (valid.success) {
      const index = this._master_row_list.findIndex((gs) => {
        if (!gs.primary.name.localeCompare(valid.data.primary.name)) {
          if (
            !(gs as GeneralPair).secondary.name.localeCompare(
              valid.data.secondary.name,
            )
          ) {
            return true;
          }
        }
        return false;
      });
      if (index >= 0) {
        if (runId === this.dataRunId) {
          this._master_row_list[index].current = 'current';
          this._master_row_list[index].runId = runId;
          this._row_data[index] = valid.data;
          this.host.requestUpdate();
        } else if (runId < this.dataRunId) {
          if (
            !this._master_row_list[index].runId ||
            runId < this._master_row_list[index].runId
          ) {
            this._master_row_list[index].current = 'stale';
            this._master_row_list[index].runId = runId;
            this._row_data[index] = valid.data;
            this.host.requestUpdate();
          }
        }
      }
    }
  };

  private fetchStubPairs: () => Promise<RowStub[]> = async () => {
    const url = this.urlConversion('data');

    if (DEBUG) {
      console.log(`[fetchStubPairs] URL: ${url}`);
    }

    const res = await fetch(url);
    const json = (await res.json()) as object;

    const result = GeneralPairStub.array().safeParse(
      json['data' as keyof typeof json],
    );

    if (result.success) return result.data;

    console.error('[fetchStubPairs] Stub validation failed', result.error);
    return [];
  };

  private urlParams = computed(() => {
    const params = new URLSearchParams();

    if (this.primarySettings) {
      params.set('ascendingLevel', this.primarySettings.ascendingLevel.get());
      params.set(
        'primaryCovenantLevel',
        this.primarySettings.covenantLevel.get(),
      );
      let p = 'primarySpecialty1';
      params.set(p, this.primarySettings.specialtyLevel1.get());
      p = 'primarySpecialty2';
      params.set(p, this.primarySettings.specialtyLevel2.get());
      p = 'primarySpecialty3';
      params.set(p, this.primarySettings.specialtyLevel3.get());
      p = 'primarySpecialty4';
      params.set(p, this.primarySettings.specialtyLevel4.get());
    } else {
      if (DEBUG) {
        console.error('this.primarySettings is undefined');
      }
    }

    params.set(
      'secondaryCovenantLevel',
      this.secondarySettings.covenantLevel.get(),
    );
    let p = 'secondarySpecialty1';
    params.set(p, this.secondarySettings.specialtyLevel1.get());
    p = 'secondarySpecialty2';
    params.set(p, this.secondarySettings.specialtyLevel2.get());
    p = 'secondarySpecialty3';
    params.set(p, this.secondarySettings.specialtyLevel3.get());
    p = 'secondarySpecialty4';
    params.set(p, this.secondarySettings.specialtyLevel4.get());

    return params;
  });

  private urlConversion = (scope: 'data' | 'row' | 'details') => {
    let basePath = window.location.pathname.replace(/\/$/, '');

    basePath = decodeURIComponent(basePath);

    if (scope === 'data') {
      basePath = basePath.replace('pair-comparison', 'pair/data.json');
    } else if (scope === 'details') {
      basePath = basePath.replace(
        'pair-comparison',
        `${this.dataRunId++}/pair-details-stream`,
      );
    } else {
      basePath = basePath.replace('pair-comparison', 'pair-row.json');
    }
    if (DEBUG) {
      console.log(`urlConversion returning ${basePath}`);
    }
    return basePath;
  };

  public display(): TemplateResult {
    if (DEBUG) {
      console.log(`PairTableData display called`);
    }
    return html` ${this.primarySettings} ${this.secondarySettings} `;
  }
}

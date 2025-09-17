import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { Store } from '@tanstack/store';
import z from 'zod';

import { GeneralData, SingleGeneralState } from '../GeneralRowSchemas';

const GSA = SingleGeneralState.array();
type GSA = z.infer<typeof GSA>;

type GeneralRecord = Record<string, SingleGeneralState>;

interface GeneralState {
  // Catalog sent first by the server as “stubs”
  catalog: Array<SingleGeneralState>;
  catalogRev: number; // bump when catalog changes (to signal menus)

  // Fast lookup for rows (both stub-only and full data)
  rows: GeneralRecord;

  // Streaming run control
  runId: number;
  streaming: 'idle' | 'open' | 'closing';
}

export class GeneralStore {
  readonly sessionId: Store<string> = new Store<string>('');
  protected _currentES?: EventSource;

  readonly store: Store<GeneralState> = new Store<GeneralState>(
    {
      catalog: [],
      catalogRev: 0,
      rows: {},
      runId: 0,
      streaming: 'idle',
    },
    {
      updateFn: (prev) => (updater) => {
        const next =
          typeof updater === 'function'
            ? (updater as (p: GeneralState) => GeneralState)(prev)
            : updater;

        return next;
      },
    },
  );

  public get currentES() {
    return this._currentES;
  }

  public subscribe(cb: () => void) {
    return this.store.subscribe(cb);
  }

  public setCatalog(stubs: GSA) {
    this.store.setState((prev) => {
      if (!stubs) {
        if (DEBUG) {
          console.error('stubs not defined in setCatalog!!');
        }
        return prev;
      }
      if (!Array.isArray(stubs)) {
        if (DEBUG) {
          console.error(`stubs is not an array, ${JSON.stringify(stubs)}`);
        }
        return prev;
      }
      const catalog: GSA = stubs.map((s) => ({
        primary: s.primary,
      }));

      // Build/refresh rows map from stubs; keep any existing data if keys match
      const rows: GeneralRecord = { ...prev.rows };
      for (const { primary } of catalog) {
        const existing = rows[primary];
        rows[primary] = existing
          ? { ...existing, primary }
          : { primary, state: 'stale' };
      }

      return {
        ...prev,
        catalog,
        catalogRev: prev.catalogRev + 1,
        rows,
        receivedCount: 0, // will increment as full rows arrive
      } as GeneralState;
    });
  }

  public updateCatalog = async (selectedPrimaries: string[] = []) => {
    const isUpdate: boolean = selectedPrimaries.length !== 0;

    let path = window.location.pathname;
    path = path.replace('comparison', 'data.json');
    const catalogUrl = new URL(path, window.location.toString());
    if (DEBUG) {
      console.log(`using catalog Url ${catalogUrl.toString()}`);
    }
    const response = await fetch(catalogUrl, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ primaries: selectedPrimaries }),
    });
    if (!response.ok) {
      if (DEBUG) {
        console.error('error getting catalog: ', response.statusText);
      }
    }
    const ro = (await response.json()) as {
      sessionId: string;
      selected: GSA;
    };

    const valid = GSA.safeParse(ro.selected);
    if (!valid.success) {
      console.error('invalid catalog payload');
      return;
    }
    this.sessionId.setState(ro.sessionId);
    const sp = new Set<string>();
    if (valid.success) {
      valid.data.map((s) => {
        sp.add(s.primary);
      });
    }

    if (!isUpdate) {
      this.setCatalog(valid.data);
    }
  };

  public getCatalog = () => {
    if (!this.store.state.catalog || this.store.state.catalog.length === 0) {
      this.updateCatalog().then(() => {
        return [...this.store.state.catalog];
      });
    }
    return [...this.store.state.catalog];
  };

  public toggleIgnoreState(primaryName: string) {
    const key = primaryName;
    const row = this.store.state.rows[key];
    if (row) {
      if (row.state === 'ignore') {
        row.state = 'stale';
      } else {
        row.state = 'ignore';
      }
    }
    const rows = {
      ...this.store.state.rows,
      [key]: row,
    };
    this.store.setState((prev) => {
      return {
        ...prev,
        rows,
      } as GeneralState;
    });
  }

  public toggleAllIgnoredForPrimary(primaryName: string) {
    if (DEBUG) {
      console.log(`toggling all for ${primaryName}`);
    }
    const rows = this.store.state.catalog.filter((ce) => {
      if (!ce.primary.localeCompare(primaryName)) {
        return true;
      }
      return false;
    });
    if (DEBUG) {
      console.log(`toggling ${rows.length} for ${primaryName}`);
    }
    rows.forEach((row) => {
      this.toggleIgnoreState(row.primary);
    });
  }

  upsertRowFromStream(runId: number, payload: unknown) {
    if (DEBUG) {
      console.log('upsertRowFromStream called with runId:', runId);
    }
    const parsed = GeneralData.safeParse(payload);
    if (!parsed.success) {
      if (DEBUG) {
        console.error('invalid payload: ', parsed.error);
      }
      return;
    } else if (DEBUG) {
      console.log(`received row event: `, JSON.stringify(parsed.data));
    }
    const gp = parsed.data;
    const key = gp.primary.name;

    this.store.setState((prev: GeneralState) => {
      // Ignore old runs
      if (runId !== prev.runId) {
        if (DEBUG) {
          console.error(
            `runId ${runId} does not match previous id ${prev.runId}`,
          );
        }
        return prev;
      }

      let rows;
      const old = prev.rows[key];
      if (old) {
        if (old.state !== 'ignore') {
          old.state = 'current';
        }

        old.data = gp;
        rows = {
          ...prev.rows,
          [key]: old,
        };
      } else {
        const baseRow: SingleGeneralState = {
          primary: gp.primary.name,
          state: 'current',
          data: gp,
        };
        rows = {
          ...prev.rows,
          [key]: baseRow,
        };
      }

      return { ...prev, rows } as GeneralState;
    });
  }

  protected beginRun(newRunId: number) {
    this.store.setState((prev) => ({
      ...prev,
      runId: newRunId,
      streaming: 'open',
      receivedCount: 0,
      // expectedCount stays as set by latest catalog;
      // if catalog arrives *after* beginRun, setCatalog will update it
    }));
    // Optionally set all non-ignored rows to 'pending'
    this.store.setState((prev) => {
      const rows = { ...prev.rows };
      for (const k in rows) {
        if (rows[k].state !== 'ignore')
          rows[k] = { ...rows[k], state: 'pending' };
      }
      return { ...prev, rows };
    });
  }

  protected endRun(runId: number) {
    this.store.setState((prev) =>
      prev.runId !== runId ? prev : { ...prev, streaming: 'idle' },
    );
  }

  protected openDetailsStream(
    url: string,
    params: URLSearchParams,
    sessionId: string,
  ) {
    // Create a new monotonic runId
    const runId = Math.max(Date.now(), this.store.state.runId + 1);
    this.beginRun(runId);
    params.set('runId', `${runId}`);
    params.set('sessionId', sessionId);

    const sourceUrl = `${url}?${params.toString()}`;
    if (DEBUG) {
      console.log(`sourceUrl is ${sourceUrl}`);
    }
    const es = new EventSource(sourceUrl);

    if (DEBUG) {
      console.log('EventSource created for:', sourceUrl);
    }

    es.onopen = (event) => {
      if (DEBUG) {
        console.log('EventSource opened:', event);
      }
    };

    es.onerror = (event) => {
      if (DEBUG) {
        console.error('EventSource error:', event);
      }
      es.close();
      this.endRun(runId);
    };

    es.onmessage = (event) => {
      if (DEBUG) {
        console.log('EventSource received generic message:', event.data);
      }
    };

    es.addEventListener('row', (e: MessageEvent) => {
      const jsonString = atob(e.data);
      const msg = JSON.parse(jsonString);
      if (DEBUG) {
        console.log('parsed row message:', msg);
      }
      if (msg.runId !== runId) {
        if (DEBUG) {
          console.warn(`row event runId mismatch: ${msg.runId} !== ${runId}`);
        }
        return; // guard
      }
      if (DEBUG) {
        console.log('calling upsertRowFromStream with:', msg.data);
      }
      this.upsertRowFromStream(runId, msg.data);
    });

    es.addEventListener('complete', () => {
      es.close();
      this.endRun(runId);
    });

    if (DEBUG) {
      es.addEventListener('message', (e) => {
        console.log(`message event handler`, e.data);
      });
    }

    return es; // keep a ref to close on filter changes
  }

  public restartStream(params: URLSearchParams) {
    if (DEBUG) {
      console.log(
        'restartStream called, current ES exists:',
        !!this._currentES,
        ' state is ',
        this._currentES?.readyState,
      );
      console.trace('restartStream call stack');
    }

    let sp = window.location.pathname;
    sp = sp.replace('comparison', '1/details-stream');
    const streamUrl = new URL(sp, window.location.toString());

    if (!!this._currentES) {
      if (DEBUG) {
        console.log(
          'Closing existing EventSource, state is ',
          this._currentES?.readyState,
        );
      }
      this._currentES.close();
    }
    this._currentES = this.openDetailsStream(
      streamUrl.toString(),
      params,
      this.sessionId.state,
    );
  }
}

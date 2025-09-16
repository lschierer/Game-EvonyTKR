import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { Store } from '@tanstack/store';
import z from 'zod';

import { GeneralPair, GeneralPairStub } from '../GeneralRowSchemas';

const GPSA = GeneralPairStub.array();
type GPSA = z.infer<typeof GPSA>;

// Stable row key: "Primary ␟ Secondary" (use a char you're sure won’t appear in names)
const SEP = '\u241F'; // SYMBOL FOR UNIT SEPARATOR
export const pairKey = (p: string, s: string) => `${p}${SEP}${s}`;

export type Key = string;

export type RowState = 'stale' | 'pending' | 'current' | 'error' | 'ignore';
export interface Stub {
  primary: string;
  secondary: string;
}

export interface RowEntry {
  key: Key;
  primary: string;
  secondary: string;
  state: RowState; // UI state
  data?: GeneralPair; // present once fetched/validated
}

interface PairsState {
  // Catalog sent first by the server as “stubs”
  catalog: Array<Stub>;
  catalogRev: number; // bump when catalog changes (to signal menus)

  // Fast lookup for rows (both stub-only and full data)
  rows: Record<Key, RowEntry>;

  // Streaming run control
  runId: number;
  streaming: 'idle' | 'open' | 'closing';
}

export class PairStore {
  readonly sessionId: Store<string> = new Store<string>('');
  protected _currentES?: EventSource;

  readonly store: Store<PairsState> = new Store<PairsState>(
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
            ? (updater as (p: PairsState) => PairsState)(prev)
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

  public setCatalog(stubs: GPSA) {
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
      const catalog = stubs.map((s) => ({
        primary: s.primary.name,
        secondary: s.secondary.name,
      }));

      // Build/refresh rows map from stubs; keep any existing data if keys match
      const rows: Record<Key, RowEntry> = { ...prev.rows };
      for (const { primary, secondary } of catalog) {
        const key = pairKey(primary, secondary);
        const existing = rows[key];
        rows[key] = existing
          ? { ...existing, primary, secondary }
          : { key, primary, secondary, state: 'stale' };
      }

      return {
        ...prev,
        catalog,
        catalogRev: prev.catalogRev + 1,
        rows,
        receivedCount: 0, // will increment as full rows arrive
      } as PairsState;
    });
  }

  public getCatalog = async (url: string) => {
    const response = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ primaries: [] }),
    });

    if (!response.ok) {
      if (DEBUG) {
        console.error('error getting catalog: ', response.statusText);
      }
    }
    const ro = (await response.json()) as {
      sessionId: string;
      selected: GPSA;
    };

    this.sessionId.setState(ro.sessionId);
    const valid = GPSA.safeParse(ro.selected);
    if (!valid.success) {
      console.error('invalid catalog payload');
      return;
    }
    const sp = new Set<string>();
    if (valid.success) {
      valid.data.map((s) => {
        sp.add(s.primary.name);
      });
    }

    this.setCatalog(valid.data);
  };

  public togglePairIgnoreState(primaryName: string, secondaryName: string) {
    const key = pairKey(primaryName, secondaryName);
    const pair = this.store.state.rows[key];
    if (pair) {
      if (pair.state === 'ignore') {
        pair.state = 'stale';
      } else {
        pair.state = 'ignore';
      }
    }
  }

  public toggleAllIgnoredForPrimary(primaryName: string) {
    const pairs = this.store.state.catalog.filter((ce) => {
      if (!ce.primary.localeCompare(primaryName)) {
        return true;
      }
      return false;
    });
    pairs.forEach((pair) => {
      this.togglePairIgnoreState(pair.primary, pair.secondary);
    });
  }

  upsertRowFromStream(runId: number, payload: unknown) {
    if (DEBUG) {
      console.log('upsertRowFromStream called with runId:', runId);
    }
    const parsed = GeneralPair.safeParse(payload);
    if (!parsed.success) {
      if (DEBUG) {
        console.error('invalid payload: ', parsed.error);
      }
      return;
    } else if (DEBUG) {
      console.log(`received row event: `, JSON.stringify(parsed.data));
    }
    const gp = parsed.data;
    const key = pairKey(gp.primary.name, gp.secondary.name);

    this.store.setState((prev: PairsState) => {
      // Ignore old runs
      if (runId !== prev.runId) {
        if (DEBUG) {
          console.error(
            `runId ${runId} does not match previous id ${prev.runId}`,
          );
        }
        return prev;
      }

      const old = prev.rows[key];
      // If row is not in catalog yet (rare ordering), create it
      const baseRow: RowEntry = old ?? {
        key,
        primary: gp.primary.name,
        secondary: gp.secondary.name,
        state: 'stale',
      };

      // Apply selection: if primary not selected, keep ignore
      const state: RowState = 'current';

      const rows = {
        ...prev.rows,
        [key]: { ...baseRow, data: gp, state },
      };

      return { ...prev, rows } as PairsState;
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

  public openPairsStream(
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

    es.addEventListener('pair', (e: MessageEvent) => {
      if (DEBUG) {
        console.log('received pair event:', e.data);
      }
      const jsonString = atob(e.data);
      const msg = JSON.parse(jsonString);
      if (DEBUG) {
        console.log('parsed pair message:', msg);
      }
      // Suggested shape for server event: { runId: number, data: GeneralPair }
      if (msg.runId !== runId) {
        if (DEBUG) {
          console.warn(`pair event runId mismatch: ${msg.runId} !== ${runId}`);
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
      );
      console.trace('restartStream call stack');
    }

    let sp = window.location.pathname;
    sp = sp.replace('-comparison', '-details-stream');
    const streamUrl = new URL(sp, window.location.toString());

    if (this._currentES) {
      if (DEBUG) {
        console.log('Closing existing EventSource');
      }
      this._currentES.close();
    }
    this._currentES = this.openPairsStream(
      streamUrl.toString(),
      params,
      this.sessionId.state,
    );
  }
}

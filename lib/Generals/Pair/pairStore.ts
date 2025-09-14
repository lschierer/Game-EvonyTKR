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

  // Selected primaries (affects which pairs are “ignored” client-side)
  selectedPrimaries: Set<string>;
  selectionRev: number; // bump when selection changes

  // Streaming run control
  runId: number;
  streaming: 'idle' | 'open' | 'closing';
  expectedCount: number; // how many total rows we expect for this run (from catalog)
  receivedCount: number; // how many rows (full data) we have processed this run
}

export class PairStore {
  readonly sessionId: Store<string> = new Store<string>('');

  readonly store: Store<PairsState> = new Store<PairsState>(
    {
      catalog: [],
      catalogRev: 0,
      rows: {},
      selectedPrimaries: new Set<string>(),
      selectionRev: 0,
      runId: 0,
      streaming: 'idle',
      expectedCount: 0,
      receivedCount: 0,
    },
    {
      updateFn: (prev) => (updater) => {
        const next =
          typeof updater === 'function'
            ? (updater as (p: PairsState) => PairsState)(prev)
            : updater;

        // Normalize/defend invariants here if needed.
        // Example: never let receivedCount exceed expectedCount
        if (next.receivedCount > next.expectedCount && next.expectedCount > 0) {
          next.receivedCount = next.expectedCount;
        }
        return next;
      },
    },
  );

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

      // expectedCount is how many we *could* receive for this run (subject to selection)
      // We don’t filter here; filtering is applied at render/update time via state=ignore
      // If this is the first catalog (empty selectedPrimaries), select all primaries by default
      const selectedPrimaries =
        prev.selectedPrimaries.size === 0
          ? new Set(catalog.map((c) => c.primary))
          : prev.selectedPrimaries;

      const expectedCount =
        selectedPrimaries.size > 0
          ? catalog.filter((entry) => selectedPrimaries.has(entry.primary))
              .length
          : catalog.length;

      return {
        ...prev,
        catalog,
        catalogRev: prev.catalogRev + 1,
        rows,
        selectedPrimaries,
        expectedCount,
        receivedCount: 0, // will increment as full rows arrive
      };
    });
  }

  public setSelectedPrimaries(names: string[]) {
    this.store.setState((prev) => {
      const selectedPrimaries = new Set(names);
      // Recalculate expectedCount based on catalog entries with selected primaries
      const expectedCount = this.store.state.catalog.filter((entry) =>
        selectedPrimaries.has(entry.primary),
      ).length;

      return {
        ...prev,
        selectedPrimaries,
        selectionRev: prev.selectionRev + 1,
        expectedCount,
      };
    });
    this.recomputeIgnoreStates();
  }

  protected applySelectionToRows(base: PairsState): PairsState {
    const rows = { ...base.rows };
    const selected = base.selectedPrimaries;
    for (const k in rows) {
      const r = rows[k];
      rows[k] = {
        ...r,
        state: selected.has(r.primary)
          ? r.state === 'ignore'
            ? 'stale'
            : r.state // revive previously ignored rows
          : 'ignore',
      };
    }
    return { ...base, rows };
  }

  public recomputeIgnoreStates() {
    this.store.setState((prev) => this.applySelectionToRows(prev));
  }

  public beginRun(newRunId: number) {
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

  endRun(runId: number) {
    this.store.setState((prev) =>
      prev.runId !== runId ? prev : { ...prev, streaming: 'idle' },
    );
  }

  upsertRowFromStream(runId: number, payload: unknown) {
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

    this.store.setState((prev) => {
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
      const selected = prev.selectedPrimaries.has(baseRow.primary);
      const state: RowState = selected ? 'current' : 'ignore';

      const rows = {
        ...prev.rows,
        [key]: { ...baseRow, data: gp, state },
      };

      // Count only rows that are part of this run and not ignored
      const receivedCount =
        state !== 'ignore'
          ? Math.min(prev.receivedCount + 1, Math.max(prev.expectedCount, 0))
          : prev.receivedCount;

      return { ...prev, rows, receivedCount };
    });
  }

  public getCatalog = async (url: string) => {
    let p = new Array<string>();
    if (this.store.state.selectedPrimaries) {
      p = [...this.store.state.selectedPrimaries];
    }

    const response = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ primaries: p }),
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
    this.recomputeIgnoreStates();
  };

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

    es.addEventListener('pair', (e: MessageEvent) => {
      const msg = JSON.parse(e.data);
      // Suggested shape for server event: { runId: number, data: GeneralPair }
      if (msg.runId !== runId) return; // guard
      this.upsertRowFromStream(runId, msg.data);
    });

    es.addEventListener('complete', () => {
      es.close();
      this.endRun(runId);
    });

    es.onerror = () => {
      es.close();
      this.endRun(runId);
    };

    return es; // keep a ref to close on filter changes
  }
}

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

export interface RowEntry {
  key: Key;
  primary: string;
  secondary: string;
  state: RowState; // UI state
  data?: GeneralPair; // present once fetched/validated
}

interface PairsState {
  // Catalog sent first by the server as “stubs”
  catalog: Array<{ primary: string; secondary: string }>;
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
      return {
        ...prev,
        catalog,
        catalogRev: prev.catalogRev + 1,
        rows,
        expectedCount: catalog.length,
        receivedCount: 0, // will increment as full rows arrive
      };
    });
  }

  public setSelectedPrimaries(names: string[]) {
    this.store.setState((prev) => ({
      ...prev,
      selectedPrimaries: new Set(names),
      selectionRev: prev.selectionRev + 1,
    }));
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
      // expectedCount stays as set by latest catalog; if catalog arrives *after* beginRun, setCatalog will update it
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
      // You may want to log and mark a row error if you can compute a key from payload
      return;
    }
    const gp = parsed.data;
    const key = pairKey(gp.primary.name, gp.secondary.name);

    this.store.setState((prev) => {
      // Ignore old runs
      if (runId !== prev.runId) return prev;

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

  public openPairsStream(url: string) {
    // Create a new monotonic runId
    const runId = Math.max(Date.now(), this.store.state.runId + 1);
    this.beginRun(runId);

    const es = new EventSource(url);

    es.addEventListener('catalog', (e: MessageEvent) => {
      const payload = JSON.parse(e.data) as Array<{
        primary: { name: string };
        secondary: { name: string };
      }>;
      this.setCatalog(payload);
      this.recomputeIgnoreStates();
    });

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

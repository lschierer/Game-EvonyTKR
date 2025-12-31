// urlBinder.ts
type Readable<T> = { state: T; subscribe(cb: () => void): () => void };
//type Writable<T> = Readable<T> & { setState(v: T): void };

export type ParamRow = {
  key: string;
  get: () => string;
  set?: (v: string) => void; // omit if read-only
};

export class UrlBinder {
  private unsub: Array<() => void> = [];
  private writing = false;
  private applying = false;
  private writeTimer: number | undefined;

  constructor(
    private rows: ParamRow[],
    private debounceMs = 50,
  ) {}

  attach(storesToWatch: Readable<unknown>[]) {
    // Initial URL -> stores
    this.applyFromUrl();

    // Stores -> URL (debounced)
    for (const s of storesToWatch) {
      this.unsub.push(s.subscribe(() => this.queueWrite()));
    }

    // Back/forward support
    window.addEventListener('popstate', this.onPopstate);
  }

  dispose() {
    for (const u of this.unsub) u();
    this.unsub = [];
    window.removeEventListener('popstate', this.onPopstate);
    if (this.writeTimer) window.clearTimeout(this.writeTimer);
  }

  private onPopstate = () => this.applyFromUrl();

  private applyFromUrl() {
    this.applying = true;
    const params = new URLSearchParams(location.search);

    for (const row of this.rows) {
      if (!row.set) continue;
      if (!params.has(row.key)) continue;
      const v = params.get(row.key);
      if (v != null && v !== row.get()) row.set(v);
    }

    this.applying = false;
  }

  private queueWrite() {
    if (this.applying) return; // prevent feedback loop
    if (this.writing) return; // avoid re-entrancy during replaceState
    if (this.writeTimer) window.clearTimeout(this.writeTimer);
    this.writeTimer = window.setTimeout(
      () => this.writeToUrl(),
      this.debounceMs,
    );
  }

  private writeToUrl() {
    this.writing = true;

    const url = new URL(location.href);
    const params = url.searchParams;
    const before = params.toString();

    for (const row of this.rows) {
      const v = row.get();
      if (!v) params.delete(row.key);
      else params.set(row.key, v);
    }

    const after = params.toString();
    if (before !== after) {
      const next = after ? `${location.pathname}?${after}` : location.pathname;
      history.replaceState(null, '', next);
    }

    this.writing = false;
  }
}

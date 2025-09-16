import { Store } from '@tanstack/store';

interface PrimaryFilterState {
  selected: Set<string>;
  available: string[];
}

export class PrimaryFilterStore {
  readonly store = new Store<PrimaryFilterState>({
    selected: new Set(),
    available: [],
  });

  public subscribe(cb: () => void) {
    return this.store.subscribe(cb);
  }

  public setAvailable(primaries: string[]) {
    this.store.setState((prev) => {
      const newSelected = new Set(prev.selected);

      // Add any new primaries to selected (default to selected)
      for (const primary of primaries) {
        if (!prev.available.includes(primary)) {
          newSelected.add(primary);
        }
      }

      // Remove any primaries that are no longer available
      for (const selected of prev.selected) {
        if (!primaries.includes(selected)) {
          newSelected.delete(selected);
        }
      }

      return {
        ...prev,
        available: [...primaries].sort(),
        selected: newSelected,
      };
    });
  }

  public toggle(primaryName: string) {
    this.store.setState((prev) => {
      const newSelected = new Set(prev.selected);
      if (newSelected.has(primaryName)) {
        newSelected.delete(primaryName);
      } else {
        newSelected.add(primaryName);
      }
      return { ...prev, selected: newSelected };
    });
  }

  public isSelected(primaryName: string): boolean {
    return this.store.state.selected.has(primaryName);
  }

  public getSelected(): string[] {
    return Array.from(this.store.state.selected).sort();
  }

  public getAvailable(): string[] {
    return this.store.state.available;
  }
}

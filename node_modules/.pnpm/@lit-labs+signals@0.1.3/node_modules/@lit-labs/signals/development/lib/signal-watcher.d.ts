/**
 * @license
 * Copyright 2023 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
import type { ReactiveElement } from 'lit';
import { WatchDirective } from './watch.js';
type ReactiveElementConstructor = abstract new (...args: any[]) => ReactiveElement;
export interface SignalWatcher extends ReactiveElement {
    _updateWatchDirective(d: WatchDirective<unknown>): void;
    _clearWatchDirective(d: WatchDirective<unknown>): void;
}
/**
 * Adds the ability for a LitElement or other ReactiveElement class to
 * watch for access to signals during the update lifecycle and trigger a new
 * update when signals values change.
 */
export declare function SignalWatcher<T extends ReactiveElementConstructor>(Base: T): T;
export {};
//# sourceMappingURL=signal-watcher.d.ts.map
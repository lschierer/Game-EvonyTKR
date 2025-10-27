/**
 * @license
 * Copyright 2023 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
import { directive } from 'lit/directive.js';
import { AsyncDirective } from 'lit/async-directive.js';
import { Signal } from 'signal-polyfill';
export class WatchDirective extends AsyncDirective {
    __watch() {
        if (this.__watcher !== undefined) {
            return;
        }
        this.__computed = new Signal.Computed(() => {
            var _a;
            return (_a = this.__signal) === null || _a === void 0 ? void 0 : _a.get();
        });
        const watcher = (this.__watcher = new Signal.subtle.Watcher(() => {
            var _a;
            // TODO: If we're not running inside a SignalWatcher, we can commit to
            // the DOM independently.
            (_a = this.__host) === null || _a === void 0 ? void 0 : _a._updateWatchDirective(this);
            watcher.watch();
        }));
        watcher.watch(this.__computed);
    }
    __unwatch() {
        var _a;
        if (this.__watcher !== undefined) {
            this.__watcher.unwatch(this.__computed);
            this.__computed = undefined;
            this.__watcher = undefined;
            (_a = this.__host) === null || _a === void 0 ? void 0 : _a._clearWatchDirective(this);
        }
    }
    commit() {
        this.setValue(Signal.subtle.untrack(() => { var _a; return (_a = this.__computed) === null || _a === void 0 ? void 0 : _a.get(); }));
    }
    render(signal) {
        // This would only be called if render is called directly, like in SSR.
        return Signal.subtle.untrack(() => signal.get());
    }
    update(part, [signal]) {
        var _a, _b;
        (_a = this.__host) !== null && _a !== void 0 ? _a : (this.__host = (_b = part.options) === null || _b === void 0 ? void 0 : _b.host);
        if (signal !== this.__signal && this.__signal !== undefined) {
            // Unwatch the old signal
            this.__unwatch();
        }
        this.__signal = signal;
        this.__watch();
        // We use untrack() so that the signal access is not tracked by the watcher
        // created by SignalWatcher. This means that an can use both SignalWatcher
        // and watch() and a signal update won't trigger a full element update if
        // it's only passed to watch() and not otherwise accessed by the element.
        return Signal.subtle.untrack(() => this.__computed.get());
    }
    disconnected() {
        this.__unwatch();
    }
    reconnected() {
        this.__watch();
    }
}
/**
 * Renders a signal and subscribes to it, updating the part when the signal
 * changes.
 *
 * watch() can only be used in a reactive element that applies the
 * SignalWatcher mixin.
 */
export const watch = directive(WatchDirective);
//# sourceMappingURL=watch.js.map
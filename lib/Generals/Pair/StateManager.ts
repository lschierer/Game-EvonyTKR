import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { customElement } from 'lit/decorators.js';

import { type PairData } from './data';
import { LitElement, html, type TemplateResult } from 'lit';

@customElement('state-manager')
export class StateManager extends LitElement {
  protected data?: PairData;
  private restartTimer?: number;

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }

  // Debounce restartStream so synchronous cascades of setState
  // (from alien-signals in @tanstack/store 0.9.x) only trigger one restart.
  private scheduleRestart() {
    if (!this.data) return;
    const sid = this.data.pairStore.sessionId.state;
    if (!sid || sid.length <= 1) return;

    if (this.restartTimer) window.clearTimeout(this.restartTimer);
    this.restartTimer = window.setTimeout(() => {
      if (DEBUG) {
        console.log(`debounced restartStream, sid="${sid}"`);
      }
      this.data?.pairStore.restartStream(this.data.queryParams.state);
    }, 50);
  }

  connectedCallback(): void {
    super.connectedCallback();
    // pair-data should be a direct child of state-manager
    const qr = this.querySelector('pair-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as PairData;
    }
    if (this.data) {
      this.data.queryParams.subscribe(() => { this.scheduleRestart(); });

      this.data.pairStore.sessionId.subscribe(() => { this.scheduleRestart(); });
      let prevSelected = [...this.data.primaryFilter.store.state.selected].sort();
      this.data.primaryFilter.subscribe(() => {
        if (!this.data) return;
        const current = [...this.data.primaryFilter.store.state.selected].sort();

        let refresh: boolean = false;
        // if there are *less* rows, the change to the ignore state will be
        // picked up by a pairStore subscription
        // if the row count is the *same* I test if all the values are the same
        // if the values have changed, there is at least one pair I need new data
        // for, and I need to refresh.
        // if there are *more rows* then there *must be* new data needed.
        if (current.length > prevSelected.length) {
          refresh = true;
        } else if (current.length === prevSelected.length) {
          refresh = !current.every((value, index) => value === prevSelected[index]);
        }
        prevSelected = current;
        if (refresh) {
          void this.data.pairStore.updateCatalog([...current]);
        }
      });
    }
  }
}

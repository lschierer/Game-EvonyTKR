import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { customElement } from 'lit/decorators.js';

import { type SingleData } from './data';
import { LitElement, html, type TemplateResult } from 'lit';

@customElement('state-manager')
export class StateManager extends LitElement {
  protected data?: SingleData;
  private restartTimer?: number;

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }

  // Debounce restartStream so synchronous cascades of setState
  // (from alien-signals in @tanstack/store 0.9.x) only trigger one restart.
  private scheduleRestart() {
    if (!this.data) return;
    const sid = this.data.generalStore.sessionId.state;
    if (!sid || sid.length <= 1) return;

    if (this.restartTimer) window.clearTimeout(this.restartTimer);
    this.restartTimer = window.setTimeout(() => {
      if (DEBUG) {
        console.log(`debounced restartStream, sid="${sid}"`);
      }
      this.data?.generalStore.restartStream(this.data.queryParams.state);
    }, 50);
  }

  connectedCallback(): void {
    const qr = this.querySelector('single-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as SingleData;
    }
    if (this.data) {
      this.data.queryParams.subscribe(() => { this.scheduleRestart(); });

      this.data.generalStore.sessionId.subscribe(() => { this.scheduleRestart(); });
      let prevSelected = [...this.data.buffFilter.store.state.selected].sort();
      this.data.buffFilter.subscribe(() => {
        if (!this.data) return;
        const current = [...this.data.buffFilter.store.state.selected].sort();

        let refresh: boolean = false;
        // if there are *less* rows, the change to the ignore state will be
        // picked up by a singleStore subscription
        // if the row count is the *same* I test if all the values are the same
        // if the values have changed, there is at least one general I need new data
        // for, and I need to refresh.
        // if there are *more rows* then there *must be* new data needed.
        if (current.length > prevSelected.length) {
          refresh = true;
        } else if (current.length === prevSelected.length) {
          refresh = !current.every((value, index) => value === prevSelected[index]);
        }
        prevSelected = current;
        if (refresh && this.data) {
          this.data.generalStore.updateCatalog([...current]);
        }
      });
    }
  }
}

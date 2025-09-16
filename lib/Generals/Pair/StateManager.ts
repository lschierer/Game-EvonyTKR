import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { customElement } from 'lit/decorators.js';

import { PairData } from './data';
import { type PairPicker } from './PairPicker';
import { LitElement, html, type TemplateResult } from 'lit';

@customElement('state-manager')
export class StateManager extends LitElement {
  protected data?: PairData;

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }

  connectedCallback(): void {
    const qr = this.querySelector('pair-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as PairData;
    }
    if (this.data) {
      this.data.queryParams.subscribe(() => {
        if (this.data && this.data.pairStore.sessionId.state) {
          const sid = this.data.pairStore.sessionId.state;
          if (sid.length > 1) {
            if (DEBUG) {
              console.log(`sid is "${sid}"`);
            }
            this.data.pairStore.restartStream(this.data.queryParams.state);
          }
        }
      });

      this.data.pairStore.sessionId.subscribe(() => {
        if (this.data && this.data.pairStore.sessionId.state) {
          const sid = this.data.pairStore.sessionId.state;
          if (sid.length > 1) {
            if (DEBUG) {
              console.log(`sid is "${sid}"`);
            }
            this.data.pairStore.restartStream(this.data.queryParams.state);
          }
        }
      });
    }
    const gf = this.querySelector('pair-picker') as PairPicker | null;
    if (gf) {
      gf.selectedPrimaries.subscribe(() => {
        const current = gf.selectedPrimaries.state;
        const previous = gf.selectedPrimaries.prevState;
        current.sort();
        previous.sort();
        let refresh: boolean = false;
        // if there are *less* rows, the change to the ignore state will be
        // picked up by a pairStore subscription
        // if the row count is the *same* I test if all the values are the same
        // if the values have changed, there is at least one pair I need new data
        // for, and I need to refresh.
        // if there are *more rows* then there *must be* new data needed.
        if (current.length > previous.length) {
          refresh = true;
        } else if (current.length === previous.length) {
          refresh = !current.every((value, index) => value === previous[index]);
        }
        if (refresh && this.data) {
          this.data.pairStore.restartStream(this.data.queryParams.state);
        }
      });
    }
  }
}

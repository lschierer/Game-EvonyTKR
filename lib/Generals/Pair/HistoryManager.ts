import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { html, LitElement, type TemplateResult } from 'lit';
import { customElement } from 'lit/decorators.js';

import { PairData } from './data';

@customElement('history-manager')
export class HistoryManager extends LitElement {
  protected data?: PairData;

  connectedCallback(): void {
    super.connectedCallback();
    const qr = this.querySelector('pair-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as PairData;
    }
    if (this.data) {
      this.data.queryParams.subscribe(() => {
        this.requestUpdate();
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
  }

  protected override render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

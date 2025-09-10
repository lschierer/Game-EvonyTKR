import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { html, LitElement, type TemplateResult } from 'lit';
import { property } from 'lit/decorators.js';

import { Store } from '@tanstack/store';

import {
  AscendingAttributeLevelValues,
  AscendingOptions,
} from '../../Game/EvonyTKR/Shared/Constants';

export class PairData extends LitElement {
  readonly ascendingMap: AscendingOptions = {
    none: 'None',
    purple1: '1 Purple Star',
    purple2: '2 Purple Stars',
    purple3: '3 Purple Stars',
    purple4: '4 Purple Stars',
    purple5: '5 Purple Stars',
    red1: '1 Red Star',
    red2: '2 Red Stars',
    red3: '3 Red Stars',
    red4: '4 Red Stars',
    red5: '5 Red Stars',
  };

  @property({ attribute: false })
  public ascendingLevel = new Store('none', {
    onUpdate: () => {
      const valid = AscendingAttributeLevelValues.safeParse(
        this.ascendingLevel.state,
      );
      if (!valid.success) {
        if (DEBUG) {
          console.error('invalid ascending level', valid.error);
        }
        this.ascendingLevel.setState(this.ascendingLevel.prevState);
      } else if (DEBUG) {
        console.log(`ascending level validated at ${valid.data}`);
      }
    },
  });

  connectedCallback(): void {
    super.connectedCallback();
    this.ascendingLevel.subscribe(() => this.requestUpdate());
  }

  protected render(): TemplateResult {
    return html`
      ${DEBUG
        ? html`
            <div>
              <span class="title">ascendingLevel</span>:
              ${this.ascendingLevel.state}
            </div>
          `
        : ''}
    `;
  }
}
if (!customElements.get('pair-data')) {
  customElements.define('pair-data', PairData);
}

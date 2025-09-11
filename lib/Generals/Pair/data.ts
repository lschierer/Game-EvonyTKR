import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { html, LitElement, type TemplateResult } from 'lit';
import { property } from 'lit/decorators.js';

import { Store } from '@tanstack/store';

import {
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  AscendingOptions,
} from '../../Game/EvonyTKR/Shared/Constants';

import { SpecialtyStore } from '../specialtyStore';
import { PairStore } from './pairStore';

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

  @property({ attribute: false })
  public primaryCovenantLevel = new Store('none', {
    onUpdate: () => {
      const valid = CovenantCategoryValues.safeParse(
        this.primaryCovenantLevel.state,
      );
      if (!valid.success) {
        if (DEBUG) {
          console.error('invalid ascending level', valid.error);
        }
        this.primaryCovenantLevel.setState(this.primaryCovenantLevel.prevState);
      } else if (DEBUG) {
        console.log(`Primary CovenantCategory validated at ${valid.data}`);
      }
    },
  });

  @property({ attribute: false })
  public secondaryCovenantLevel = new Store('none', {
    onUpdate: () => {
      const valid = CovenantCategoryValues.safeParse(
        this.secondaryCovenantLevel.state,
      );
      if (!valid.success) {
        if (DEBUG) {
          console.error('invalid ascending level', valid.error);
        }
        this.secondaryCovenantLevel.setState(
          this.secondaryCovenantLevel.prevState,
        );
      } else if (DEBUG) {
        console.log(`Secondary CovenantCategory validated at ${valid.data}`);
      }
    },
  });

  // Primary and secondary specialty groups
  @property({ attribute: false })
  primarySpecialties = new SpecialtyStore();

  @property({ attribute: false })
  secondarySpecialties = new SpecialtyStore();

  @property({ attribute: false })
  pairStore: PairStore = new PairStore();

  connectedCallback(): void {
    super.connectedCallback();
    this.ascendingLevel.subscribe(() => this.requestUpdate());
    this.primaryCovenantLevel.subscribe(() => this.requestUpdate());
    this.secondaryCovenantLevel.subscribe(() => this.requestUpdate());
    this.primarySpecialties.subscribe(() => this.requestUpdate());
    this.secondarySpecialties.subscribe(() => this.requestUpdate());
    this.pairStore.subscribe(() => this.requestUpdate());
  }

  protected renderPairStoreDebug = () => {
    let template = html``;
    for (const key in Object.keys(this.pairStore.store.state.rows)) {
      const value = this.pairStore.store.state.rows[key];
      template = html`
        <dd>
          <dl>
            <dt>Primary:</dt>
            <dd>${value.primary}</dd>
            <dt>Secondary:</dt>
            <dd>${value.secondary}</dd>
            <dt>State:</dt>
            <dd>${value.state}</dd>
            <dt>Data:</dt>
            <dd>${value.data}</dd>
          </dl>
        </dd>
      `;
    }
    template = html`
      <dl>
        <dt>catalogRev</dt>
        <dd>${this.pairStore.store.state.catalogRev}</dd>
        <dt>selectionRev</dt>
        <dd>${this.pairStore.store.state.selectionRev}</dd>
        <dt>runId</dt>
        <dd>${this.pairStore.store.state.runId}</dd>
        <dt>catalog</dt>
        ${this.pairStore.store.state.catalog.map((ce) => {
          return html` <dd>${ce.primary} / ${ce.secondary}</dd> `;
        })}
        <dt>streaming</dt>
        <dd>${this.pairStore.store.state.streaming}</dd>
        <dt>rows</dt>
        <dd>${template}</dd>
      </dl>
    `;
    return template;
  };

  protected renderSpecialtyDebug = (primary: boolean) => {
    let template = html``;
    for (let i = 1; i <= 4; i++) {
      let s;
      if (primary) {
        s = this.primarySpecialties.get(i as 1 | 2 | 3 | 4);
      } else {
        s = this.secondarySpecialties.get(i as 1 | 2 | 3 | 4);
      }
      template = html`${template}
        <li><strong>${i}</strong>: ${s}</li> `;
    }
    template = html`
      <ul>
        ${template}
      </ul>
    `;
    return template;
  };

  protected render(): TemplateResult {
    return html`
      ${DEBUG
        ? html`
            <div class="pair-store">
              <h4>Pair Store</h4>
              ${this.renderPairStoreDebug()}
            </div>
            <div class="primary">
              <h4>Primary General Buff Filter Options</h4>
              <div>
                <span class="title">ascendingLevel</span>:
                ${this.ascendingLevel.state}
              </div>
              <div>
                <span class="title">covenantCategory</span>:
                ${this.primaryCovenantLevel.state}
              </div>
              <div>
                <span class="title">Specialty Levels</span>:
                ${this.renderSpecialtyDebug(true)}
              </div>
            </div>
            <div class="secondary">
              <h4>Secondary General Buff Filter Options</h4>
              <div>
                <span class="title">covenantCategory</span>:
                ${this.secondaryCovenantLevel.state}
              </div>
              <div>
                <span class="title">Specialty Levels</span>:
                ${this.renderSpecialtyDebug(false)}
              </div>
            </div>
          `
        : ''}
    `;
  }
}
if (!customElements.get('pair-data')) {
  customElements.define('pair-data', PairData);
}

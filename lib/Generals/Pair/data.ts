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
import { pairKey, PairStore } from './pairStore';
import { PrimaryFilterStore } from './primaryFilterStore';

export class PairData extends LitElement {
  static VERSION = '0.3';
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

  @property({ attribute: false })
  primaryFilter: PrimaryFilterStore = new PrimaryFilterStore();

  @property({ attribute: false })
  public queryParams = new Store<URLSearchParams>(new URLSearchParams());

  connectedCallback(): void {
    super.connectedCallback();
    if (DEBUG) {
      console.log(`version is ${PairData.VERSION}`);
    }
    this.ascendingLevel.subscribe(() => {
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.primaryCovenantLevel.subscribe(() => {
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.secondaryCovenantLevel.subscribe(() => {
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.primarySpecialties.subscribe(() => {
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.secondarySpecialties.subscribe(() => {
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.pairStore.subscribe(() => this.requestUpdate());
    this.primaryFilter.subscribe(() => this.requestUpdate());
  }

  public updateFilterParams() {
    const params = this.queryParams.state;
    params.set('ascendingLevel', this.ascendingLevel.state);
    params.set('primaryCovenantLevel', this.primaryCovenantLevel.state);
    params.set('secondaryCovenantLevel', this.primaryCovenantLevel.state);
    params.set('primarySpecialty1', this.primarySpecialties.store.state.s1);
    params.set('primarySpecialty2', this.primarySpecialties.store.state.s2);
    params.set('primarySpecialty3', this.primarySpecialties.store.state.s3);
    params.set('primarySpecialty4', this.primarySpecialties.store.state.s4);
    params.set('secondarySpecialty1', this.secondarySpecialties.store.state.s1);
    params.set('secondarySpecialty2', this.secondarySpecialties.store.state.s2);
    params.set('secondarySpecialty3', this.secondarySpecialties.store.state.s3);
    params.set('secondarySpecialty4', this.secondarySpecialties.store.state.s4);
    this.queryParams.setState(params);
  }

  protected renderPairStoreDebug = () => {
    const rowsTemplate = this.pairStore.store.state.catalog.map((entry) => {
      const key = pairKey(entry.primary, entry.secondary);
      const value = this.pairStore.store.state.rows[key];
      return html`
        <dd>
          <dl>
            <dt>Primary:</dt>
            <dd>${entry.primary}</dd>
            <dt>Secondary:</dt>
            <dd>${entry.secondary}</dd>
            <dt>State:</dt>
            <dd>${value ? value.state : 'missing'}</dd>
            <dt>Data:</dt>
            <dd>${value ? JSON.stringify(value.data) : 'no data'}</dd>
          </dl>
        </dd>
      `;
    });

    return html`
      <dl>
        <dt>catalogRev</dt>
        <dd>${this.pairStore.store.state.catalogRev}</dd>
        <dt>runId</dt>
        <dd>${this.pairStore.store.state.runId}</dd>
        <dt>catalog</dt>
        ${this.pairStore.store.state.catalog.map((ce) => {
          return html` <dd>${ce.primary} / ${ce.secondary}</dd> `;
        })}
        <dt>streaming</dt>
        <dd>${this.pairStore.store.state.streaming}</dd>
        <dt>rows</dt>
        ${rowsTemplate}
      </dl>
    `;
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

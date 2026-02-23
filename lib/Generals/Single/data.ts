import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { html, LitElement, type TemplateResult } from 'lit';
import { property } from 'lit/decorators.js';

import { Store } from '@tanstack/store';

import {
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  type AscendingOptions,
} from '../../Game/EvonyTKR/Shared/Constants';

import { SpecialtyStore } from '../specialtyStore';
import { GeneralStore } from './singleStore';
import { PrimaryFilterStore } from './generalFilterStore';

export class SingleData extends LitElement {
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
  public ascendingLevel = new Store('none');
  private _prevAscendingLevel = 'none';

  @property({ attribute: false })
  public covenantLevel = new Store('none');
  private _prevCovenantLevel = 'none';

  // Primary and secondary specialty groups
  @property({ attribute: false })
  specialties = new SpecialtyStore();

  @property({ attribute: false })
  generalStore: GeneralStore = new GeneralStore();

  @property({ attribute: false })
  buffFilter: PrimaryFilterStore = new PrimaryFilterStore();

  @property({ attribute: false })
  public generalLevel = new Store(40);
  private _prevGeneralLevel = 40;

  @property({ attribute: false })
  public victoryColumnLevel = new Store(0);
  private _prevVictoryColumnLevel = 0;

  @property({ attribute: false })
  public queryParams = new Store<URLSearchParams>(new URLSearchParams());

  connectedCallback(): void {
    super.connectedCallback();
    if (DEBUG) {
      console.log(`version is ${SingleData.VERSION}`);
    }
    this.ascendingLevel.subscribe(() => {
      const valid = AscendingAttributeLevelValues.safeParse(this.ascendingLevel.state);
      if (!valid.success) {
        if (DEBUG) console.error('invalid ascending level', valid.error);
        this.ascendingLevel.setState(() => this._prevAscendingLevel);
        return;
      }
      this._prevAscendingLevel = this.ascendingLevel.state;
      if (DEBUG) console.log(`ascending level validated at ${valid.data}`);
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.covenantLevel.subscribe(() => {
      const valid = CovenantCategoryValues.safeParse(this.covenantLevel.state);
      if (!valid.success) {
        if (DEBUG) console.error('invalid covenant level', valid.error);
        this.covenantLevel.setState(() => this._prevCovenantLevel);
        return;
      }
      this._prevCovenantLevel = this.covenantLevel.state;
      if (DEBUG) console.log(`CovenantCategory validated at ${valid.data}`);
      this.updateFilterParams();
      this.requestUpdate();
    });

    this.specialties.subscribe(() => {
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.generalLevel.subscribe(() => {
      const level = this.generalLevel.state;
      if (level < 25 || level > 50) {
        if (DEBUG) console.error('invalid general level', level);
        this.generalLevel.setState(() => this._prevGeneralLevel);
        return;
      }
      this._prevGeneralLevel = level;
      if (DEBUG) console.log(`general level validated at ${level}`);
      this.updateFilterParams();
      this.requestUpdate();
    });
    this.victoryColumnLevel.subscribe(() => {
      const level = this.victoryColumnLevel.state;
      if (level < 0 || level > 11) {
        if (DEBUG) console.error('invalid victory column level', level);
        this.victoryColumnLevel.setState(() => this._prevVictoryColumnLevel);
        return;
      }
      this._prevVictoryColumnLevel = level;
      if (DEBUG) console.log(`victory column level validated at ${level}`);
      this.updateFilterParams();
      this.requestUpdate();
    });

    this.generalStore.subscribe(() => { this.requestUpdate(); });
    this.buffFilter.subscribe(() => { this.requestUpdate(); });
  }

  public updateFilterParams() {
    const params = this.queryParams.state;
    params.set('ascendingLevel', this.ascendingLevel.state);
    params.set('covenantLevel', this.covenantLevel.state);
    params.set('generalLevel', String(this.generalLevel.state));
    params.set('victoryColumnLevel', String(this.victoryColumnLevel.state));
    params.set('specialty1', this.specialties.store.state.s1);
    params.set('specialty2', this.specialties.store.state.s2);
    params.set('specialty3', this.specialties.store.state.s3);
    params.set('specialty4', this.specialties.store.state.s4);
    this.queryParams.setState(() => params);
  }

  protected renderSingleStoreDebug = () => {
    const rowsTemplate = this.generalStore.store.state.catalog.map((entry) => {
      const key = entry.primary;
      const value = this.generalStore.store.state.rows[key];
      return html`
        <dd>
          <dl>
            <dt>Primary:</dt>
            <dd>${entry.primary}</dd>
            <dt>State:</dt>
            <dd>${value.state}</dd>
            <dt>Data:</dt>
            <dd>${JSON.stringify(value.data)}</dd>
          </dl>
        </dd>
      `;
    });

    return html`
      <dl>
        <dt>catalogRev</dt>
        <dd>${this.generalStore.store.state.catalogRev}</dd>
        <dt>runId</dt>
        <dd>${this.generalStore.store.state.runId}</dd>
        <dt>catalog</dt>
        ${this.generalStore.store.state.catalog.map((ce) => {
          return html` <dd>${ce.primary}</dd> `;
        })}
        <dt>streaming</dt>
        <dd>${this.generalStore.store.state.streaming}</dd>
        <dt>rows</dt>
        ${rowsTemplate}
      </dl>
    `;
  };

  protected renderSpecialtyDebug = () => {
    let template = html``;
    for (let i = 1; i <= 4; i++) {
      const s = this.specialties.get(i as 1 | 2 | 3 | 4);
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
            <div class="single-store">
              <h4>Single Store</h4>
              ${this.renderSingleStoreDebug()}
            </div>
            <div class="primary">
              <h4>General Buff Filter Options</h4>
              <div>
                <span class="title">ascendingLevel</span>:
                ${this.ascendingLevel.state}
              </div>
              <div>
                <span class="title">covenantCategory</span>:
                ${this.covenantLevel.state}
              </div>
              <div>
                <span class="title">Specialty Levels</span>:
                ${this.renderSpecialtyDebug()}
              </div>
            </div>
          `
        : ''}
    `;
  }
}
if (!customElements.get('single-data')) {
  customElements.define('single-data', SingleData);
}

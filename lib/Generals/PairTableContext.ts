import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { LitElement, html } from 'lit';
import { ContextRoot, provide } from '@lit/context';

import {
  GeneralPairsTableState,
  Filter,
  //type PairBuffFilter,
  //type PairState,
  filterContext,
  rowContext,
} from './PairTableState';

export class PairContextProvider extends LitElement {
  @provide({ context: filterContext })
  private filterState = Filter;

  @provide({ context: rowContext })
  public rowState = GeneralPairsTableState;

  constructor() {
    super();
  }

  override connectedCallback() {
    super.connectedCallback();
    const root = new ContextRoot();
    root.attach(document.body);
    if (DEBUG) {
      console.log(
        `PairContextProvider connected, providing: filterState: ${this.filterState}; rowState: ${this.rowState}`,
      );
    }
  }

  render() {
    return html`<slot></slot>`;
  }
}
if (!customElements.get('pair-context')) {
  customElements.define('pair-context', PairContextProvider);
}

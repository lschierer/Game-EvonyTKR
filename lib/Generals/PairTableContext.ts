import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import { LitElement, html } from 'lit';
import { ContextRoot, createContext, ContextProvider } from '@lit/context';

import {
  GeneralPairsTableState,
  Filter,
  type PairBuffFilter,
  type PairState,
} from './PairTableState';

export const filterContext = createContext<PairBuffFilter>(
  Symbol('filter-context'),
);

export const rowContext = createContext<PairState>(Symbol('row-context'));

export class PairContextProvider extends LitElement {
  public filterState = new ContextProvider(document.body, {
    context: filterContext,
    initialValue: Filter,
  });

  public rowState = new ContextProvider(document.body, {
    context: rowContext,
    initialValue: GeneralPairsTableState,
  });

  constructor() {
    super();
    const root = new ContextRoot();
    root.attach(document.body);
  }

  override connectedCallback() {
    super.connectedCallback();
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

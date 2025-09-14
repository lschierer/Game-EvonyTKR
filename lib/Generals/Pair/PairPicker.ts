import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import {
  html,
  css,
  LitElement,
  type PropertyValues,
  type CSSResultGroup,
  type TemplateResult,
} from 'lit';
import { customElement, property } from 'lit/decorators.js';

import { Store } from '@tanstack/store';

import SpectrumButton from '@spectrum-css/button/dist/index.css' with { type: 'css' };
import SpectrumDivider from '@spectrum-css/divider/dist/index.css' with { type: 'css' };
import SpectrumFieldGroup from '@spectrum-css/fieldgroup/dist/index.css' with { type: 'css' };
import SpectrumFieldLabel from '@spectrum-css/fieldlabel/dist/index.css' with { type: 'css' };
import SpectrumIcon from '@spectrum-css/icon/dist/index.css' with { type: 'css' };
import SpectrumForm from '@spectrum-css/form/dist/index.css' with { type: 'css' };
import SpectrumMenu from '@spectrum-css/menu/dist/index.css' with { type: 'css' };
import SpectrumPicker from '@spectrum-css/picker/dist/index.css' with { type: 'css' };
import SpectrumPickerButton from '@spectrum-css/pickerbutton/dist/index.css' with { type: 'css' };
import SpectrumPopOver from '@spectrum-css/popover/dist/index.css' with { type: 'css' };
import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import SpectrumTextField from '@spectrum-css/textfield/dist/index.css' with { type: 'css' };

import { PairData } from './data';

@customElement('pair-picker')
export class PairPicker extends LitElement {
  static styles: CSSResultGroup = [
    SpectrumTokensCSS,
    SpectrumButton,
    SpectrumDivider,
    SpectrumFieldGroup,
    SpectrumFieldLabel,
    SpectrumForm,
    SpectrumIcon,
    SpectrumMenu,
    SpectrumPicker,
    SpectrumPickerButton,
    SpectrumPopOver,
    SpectrumTextField,
    css`
      :host {
        --mod-textfield-width: 20em;
      }
      div.general-filter-popover-wrapper {
        --spectrum-popover-height: 142px;
        --spectrum-popover-width: 89px;
        position: relative;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        inline-size: var(--spectrum-popover-width);
      }

      div.spectrum-Form-item.two-item {
        display: grid;
        grid-template-columns: subgrid;
        grid-template-rows: subgrid;
        grid-column-end: span 2;
      }

      div.spectrum-Form-item.two-item > div.spectrum-Form-itemField.two-item {
        display: grid;
        grid-template-columns: subgrid;
        grid-template-rows: subgrid;
        grid-column-end: span 2;
      }

      div#primary-picker {
        inset-block-start: 100%;
        inset-inline-start: 0;
        z-index: 1000;
      }

      iconify-icon {
        width: 1em;
        height: 1em;
      }

      iconify-icon.hidden {
        visibility: hidden;
      }
    `,
  ];

  protected data?: PairData;
  private currentES?: EventSource;
  private submitDebounce?: number;

  @property({ type: String })
  public generalFilterLabel: String = 'General';

  @property({ attribute: false })
  protected menuOpen = new Store<boolean>(false);

  @property({ attribute: false })
  protected filterText = new Store<string>('');

  protected override firstUpdated(_changedProperties: PropertyValues): void {
    super.firstUpdated(_changedProperties);
    const form = this.renderRoot.querySelector<HTMLFormElement>(
      '#general-picker-form',
    )!;

    form.addEventListener('formdata', (e: Event) => {
      const fd = (e as unknown as { formData: FormData }).formData;
      // Clear previous
      fd.delete('primaries[]');

      // Inject current selection from the store
      const selected =
        this.data?.pairStore.store.state.selectedPrimaries ?? new Set<string>();
      for (const name of selected) fd.append('primaries[]', name);
    });

    // Intercept native submit
    form.addEventListener('submit', (e) => {
      e.preventDefault();
      const fd = new FormData(form);

      // If you didn’t add hidden inputs for selection, fill via `formdata` event (see below),
      // or manually inject here:
      // for (const name of this.data!.pairStore.store.state.selectedPrimaries) {
      //   fd.append('primaries[]', name);
      // }

      // Build URL
      const params = new URLSearchParams();
      for (const [k, v] of fd.entries()) params.append(k, String(v));

      // Keep URL shareable
      const base = location.pathname;
      const qs = params.toString();
      history.replaceState(null, '', qs ? `${base}?${qs}` : base);
      if (this.data) {
        this.data.updateFilterParams();
        this.restartStream();
      }
      // Restart stream with new params
    });

    // Auto-submit on any change (debounced)
    form.addEventListener('change', () => this.requestSubmitDebounced(form));
    form.addEventListener('input', () => this.requestSubmitDebounced(form));

    // Optionally: pulse once on mount to hydrate based on existing URL
    form.requestSubmit();
  }

  override connectedCallback(): void {
    super.connectedCallback();
    this.menuOpen.subscribe(() => this.requestUpdate());
    this.filterText.subscribe(() => this.requestUpdate());
    const qr = this.querySelector('pair-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as PairData;
    }
    if (this.data) {
      this.data.pairStore.subscribe(() => this.requestUpdate());
      this.data.queryParams.subscribe(() => {
        this.requestUpdate();
        this.restartStream();
      });
      let path = window.location.pathname;
      path = path.replace('-comparison', '/data.json');
      const catalogUrl = new URL(path, window.location.toString());
      if (DEBUG) {
        console.log(`using catalog Url ${catalogUrl.toString()}`);
      }
      this.data.pairStore.getCatalog(catalogUrl.toString());

      this.data.pairStore.sessionId.subscribe(() => this.restartStream());
    }
  }

  readonly onFilterInput = (e: Event) => {
    const target = e.target as HTMLInputElement;
    this.filterText.setState(target.value.toLowerCase());
  };

  private requestSubmitDebounced(form: HTMLFormElement) {
    if (this.submitDebounce) clearTimeout(this.submitDebounce);
    this.submitDebounce = window.setTimeout(() => form.requestSubmit(), 250);
  }

  private restartStream() {
    if (!this.data) return;
    let sp = window.location.pathname;
    sp = sp.replace('-comparison', '-details-stream');
    const streamUrl = new URL(sp, window.location.toString());
    if (this.currentES) {
      this.currentES.close();
    }
    this.currentES = this.data.pairStore.openPairsStream(
      streamUrl.toString(),
      this.data.queryParams.state,
      this.data.pairStore.sessionId.state,
    );
    this.data.pairStore.recomputeIgnoreStates();
  }

  private toggleMenu = (e: Event) => {
    e.preventDefault(); // belt & suspenders
    const willOpen = !this.menuOpen.state;
    this.menuOpen.setState(willOpen);

    // Clear filter when closing menu
    if (!willOpen) {
      this.filterText.setState('');
      // Also clear the input field
      const input =
        this.renderRoot.querySelector<HTMLInputElement>('#general-filter');
      if (input) input.value = '';
    }
  };

  private selectFiltered = (primary: string, newState: boolean) => {
    if (!this.data) return;
    const current = new Set([
      ...this.data.pairStore.store.state.selectedPrimaries,
    ]);
    const isSelected = current.has(primary);
    const willSelect = newState ?? !isSelected;
    if (willSelect && !isSelected) current.add(primary);
    if (!willSelect && isSelected) current.delete(primary);
    this.data.pairStore.setSelectedPrimaries([...current]);
    let path = window.location.pathname;
    path = path.replace('-comparison', '/data.json');
    const catalogUrl = new URL(path, window.location.toString());
    this.data.pairStore.getCatalog(catalogUrl.toString());
  };

  protected renderGeneralFilter() {
    let rowTemplate = html``;
    const primaries = new Set<string>();
    if (this.data) {
      for (const entry of this.data.pairStore.store.state.catalog) {
        primaries.add(entry.primary);
      }

      // Filter primaries based on the search text
      const filterText = this.filterText.state;
      const filteredPrimaries = [...primaries].filter(
        (primary) =>
          filterText === '' || primary.toLowerCase().includes(filterText),
      );

      for (const primary of filteredPrimaries) {
        const itemSelected =
          this.data.pairStore.store.state.selectedPrimaries.has(primary);
        const noSpaces = primary.replaceAll(' ', '');
        rowTemplate = html`${rowTemplate}
          <li
            class=" spectrum-Menu-item "
            style=""
            id="${noSpaces}"
            role="menuitemcheckbox"
            aria-checked="${itemSelected ? 'true' : 'false'}"
            aria-disabled="false"
            @click=${() => this.selectFiltered(primary, !itemSelected)}
            tabindex="0"
          >
            <iconify-icon
              icon="tabler:circle-check"
              width="none"
              class="spectrum-Icon spectrum-Icon--medium spectrum-Menu-itemIcon ${itemSelected
                ? ''
                : 'hidden'}"
            ></iconify-icon>

            <span class=" spectrum-Menu-itemLabel "> ${primary} </span>
          </li> `;
      }
    }

    return html`
      <div class="spectrum-Form-item two-item">
        <label
          for="general-filter"
          class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
        >
          ${this.generalFilterLabel}
        </label>
        <div class="spectrum-Form-itemField two-item">
          <input
            id="general-filter"
            type="search"
            class="spectrum-Textfield spectrum-Textfield--sizeM"
            placeholder="Filter generals…"
            @input=${this.onFilterInput}
            @keydown=${(e: KeyboardEvent) => {
              if (e.key === 'Enter') e.preventDefault();
            }}
            aria-label="Filter generals"
          />
          <div class="general-filter-popover-wrapper">
            <button
              aria-pressed="${this.menuOpen.state ? 'true' : 'false'}"
              class="spectrum-PickerButton spectrum-PickerButton--uiicononly spectrum-PickerButton--right spectrum-PickerButton--sizeM ${this
                .menuOpen.state
                ? 'is-open'
                : ''}"
              aria-haspopup="true"
              aria-expanded="${this.menuOpen.state ? 'true' : 'false'}"
              role="button"
              type="button"
              @click=${this.toggleMenu}
            >
              <div class="spectrum-PickerButton-fill">
                <iconify-icon
                  icon="${this.menuOpen.state
                    ? 'gg:chevron-down-r'
                    : 'gg:chevron-up-r'}"
                  width="none"
                  class="spectrum-PickerButton-fill"
                ></iconify-icon>
              </div>
            </button>

            <div
              role="presentation"
              id="primary-picker"
              class="spectrum-Popover ${this.menuOpen.state
                ? 'is-open'
                : 'is-closed'} spectrum-Popover--sizeM spectrum-Popover--bottom-right"
            >
              <ul
                class="spectrum-Menu spectrum-Menu--sizeM is-selectableMultiple"
                id="primary-picker-menu"
                role="group"
                aria-disabled="false"
              >
                ${this.data ? rowTemplate : ''}
              </ul>
            </div>
          </div>
        </div>
      </div>
    `;
  }

  protected override render(): TemplateResult {
    return html` <div class="settings-form">
        <form
          id="general-picker-form"
          class="spectrum-Form spectrum-Form--labelsAbove spectrum-Form--sizeM"
          novalidate
          role="group"
          @submit=${(e: Event) => e.preventDefault()}
        >
          ${this.renderGeneralFilter()}
        </form>
      </div>
      <slot></slot>`;
  }
}

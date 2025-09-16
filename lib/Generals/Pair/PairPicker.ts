import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import {
  html,
  css,
  LitElement,
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

      div.settings-form {
        padding-top: 0.5em;
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

  @property({ type: String })
  public generalFilterLabel: String = 'General';

  @property({ attribute: false })
  protected menuOpen = new Store<boolean>(false);

  @property({ attribute: false })
  protected filterText = new Store<string>('');

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
      this.data.primaryFilter.subscribe(() => this.requestUpdate());
    }
    if (this.data) {
      this.data.pairStore.subscribe(() => {
        if (!this.data) return;
        // Update available primaries when catalog changes
        const primaries = new Set<string>();
        for (const entry of this.data.pairStore.getCatalog()) {
          primaries.add(entry.primary);
        }
        this.data.primaryFilter.setAvailable([...primaries]);
        this.requestUpdate();
      });

      void this.data.pairStore.updateCatalog();
    }
  }

  readonly onFilterInput = (e: Event) => {
    e.stopPropagation();
    const target = e.target as HTMLInputElement;
    this.filterText.setState(target.value.toLowerCase());
  };

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

    const wasSelected = this.data.primaryFilter.isSelected(primary);
    const willSelect = newState ?? !wasSelected;

    if (willSelect !== wasSelected) {
      if (DEBUG) {
        console.log(
          `selectFiltered changing primary ${primary} from ${wasSelected} to ${willSelect}`,
        );
      }
      this.data.primaryFilter.toggle(primary);
      this.data.pairStore.toggleAllIgnoredForPrimary(primary);
    }
  };

  protected renderGeneralFilter() {
    let rowTemplate = html``;

    if (this.data) {
      // Filter primaries based on the search text
      const filterText = this.filterText.state;
      const filteredPrimaries = this.data.primaryFilter
        .getAvailable()
        .filter(
          (primary) =>
            filterText === '' || primary.toLowerCase().includes(filterText),
        );

      for (const primary of filteredPrimaries) {
        const itemSelected = this.data.primaryFilter.isSelected(primary);

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

import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import 'iconify-icon';
import { property } from 'lit/decorators.js';
import {
  html,
  type CSSResultGroup,
  LitElement,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';
import { Signal, SignalWatcher, signal, computed } from '@lit-labs/signals';
import { ContextConsumer } from '@lit/context';

import * as z from 'zod';
import {
  AscendingAttributeLevelNames,
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  SpecialtyLevelValues,
} from '../Game/EvonyTKR/Shared/Constants';

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
import LevelSettingsFormCSS from '../../share/public/css/level_settings_form.css' with { type: 'css' };

type SpecialtyLookupType = Record<
  0 | 1 | 2 | 3,
  Signal.State<SpecialtyLevelValues>
>;

interface GeneralOption {
  name: string;
  selected: boolean;
}

import { filterContext, rowContext } from './PairTableState';

export class LevelSettings extends SignalWatcher(LitElement) {
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
    LevelSettingsFormCSS,
  ];

  private filterState = new ContextConsumer(this, {
    context: filterContext,
    subscribe: true,
  });

  private rowState = new ContextConsumer(this, {
    context: rowContext,
    subscribe: true,
  });

  @property({ type: Boolean })
  public is_primary: boolean = false;

  @property({ type: String })
  public form_title: string = this.is_primary
    ? 'Primary General'
    : 'Secondary General';

  @property({ type: String })
  public generalFilterLabel: string = 'Selected Primary Generals';

  private menuOpen: Signal.State<boolean> = signal(false);

  private _generalOptions = new Array<GeneralOption>();

  public revision: number = 0;

  private filterText = signal('');

  override connectedCallback() {
    super.connectedCallback();
    if (DEBUG) {
      console.log(
        'LevelSettings connectedCallback, filterState:',
        this.filterState,
        'rowState:',
        this.rowState,
      );
    }
  }

  private filteredOptions = computed(() => {
    this.rowState.value?.namesRev.get(); // depend on list changes
    const q = this.filterText.get().trim().toLocaleLowerCase();
    if (!q) return [...this._generalOptions];
    // simple contains; upgrade to fuzzy later if you want
    return this._generalOptions.filter((o) =>
      o.name.toLocaleLowerCase().includes(q),
    );
  });

  protected override updated(_changedProperties: PropertyValues): void {
    super.updated(_changedProperties);
    this.dispatchEvent(
      new CustomEvent('form_updated', {
        detail: {
          revision: this.revision,
          is_primary: this.is_primary,
        },
        bubbles: true,
        composed: true,
      }),
    );
    this.revision++;
  }

  set generals(names: string[]) {
    this._generalOptions = names.map((n) => ({ name: n, selected: true }));
    this.rowState.value!.selected_list.set(names.slice());
    this.rowState.value!.namesRev.set(this.rowState.value!.namesRev.get() + 1);
    this.requestUpdate(); // to re-render the menu immediately
  }

  private toggleByName(name: string) {
    const i = this._generalOptions.findIndex((o) => o.name === name);
    if (i >= 0) {
      const cv = this._generalOptions[i].selected;
      this._generalOptions[i].selected = !cv;
    }
    this.rowState.value!.selected_list.set(
      this._generalOptions.filter((go) => go.selected).map((go) => go.name),
    );
    this.requestUpdate();
  }

  private selectFiltered(value: boolean) {
    if (DEBUG) console.log(`setting filtered to ${value}`);
    const visible = new Set(this.filteredOptions.get().map((o) => o.name));
    let changed = false;
    for (const o of this._generalOptions) {
      if (!visible.has(o.name)) continue;
      if (o.selected !== value) {
        o.selected = value;
        changed = true;
      }
    }
    if (changed) {
      this.rowState.value!.selected_list.set(
        this._generalOptions.filter((go) => go.selected).map((go) => go.name),
      );
      this.requestUpdate();
    }
  }

  private onFilterInput = (e: Event) => {
    const v = (e.currentTarget as HTMLInputElement).value;
    this.filterText.set(v);
  };

  protected renderGeneralFilter() {
    const filtered = this.filteredOptions.get();
    return html`
      <div class="spectrum-Form-item">
        <label
          for="general-filter"
          class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
        >
          ${this.generalFilterLabel}
        </label>
        <div class="spectrum-Form-itemField row">
          <input
            id="general-filter"
            type="search"
            class="spectrum-Textfield spectrum-Textfield--sizeM"
            placeholder="Filter generals…"
            @input=${this.onFilterInput}
            aria-label="Filter generals"
          ></input>

          <div class="dropdown">
            <button
              type="button"
              class="spectrum-PickerButton spectrum-PickerButton--uiicononly spectrum-PickerButton--right spectrum-PickerButton--sizeM ${
                this.menuOpen.get() ? 'is-open' : ''
              }"
              aria-haspopup="true"
              aria-expanded=${this.menuOpen.get() ? 'true' : 'false'}
              @click=${() => {
                let mo = this.menuOpen.get();
                console.log(
                  `clicked picker button; it was ${mo ? 'true' : 'false'}`,
                );
                this.menuOpen.set(!this.menuOpen.get());
                mo = this.menuOpen.get();
                console.log(`it is now ${mo ? 'true' : 'false'}`);
              }}
            >
              <div class="spectrum-PickerButton-fill">
                <iconify-icon icon="${this.menuOpen.get() ? 'gg:chevron-up-r' : 'gg:chevron-down-r'}" width="none" class="spectrum-PickerButton-fill"></iconify-icon>
              </div>
            </button>

            ${
              this.menuOpen.get()
                ? html`
                    <div
                      id="general-filter-popover"
                      role="presentation"
                      class="spectrum-Popover is-open spectrum-Popover--sizeM spectrum-Popover--bottom-left"
                    >
                      <ul
                        class="spectrum-Menu spectrum-Menu--sizeM is-selectableMultiple"
                        role="group"
                      >
                        <!-- Select all / none -->
                        <li
                          class="spectrum-Menu-item "
                          role="menuitem"
                          @click=${() => this.selectFiltered(true)}
                          tabindex="0"
                        >
                          <span class="spectrum-Menu-itemLabel"
                            >Select all (filtered)</span
                          >
                        </li>
                        <li
                          class="spectrum-Menu-item"
                          role="menuitem"
                          @click=${() => this.selectFiltered(false)}
                          tabindex="0"
                        >
                          <span class="spectrum-Menu-itemLabel"
                            >Select none (filtered)</span
                          >
                        </li>

                        <!-- Divider -->
                        <li
                          class="spectrum-Divider spectrum-Divider--sizeM"
                          role="separator"
                        ></li>

                        <!-- Checkable options -->
                        ${filtered
                          .sort((a, b) => {
                            return a.name.localeCompare(b.name);
                          })
                          .map(
                            (opt, index) => html`
                              <li
                                class="spectrum-Menu-item ${opt.selected
                                  ? 'is-selected'
                                  : ''}"
                                role="option"
                                aria-selected="${opt.selected
                                  ? 'true'
                                  : 'false'}"
                                tabindex="0"
                                @click=${() => this.toggleByName(opt.name)}
                              >
                                <iconify-icon
                                  icon="tabler:circle-check"
                                  width="none"
                                  class="spectrum-Icon spectrum-Icon--medium spectrum-Menu-itemIcon ${opt.selected
                                    ? ''
                                    : 'hidden'}"
                                ></iconify-icon>
                                <span class="spectrum-Menu-itemLabel">
                                  ${opt.name}
                                </span>
                              </li>
                            `,
                          )}
                        ${filtered.length === 0
                          ? html` <li
                              class="spectrum-Menu-item is-disabled"
                              tabindex="-1"
                            >
                              <span class="spectrum-Menu-itemLabel"
                                >No matches</span
                              >
                            </li>`
                          : null}
                      </ul>
                    </div>
                  `
                : null
            }
          </div>
        </div>
      </div>
    `;
  }

  private handleLevelChange<T>(
    signal: Signal.State<T>,
    validator: z.ZodType<T>,
  ) {
    return (e: Event) => {
      const target = e.target as HTMLInputElement;
      const valid = validator.safeParse(target.value);
      if (valid.error) {
        console.error(`Invalid value ${target.value} provided`);
      } else {
        signal.set(valid.data); // valid.data is now type T
      }
    };
  }

  protected renderCovenantcombo() {
    if (this.filterState.value) {
      return html`
        <div class="spectrum-Form-item">
          <label
            for="covenantLevel"
            class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
          >
            Covenant Level:
          </label>
          <div class="spectrum-Form-itemField">
            <select
              id="covenantLevel"
              name="covenantLevel"
              class="spectrum-Picker spectrum-Picker--sizeM"
              @change="${this.handleLevelChange(
                this.is_primary
                  ? this.filterState.value.primary.covenantLevel
                  : this.filterState.value.secondary.covenantLevel,
                CovenantCategoryValues,
              )}"
            >
              ${repeat(
                CovenantCategoryValues.values.values(),
                (value) => value,
                (value, index) => {
                  const selected = this.is_primary
                    ? !this.filterState
                        .value!.primary.covenantLevel.get()
                        .localeCompare(value)
                    : !this.filterState
                        .value!.secondary.covenantLevel.get()
                        .localeCompare(value);
                  return html`
                    <option value="${value}" ?selected=${selected}>
                      ${value.charAt(0).toLocaleUpperCase() + value.slice(1)}
                    </option>
                  `;
                },
              )}
            </select>
          </div>
        </div>
      `;
    } else if (DEBUG) {
      return html`<span>Pending Filter State...</span>`;
    } else {
      return html`<span>Loading...</span>`;
    }
  }

  protected renderAscendingcombo() {
    let template = html``;
    if (!this.is_primary || !this.filterState.value) {
      return template;
    }
    const li = AscendingAttributeLevelNames.values.values();
    const vi = AscendingAttributeLevelValues.values.values();
    let label;
    let value;
    do {
      label = li.next().value;
      value = vi.next().value;
      if (label && value) {
        const selected = !this.filterState.value.primary.ascendingLevel
          .get()
          .localeCompare(value);
        template = html`${template}
          <option value="${value}" ?selected=${selected}>${label}</option> `;
      }
    } while (value && label);

    template = html`
      <div class="spectrum-Form-item">
        <label
          for="ascendingLevel"
          class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
        >
          Ascending Level:
        </label>
        <div class="spectrum-Form-itemField">
          <select
            id="ascendingLevel"
            name="ascendingLevel"
            class="spectrum-Picker spectrum-Picker--sizeM"
            @change="${this.handleLevelChange(
              this.filterState.value.primary.ascendingLevel,
              AscendingAttributeLevelValues,
            )}"
          >
            ${template}
          </select>
        </div>
      </div>
    `;
    return template;
  }

  private SpecialtyLookup = (index: 0 | 1 | 2 | 3) => {
    const lookup: SpecialtyLookupType = {
      [0]: this.is_primary
        ? this.filterState.value!.primary.specialtyLevel1
        : this.filterState.value!.secondary.specialtyLevel1,
      [1]: this.is_primary
        ? this.filterState.value!.primary.specialtyLevel2
        : this.filterState.value!.secondary.specialtyLevel2,
      [2]: this.is_primary
        ? this.filterState.value!.primary.specialtyLevel3
        : this.filterState.value!.secondary.specialtyLevel3,
      [3]: this.is_primary
        ? this.filterState.value!.primary.specialtyLevel4
        : this.filterState.value!.secondary.specialtyLevel4,
    };
    return lookup[index];
  };

  private get specialtyConstraints() {
    const first3GoldArray = new Array<boolean>();
    first3GoldArray.push(
      (this.is_primary
        ? this.filterState.value!.primary.specialtyLevel1.get()
        : this.filterState.value!.secondary.specialtyLevel1.get()) === 'gold',
      (this.is_primary
        ? this.filterState.value!.primary.specialtyLevel2.get()
        : this.filterState.value!.secondary.specialtyLevel2.get()) === 'gold',
      (this.is_primary
        ? this.filterState.value!.primary.specialtyLevel3.get()
        : this.filterState.value!.secondary.specialtyLevel3.get()) === 'gold',
    );
    const first3Gold = first3GoldArray.reduce((accumulator, current) => {
      accumulator = accumulator && current;
      return accumulator;
    }, true);
    return {
      first3Gold,
      fourthOptions: first3Gold
        ? ['green', 'blue', 'purple', 'orange']
        : ['none'],
      fourthDisabled: (option: string) =>
        first3Gold ? option === 'none' : option !== 'none',
    };
  }

  private onSpecialtyChange(
    signal: Signal.State<SpecialtyLevelValues>,
    index: number,
    target: EventTarget | HTMLSelectElement | null,
  ) {
    const newValue = target ? (target as HTMLSelectElement).value : 'none';
    if (DEBUG) {
      console.log(`onSpecialtyChange for ${index} with ${newValue}`);
    }
    const valid = SpecialtyLevelValues.safeParse(newValue);
    if (valid.success) {
      signal.set(valid.data);
      if (DEBUG) {
        console.log(`specialtyLevels ${index} now ${signal.get()}`);
      }
    }

    // Auto-adjust 4th specialty based on constraint
    if (index < 3) {
      const first3GoldArray = new Array<boolean>();
      first3GoldArray.push(
        (this.is_primary
          ? this.filterState.value!.primary.specialtyLevel1.get()
          : this.filterState.value!.secondary.specialtyLevel1.get()) === 'gold',
        (this.is_primary
          ? this.filterState.value!.primary.specialtyLevel2.get()
          : this.filterState.value!.secondary.specialtyLevel2.get()) === 'gold',
        (this.is_primary
          ? this.filterState.value!.primary.specialtyLevel3.get()
          : this.filterState.value!.secondary.specialtyLevel3.get()) === 'gold',
      );
      const first3Gold = first3GoldArray.reduce((accumulator, current) => {
        accumulator = accumulator && current;
        return accumulator;
      }, true);

      if (DEBUG) {
        console.log(`onSpecialtyChange ${index} first3Gold ${first3Gold}`);
      }
      if (
        first3Gold &&
        (this.is_primary
          ? this.filterState.value!.primary.specialtyLevel4.get()
          : this.filterState.value!.secondary.specialtyLevel4) === 'none'
      ) {
        this.is_primary
          ? this.filterState.value!.primary.specialtyLevel4.set('green')
          : this.filterState.value!.secondary.specialtyLevel4.set('green');
      } else if (
        !first3Gold &&
        (this.is_primary
          ? this.filterState.value!.primary.specialtyLevel4.get()
          : this.filterState.value!.secondary.specialtyLevel4) === 'none'
      ) {
        this.is_primary
          ? this.filterState.value!.primary.specialtyLevel4.set('none')
          : this.filterState.value!.secondary.specialtyLevel4.set('none');
      }
      if (DEBUG) {
        const sp4 = this.is_primary
          ? this.filterState.value!.primary.specialtyLevel4.get()
          : this.filterState.value!.secondary.specialtyLevel4.get();
        console.log(
          `onSpecialtyChange ${index} first3Gold ${first3Gold} ${sp4}`,
        );
      }
    }
    this.requestUpdate('specialtyLevels');
  }

  protected renderSpecialitycombos() {
    let template = html``;
    if (this.filterState.value) {
      for (let i = 0; i < 4; i++) {
        const specialty = this.SpecialtyLookup(i as 0 | 1 | 2 | 3);
        if (DEBUG) {
          console.log(`render for specialty ${i} has value ${specialty.get()}`);
        }
        template = html`${template}
          <div class="spectrum-Form-item">
            <label
              for="specialty${i}"
              class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
            >
              Specialty ${i + 1}:
            </label>
            <div class="spectrum-Form-itemField">
              <select
                id="specialty${i}"
                name="specialty${i}"
                class="spectrum-Picker spectrum-Picker--sizeM"
                @change="${(e: Event) =>
                  this.onSpecialtyChange(specialty, i, e.target)}"
              >
                ${repeat(
                  SpecialtyLevelValues.values.values(),
                  (value) => value,
                  (value, index) => {
                    const selected = !specialty.get().localeCompare(value);
                    if (DEBUG) {
                      console.log(
                        `render for specialty ${i} has value ${specialty.get()} and value ${value} ${selected ? 'is' : 'is not'} selected`,
                      );
                    }
                    return html`
                      <option
                        value="${value}"
                        ?selected=${selected}
                        ?disabled=${i === 3
                          ? this.specialtyConstraints.fourthDisabled(value)
                          : false}
                      >
                        ${value.charAt(0).toLocaleUpperCase() + value.slice(1)}
                      </option>
                    `;
                  },
                )}
              </select>
            </div>
          </div> `;
      }
    }
    return template;
  }

  public render() {
    if (DEBUG) {
      console.log(
        'LevelSettings render called, filterState:',
        this.filterState,
      );
    }
    return html`
      <div class="settings-form">
        <h3 class="spectrum-Heading spectrum-Heading--sizeS">
          ${this.form_title}
        </h3>
        <form
          id="settings-form"
          class="spectrum-Form spectrum-Form--labelsAbove spectrum-Form--sizeM"
        >
          ${this.renderCovenantcombo()} ${this.renderAscendingcombo()}
          ${this.renderSpecialitycombos()}
          ${this._generalOptions.length ? this.renderGeneralFilter() : ''}
        </form>
      </div>
    `;
  }
}
if (!customElements.get('level-settings')) {
  customElements.define('level-settings', LevelSettings);
}

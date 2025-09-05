import debugFunction from '../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import 'iconify-icon';
import { customElement, property } from 'lit/decorators.js';
import {
  html,
  type CSSResultGroup,
  LitElement,
  type PropertyValues,
} from 'lit';
import { repeat } from 'lit/directives/repeat.js';
import { Signal, SignalWatcher, signal } from '@lit-labs/signals';

import * as z from 'zod';
import {
  AscendingAttributeLevelNames,
  AscendingAttributeLevelValues,
  CovenantCategoryValues,
  SpecialtyLevelValues,
} from '../Game/EvonyTKR/Shared/Constants';

import SpectrumTokensCSS from '@spectrum-css/tokens/dist/index.css' with { type: 'css' };
import LevelSettingsFormCSS from '../../share/public/css/level_settings_form.css' with { type: 'css' };

@customElement('level-settings')
export class LevelSettings extends SignalWatcher(LitElement) {
  static styles: CSSResultGroup = [SpectrumTokensCSS, LevelSettingsFormCSS];

  @property({ type: Boolean })
  public is_primary: boolean = true;

  @property({ type: String })
  public FormTitle: string = this.is_primary
    ? 'Primary General'
    : 'Secondary General';

  @property({ type: String })
  public covenantLevel: Signal.State<CovenantCategoryValues> =
    signal('civilization');

  @property({ type: String })
  public ascendingLevel: Signal.State<AscendingAttributeLevelValues> =
    signal('red5');

  @property({ type: Array })
  public specialtyLevels: Signal.State<SpecialtyLevelValues>[] = new Array<
    Signal.State<SpecialtyLevelValues>
  >();

  constructor() {
    super();
    for (let i = 0; i < 4; i++) {
      this.specialtyLevels[i] = new Signal.State<SpecialtyLevelValues>('gold');
    }
  }
  protected willUpdate(_changedProperties: PropertyValues): void {
    if (DEBUG) {
      if (_changedProperties.has('covenantLevel')) {
        console.log(`covenantLevel: ${this.covenantLevel}`);
      }
      if (_changedProperties.has('ascendingLevel')) {
        console.log(`ascendingLevel: ${this.ascendingLevel}`);
      }
      if (_changedProperties.has('specialtyLevels')) {
        for (let i = 0; i < 4; i++) {
          console.log(`specialtyLevel${i}: ${this.specialtyLevels[i].get()}`);
        }
      }
    }
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
              this.covenantLevel,
              CovenantCategoryValues,
            )}"
          >
            ${repeat(
              CovenantCategoryValues.values.values(),
              (value) => value,
              (value, index) => {
                const selected = !this.covenantLevel.get().localeCompare(value);

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
  }

  protected renderAscendingcombo() {
    let template = html``;
    if (!this.is_primary) {
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
        const selected = !this.ascendingLevel.get().localeCompare(value);
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
              this.ascendingLevel,
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

  private get specialtyConstraints() {
    const first3Gold = this.specialtyLevels
      .slice(0, 3)
      .every((level) => level.get() === 'gold');
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
    index: number,
    target: EventTarget | HTMLSelectElement | null,
  ) {
    const newValue = target ? (target as HTMLSelectElement).value : 'none';
    if (DEBUG) {
      console.log(`onSpecialtyChange for ${index} with ${newValue}`);
    }
    const valid = SpecialtyLevelValues.safeParse(newValue);
    if (valid.success) {
      this.specialtyLevels[index].set(valid.data);
      if (DEBUG) {
        console.log(
          `specialtyLevels ${index} now ${this.specialtyLevels[index].get()}`,
        );
      }
    }

    // Auto-adjust 4th specialty based on constraint
    if (index < 3) {
      const first3Gold = this.specialtyLevels
        .slice(0, 3)
        .every((level) => level.get() === 'gold');
      if (DEBUG) {
        console.log(`onSpecialtyChange ${index} first3Gold ${first3Gold}`);
      }
      if (first3Gold && this.specialtyLevels[3].get() === 'none') {
        this.specialtyLevels[3].set('green');
      } else if (!first3Gold && this.specialtyLevels[3].get() !== 'none') {
        this.specialtyLevels[3].set('none');
      }
      if (DEBUG) {
        console.log(
          `onSpecialtyChange ${index} first3Gold ${first3Gold} ${this.specialtyLevels[3].get()}`,
        );
      }
    }
    this.requestUpdate('specialtyLevels');
  }

  protected renderSpecialitycombos() {
    let template = html``;
    for (let i = 0; i < 4; i++) {
      let specialty = this.specialtyLevels[i];
      if (!specialty) {
        if (DEBUG) {
          console.warn(
            `specialty ${i} was unset in render, defaulting to gold.`,
          );
        }
        specialty = new Signal.State<SpecialtyLevelValues>('gold');
        this.specialtyLevels[i] = specialty;
      } else if (DEBUG) {
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
              @change="${(e: Event) => this.onSpecialtyChange(i, e.target)}"
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
    return template;
  }

  protected render() {
    return html`
      <div class="settings-form">
        <h3 class="spectrum-Heading spectrum-Heading--sizeS">
          ${this.FormTitle}
        </h3>
        <form
          id="settings-form"
          class="spectrum-Form spectrum-Form--labelsAbove spectrum-Form--sizeM"
        >
          ${this.renderCovenantcombo()} ${this.renderAscendingcombo()}
          ${this.renderSpecialitycombos()}
        </form>
      </div>
    `;
  }
}

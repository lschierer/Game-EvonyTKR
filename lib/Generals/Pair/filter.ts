import debugFunction from '../../localDebug';
const DEBUG = debugFunction(__FILE_PATH__);
console.log(`DEBUG is set to ${DEBUG} for ${__FILE_PATH__}`);

import {
  html,
  LitElement,
  type PropertyValues,
  type CSSResultGroup,
  type TemplateResult,
} from 'lit';
import { customElement, property } from 'lit/decorators.js';

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
import LevelSettingsFormCSS from '../../../share/public/css/level_settings_form.css' with { type: 'css' };

import { type PairData } from './data';
import {
  //AscendingAttributeLevelValues,
  type CovenantCategoryOptions,
  //CovenantCategoryValues,
  SpecialtyLevelOptions,
  SpecialtyLevelValues,
} from '../../Game/EvonyTKR/Shared/Constants';

import { UrlBinder, type ParamRow } from '../UrlBinder';

@customElement('pair-filter')
export class PairFilter extends LitElement {
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

  protected data?: PairData;

  @property({ type: Boolean })
  public is_primary: boolean = false;

  @property({ type: String })
  public form_title: string = '';

  private binder?: UrlBinder;

  protected firstUpdated(_changedProperties: PropertyValues): void {
    super.firstUpdated(_changedProperties);
    if (DEBUG) {
      console.log(
        `${this.is_primary ? 'primary' : 'secondary'} filter first updated`,
      );
    }
  }

  override connectedCallback(): void {
    super.connectedCallback();
    const qr = this.querySelector('pair-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as PairData;
      const rows = this.UrlParamHandler();
      if (rows) {
        this.binder = new UrlBinder(rows);
        const watch = this.is_primary
          ? [
              this.data.ascendingLevel,
              this.data.primaryCovenantLevel,
              this.data.primarySpecialties.store,
            ]
          : [
              this.data.secondaryCovenantLevel,
              this.data.secondarySpecialties.store,
            ];

        this.binder.attach(watch);
      }

      if (this.is_primary) {
        this.data.ascendingLevel.subscribe(() => this.requestUpdate());
        this.data.primaryCovenantLevel.subscribe(() => this.requestUpdate());
        this.data.primarySpecialties.subscribe(() => this.requestUpdate());
      } else {
        this.data.secondaryCovenantLevel.subscribe(() => this.requestUpdate());
        this.data.secondarySpecialties.subscribe(() => this.requestUpdate());
      }

      this.requestUpdate();
    } else if (DEBUG) {
      console.warn('data not found');
    }
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();
  }

  protected override updated(_changedProperties: PropertyValues): void {
    if (DEBUG) {
      console.log(
        `${this.is_primary ? 'primary' : 'secondary'} filter updated`,
      );
    }
    this.UrlParamHandler();
  }

  protected ascendingLevelUpdate = (e: Event) => {
    const target = e.target as HTMLSelectElement;
    if (!this.data) {
      if (DEBUG) {
        console.log('data undefined');
      }
      return;
    }
    this.data.ascendingLevel.setState(target.value);
  };

  protected renderAscendingCombo = () => {
    let template = html``;
    if (this.is_primary && this.data) {
      for (const key of Object.keys(this.data.ascendingMap)) {
        const value =
          this.data.ascendingMap[key as keyof typeof this.data.ascendingMap];
        const selected = !this.data.ascendingLevel.state.localeCompare(key);
        template = html`${template}
          <option value="${key}" ?selected=${selected}>${value}</option> `;
      }
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
              @change="${this.ascendingLevelUpdate}"
            >
              ${template}
            </select>
          </div>
        </div>
      `;
    }
    return template;
  };

  static covenantCategoryOptions: CovenantCategoryOptions = {
    none: 'None',
    war: 'War',
    cooperation: 'Cooperation',
    peace: 'Peace',
    faith: 'Faith',
    honor: 'Honor',
    civilization: 'Civilization',
  };

  protected covenantLevelUpdate = (e: Event) => {
    const target = e.target as HTMLSelectElement;
    if (!this.data) {
      if (DEBUG) {
        console.log('data undefined');
      }
      return;
    }
    if (this.is_primary) {
      this.data.primaryCovenantLevel.setState(target.value);
    } else {
      this.data.secondaryCovenantLevel.setState(target.value);
    }
  };

  protected renderCovenantCombo = () => {
    let template = html``;
    if (this.data) {
      for (const key of Object.keys(PairFilter.covenantCategoryOptions)) {
        const value =
          PairFilter.covenantCategoryOptions[
            key as keyof typeof PairFilter.covenantCategoryOptions
          ];
        let selected;
        if (this.is_primary) {
          selected = !this.data.primaryCovenantLevel.state.localeCompare(key);
        } else {
          selected = !this.data.secondaryCovenantLevel.state.localeCompare(key);
        }
        template = html`${template}
          <option value="${key}" ?selected=${selected}>${value}</option> `;
      }
      template = html`
        <div class="spectrum-Form-item">
          <label
            for="covenantCategory"
            class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
          >
            Covenant Level:
          </label>
          <div class="spectrum-Form-itemField">
            <select
              id="covenantCategory"
              name="covenantCategory"
              class="spectrum-Picker spectrum-Picker--sizeM"
              @change="${this.covenantLevelUpdate}"
            >
              ${template}
            </select>
          </div>
        </div>
      `;
    }
    return template;
  };

  static specialtyLevelOptions: SpecialtyLevelOptions = {
    none: 'None',
    green: 'Green',
    blue: 'Blue',
    purple: 'Purple',
    orange: 'Orange',
    gold: 'Gold',
  };

  protected renderSpecialtyCombos = () => {
    if (!this.data) return html``;

    const S = this.is_primary
      ? this.data.primarySpecialties
      : this.data.secondarySpecialties;

    const allOptions = [...SpecialtyLevelValues.values];

    const optionNode = (idx: 1 | 2 | 3 | 4, opt: SpecialtyLevelValues) => {
      const selected = S.get(idx) === opt;

      // For the 4th, disable invalid options depending on first three
      const disabled = idx === 4 ? S.fourthDisabled(opt) : false;

      return html`
        <option value=${opt} ?selected=${selected} ?disabled=${disabled}>
          ${PairFilter.specialtyLevelOptions[opt]}
        </option>
      `;
    };

    const onChange = (idx: 1 | 2 | 3 | 4) => (e: Event) => {
      const v = (e.currentTarget as HTMLSelectElement)
        .value as SpecialtyLevelValues;
      // set() will validate + enforce constraints
      S.set(idx, v);
    };

    return html`
      ${[1, 2, 3, 4].map((i) => {
        const idx = i as 1 | 2 | 3 | 4;
        const options = idx === 4 ? S.fourthOptions() : allOptions;

        return html`
          <div class="spectrum-Form-item">
            <label
              for="specialty${idx}"
              class="spectrum-FieldLabel spectrum-FieldLabel--sizeM"
            >
              Specialty ${idx}:
            </label>
            <div class="spectrum-Form-itemField">
              <select
                id="specialty${idx}"
                name="specialty${idx}"
                class="spectrum-Picker spectrum-Picker--sizeM"
                @change=${onChange(idx)}
              >
                ${options.map((opt) => optionNode(idx, opt))}
              </select>
            </div>
          </div>
        `;
      })}
    `;
  };

  private UrlParamHandler = () => {
    if (!this.data) return;
    const S = this.is_primary
      ? this.data.primarySpecialties
      : this.data.secondarySpecialties;
    return this.is_primary
      ? ([
          {
            key: 'ascendingLevel',
            get: () => this.data!.ascendingLevel.state,
            set: (v: string) => this.data!.ascendingLevel.setState(v),
          },
          {
            key: 'primaryCovenantLevel',
            get: () => this.data!.primaryCovenantLevel.state,
            set: (v: string) => this.data!.primaryCovenantLevel.setState(v),
          },
          {
            key: 'primarySpecialty1',
            get: () => S.get(1),
            set: (v: string) => S.set(1, v as SpecialtyLevelValues),
          },
          {
            key: 'primarySpecialty2',
            get: () => S.get(2),
            set: (v: string) => S.set(2, v as SpecialtyLevelValues),
          },
          {
            key: 'primarySpecialty3',
            get: () => S.get(3),
            set: (v: string) => S.set(3, v as SpecialtyLevelValues),
          },
          {
            key: 'primarySpecialty4',
            get: () => S.get(4),
            set: (v: string) => S.set(4, v as SpecialtyLevelValues),
          },
        ] as ParamRow[])
      : ([
          {
            key: 'secondaryCovenantLevel',
            get: () => this.data!.secondaryCovenantLevel.state,
            set: (v: string) => this.data!.secondaryCovenantLevel.setState(v),
          },
          {
            key: 'secondarySpecialty1',
            get: () => S.get(1),
            set: (v: string) => S.set(1, v as SpecialtyLevelValues),
          },
          {
            key: 'secondarySpecialty2',
            get: () => S.get(2),
            set: (v: string) => S.set(2, v as SpecialtyLevelValues),
          },
          {
            key: 'secondarySpecialty3',
            get: () => S.get(3),
            set: (v: string) => S.set(3, v as SpecialtyLevelValues),
          },
          {
            key: 'secondarySpecialty4',
            get: () => S.get(4),
            set: (v: string) => S.set(4, v as SpecialtyLevelValues),
          },
        ] as ParamRow[]);
  };

  protected render(): TemplateResult {
    return html`
      <div class="settings-form">
        <h3 class="spectrum-Heading spectrum-Heading--sizeS">
          ${this.form_title}
        </h3>
        <form
          id="settings-form"
          class="spectrum-Form spectrum-Form--labelsAbove spectrum-Form--sizeM"
        >
          ${this.renderAscendingCombo()} ${this.renderCovenantCombo()}
          ${this.renderSpecialtyCombos()}
        </form>
      </div>
      <slot></slot>
    `;
  }
}

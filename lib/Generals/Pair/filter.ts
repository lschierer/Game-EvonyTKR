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

  protected data: PairData | undefined;

  @property({ type: Boolean })
  public is_primary: boolean = false;

  //protected createRenderRoot() {
  //  return this;
  //}

  protected firstUpdated(_changedProperties: PropertyValues): void {
    super.firstUpdated(_changedProperties);
    const qr = this.querySelector('pair-data');
    if (qr) {
      if (DEBUG) {
        console.log('found data');
      }
      this.data = qr as PairData;
      this.data.ascendingLevel.subscribe(() => this.requestUpdate());
      this.requestUpdate();
    } else if (DEBUG) {
      console.warn('data not found');
    }
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

  protected render(): TemplateResult {
    return html`<slot></slot> ${this.renderAscendingCombo()}`;
  }
}

export const prerender = true;

import type { GetFrontmatter } from "@greenwood/cli";
import { Constants, type Generals, type Buff } from "@evonytkrtips/schemas";
import { generateValidPairs } from "../../lib/PairingService.ts";
import { pairSummary } from "../../lib/BuffSummaryService.ts";

import debugFunction from "../../lib/debug.ts";
const DEBUG = debugFunction(new URL(import.meta.url).pathname);
console.log(`DEBUG for ${new URL(import.meta.url).pathname} is ${DEBUG}`);

const getFrontmatter: GetFrontmatter = async () => {
  return Promise.resolve({
    title: "General Comparison",
    layout: "standard",
    imports: ['/components/generals/PairsTable.ts type="module"'],
  });
};

export { getFrontmatter };

// This class will be pre-rendered by Greenwood
export default class PairGeneralComparisonPage extends HTMLElement {
  private data: Generals.PairTableData[] = [];
  private buffFilter: Constants.BuffActivation[] = [
    Constants.BuffActivation.Enum.Overall,
  ];
  accessor generalType: Constants.GeneralType | null = null;
  private sortColumn: string = "attack";
  private sortDirection: "asc" | "desc" = "desc";

  constructor() {
    super();
    if (DEBUG) {
      console.log(`GeneralComparisonPage being constructed`);
    }
  }

  // This method will be called during SSR
  async connectedCallback() {
    if (DEBUG) {
      console.log("GeneralComparisonPage connected, loading data");
    }

    // Then load data and update
    await this.loadData();

    if (DEBUG) {
      console.log(`Data loaded: ${this.data.length} generals, rendering...`);
    }

    this.render();
  }

  private async loadData() {
    for (const entry of await generateValidPairs()) {
      const primary = entry[0];
      const secondary = entry[1];

      if (DEBUG) {
        console.log(`processing pair ${primary.name}/${secondary.name}`);
      }

      if (this.generalType) {
        if (!primary.type.length) {
          continue;
        } else if (!primary.type.includes(this.generalType)) {
          continue;
        } else if (!secondary.type.length) {
          continue;
        } else if (!secondary.type.includes(this.generalType)) {
          continue;
        }
      } else if (DEBUG) {
        console.log(
          `no filter for type, accepting ${primary.name}/${secondary.name}`
        );
      }

      const mergedSummary = await pairSummary(primary, secondary);

      // Add march capacity field for specialist generals
      const isSpecialist: boolean =
        this.generalType !== null &&
        (
          [
            Constants.GeneralType.Enum.ground_specialist,
            Constants.GeneralType.Enum.mounted_specialist,
            Constants.GeneralType.Enum.ranged_specialist,
            Constants.GeneralType.Enum.siege_specialist,
          ] as Constants.GeneralType[]
        ).includes(this.generalType);

      // Create table data object
      const tableData: Generals.PairTableData = {
        primary: primary.name,
        secondary: secondary.name,
        attack: 0,
        defense: 0,
        hp: 0,
        attackDebuff: 0,
        defenseDebuff: 0,
        hpDebuff: 0,
      };

      if (isSpecialist) {
        tableData.marchCapacity = 0;
      }

      // Process each buff
      for (const buff of mergedSummary.summary) {
        let wanted: number = 0;
        if (this.generalType) {
          wanted = this.filterBuffs(buff, this.generalType);
        } else {
          wanted = 1;
        }

        if (wanted) {
          let isDebuff = false;
          if (buff.condition && buff.condition.length) {
            for (const condition of buff.condition) {
              if (
                (Constants.DebuffCondition.options as string[]).includes(
                  condition
                )
              ) {
                isDebuff = true;
                break;
              }
            }
          }

          // Add values to the appropriate fields
          if (buff.attribute === "Attack") {
            if (!isDebuff) {
              tableData.attack += buff.totalValue;
            } else {
              tableData.attackDebuff += buff.totalValue;
            }
          } else if (buff.attribute === "Defense") {
            if (!isDebuff) {
              tableData.defense += buff.totalValue;
            } else {
              tableData.defenseDebuff += buff.totalValue;
            }
          } else if (buff.attribute === "HP") {
            if (!isDebuff) {
              tableData.hp += buff.totalValue;
            } else {
              tableData.hpDebuff += buff.totalValue;
            }
          } else if (
            buff.attribute ===
              Constants.Attribute.Enum["March Size Capacity"] &&
            tableData.marchCapacity !== undefined
          ) {
            tableData.marchCapacity += buff.totalValue;
          }
        }
      }
      this.data.push(tableData);
    }
    // Sort data by attack value for demonstration
    this.sortData();
  }

  private sortData() {
    const column = this.sortColumn;
    const direction = this.sortDirection;

    this.data.sort((a, b) => {
      const valueA = a[column as keyof Generals.PairTableData];
      const valueB = b[column as keyof Generals.PairTableData];

      if (typeof valueA === "number" && typeof valueB === "number") {
        return direction === "asc" ? valueA - valueB : valueB - valueA;
      } else if (typeof valueA === "string" && typeof valueB === "string") {
        return direction === "asc"
          ? valueA.localeCompare(valueB)
          : valueB.localeCompare(valueA);
      }
      return 0;
    });
  }

  protected filterBuffs(
    buff: Buff.SummarizedBuff,
    wantedType: Constants.GeneralType
  ): number {
    const filter = new Set<Constants.Condition>();
    if (this.buffFilter.length) {
      for (const bf of this.buffFilter) {
        const f = Constants.AllowedBuffConditions.get(bf);
        if (f) {
          f.map((fi) => filter.add(fi));
        }
      }
    }

    let isDebuff = false;

    if (buff.condition && buff.condition.length) {
      for (const condition of buff.condition) {
        if (
          (Constants.DebuffCondition.options as string[]).includes(condition)
        ) {
          isDebuff = true;
          continue;
        } else if (filter.size) {
          if (![...filter].includes(condition)) {
            return 0;
          }
        }
      }
    }

    if (buff.class) {
      if (
        this.buffFilter.length &&
        !this.buffFilter.includes(Constants.BuffActivation.Enum.Wall)
      ) {
        if (!isDebuff) {
          if (buff.class == Constants.TroopClass.Enum["Ground Troops"]) {
            if (wantedType !== Constants.GeneralType.Enum.ground_specialist) {
              return 0;
            }
          }
          if (buff.class == Constants.TroopClass.Enum["Ranged Troops"]) {
            if (wantedType !== Constants.GeneralType.Enum.ranged_specialist) {
              return 0;
            }
          }
          if (buff.class == Constants.TroopClass.Enum["Mounted Troops"]) {
            if (wantedType !== Constants.GeneralType.Enum.mounted_specialist) {
              return 0;
            }
          }
          if (buff.class == Constants.TroopClass.Enum["Siege Machines"]) {
            if (wantedType !== Constants.GeneralType.Enum.siege_specialist) {
              return 0;
            }
          }
        }
      }
    }

    return buff.totalValue;
  }

  protected buffFilterChanged(
    value: "overall" | "attack" | "rein" | "wall" | "monster"
  ) {
    if (value === "overall") {
      this.buffFilter = [Constants.BuffActivation.Enum.Overall];
    } else if (value === "attack") {
      this.buffFilter = [
        Constants.BuffActivation.Enum.Overall,
        Constants.BuffActivation.Enum.Attacking,
      ];
    } else if (value === "rein") {
      this.buffFilter = [
        Constants.BuffActivation.Enum.Overall,
        Constants.BuffActivation.Enum.Defense,
        Constants.BuffActivation.Enum["In City"],
        Constants.BuffActivation.Enum.Reinforcing,
      ];
    } else if (value === "wall") {
      this.buffFilter = [
        Constants.BuffActivation.Enum.Overall,
        Constants.BuffActivation.Enum.Defense,
        Constants.BuffActivation.Enum.Wall,
      ];
    } /*(value === "monster")*/ else {
      this.buffFilter = [
        Constants.BuffActivation.Enum.Overall,
        Constants.BuffActivation.Enum.Attacking,
        Constants.BuffActivation.Enum.PvM,
      ];
    }

    if (DEBUG) {
      console.log(`buffFilter is now ${this.buffFilter.join(", ")}`);
    }
  }

  protected pageDescription = () => {
    return `This allows you to compare the relative buffs of the General Pairs.`;
  };

  private render() {
    if (DEBUG) {
      console.log(`PairGeneralComparisonPage render function start`);
    }
    // Create HTML content
    let content = "";

    // Add header and description
    content += `
      <div class="spectrum spectrum-Typography spectrum--medium">
        <p class="spectrum-Body spectrum-Body--sizeM">
          ${this.pageDescription()}
        </p>

    `;

    if (DEBUG) {
      console.log(`adding the virtual table now`);
    }

    // Add virtual table container
    content += `
        <div
        id="table-container"

        class="virtual-table-container"
        >
          <pairs-table encoded-data="${encodeURIComponent(JSON.stringify(this.data))}"></pairs-table>
        </div>
      `;

    content += `</div>`;

    // Add styles - using the correct path from node_modules
    content += `
      <link rel="stylesheet" href="/node_modules/@evonytkrtips/assets/dist/styles/ComparisionTablePage.css" />
    `;

    // Add client-side script now included by the getLayout function

    // Set the innerHTML of the element
    this.innerHTML = content;
  }
}

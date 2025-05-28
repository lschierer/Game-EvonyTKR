export const prerender = true;

import type { GetFrontmatter } from "@greenwood/cli";
import { Constants, Generals, type Buff } from "@evonytkrtips/schemas";
import collection from "@evonytkrtips/assets/collections/generals";

import {
  getAscendingSummary,
  getSpecialitySummary,
  getSkillbookSummary,
  mergeBuffSummaries,
} from "../../lib/BuffSummaryService.ts";
import debugFunction from "../../lib/debug.ts";

const DEBUG = debugFunction(new URL(import.meta.url).pathname);

const getFrontmatter: GetFrontmatter = async () => {
  return Promise.resolve({
    title: "General Comparison",
    layout: "standard",
    imports: ['/components/generals/GeneralsTable.ts type="module"'],
  });
};

export { getFrontmatter };

// This class will be pre-rendered by Greenwood
export default class GeneralComparisonPage extends HTMLElement {
  private data: Generals.GeneralTableData[] = [];
  private error: string | null = null;
  private loading = true;
  private loadedCount = 0;
  private totalCount = 0;
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
    this.loading = false;
    this.render();
  }

  private async loadData() {
    if (collection.length) {
      this.totalCount = collection.length;
      this.loading = true;

      for (const entry of collection) {
        try {
          const importName = `@evonytkrtips/assets/collections/generals/${entry}`;
          if (DEBUG) {
            console.log(`attempting import of ${importName}`);
          }

          let generalData = (await import(importName, {
            with: { type: "json" },
          })) as object;

          // Handle different import formats
          if ("default" in generalData) {
            generalData = generalData.default as object;
          }

          const valid = Generals.General.safeParse(generalData);
          if (valid.success) {
            if (DEBUG) {
              console.log(`successful parse of ${valid.data.name}`);
            }
            if (this.generalType) {
              if (valid.data.type.length) {
                if (!valid.data.type.includes(this.generalType)) {
                  this.totalCount--;
                  continue;
                }
              } else {
                this.totalCount--;
                continue;
              }
            } else if (DEBUG) {
              console.log(`no filter for type, accepting ${valid.data.name}`);
            }
            const summaries = new Array<Buff.BuffSummaryResponse>();

            // Process specialities
            if (valid.data.specialities.length) {
              for (const speciality of valid.data.specialities) {
                const summary = await getSpecialitySummary(speciality);
                if (!summary.error) {
                  summaries.push(summary);
                }
              }
            }

            // Process ascending
            if (valid.data.ascending) {
              const summary = await getAscendingSummary(valid.data.name);
              if (!summary.error) {
                summaries.push(summary);
              }
            }

            // Process skillbook
            if (valid.data.book) {
              const summary = await getSkillbookSummary(valid.data.book);
              if (!summary.error) {
                summaries.push(summary);
              }
            }

            // Merge all buff summaries
            const mergedSummary = mergeBuffSummaries(summaries);

            // Create table data object
            const tableData: Generals.GeneralTableData = {
              name: valid.data.name,
              attack: 0,
              defense: 0,
              hp: 0,
              attackDebuff: 0,
              defenseDebuff: 0,
              hpDebuff: 0,
            };

            // Add march capacity field for specialist generals
            const isSpecialist = valid.data.type.some(
              (type) =>
                type === Constants.GeneralType.Enum.ground_specialist ||
                type === Constants.GeneralType.Enum.mounted_specialist ||
                type === Constants.GeneralType.Enum.ranged_specialist ||
                type === Constants.GeneralType.Enum.siege_specialist
            );

            if (isSpecialist) {
              tableData.marchCapacity = 0;
            }

            // Process each buff
            for (const buff of mergedSummary.summary) {
              const wanted = this.filterBuffs(buff, valid.data.type);
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
            this.loadedCount++;
          }
        } catch (error) {
          console.error(`Error processing general entry:`, error);
        }
      }

      // Sort data by attack value for demonstration
      this.sortData();
    }
  }

  private sortData() {
    const column = this.sortColumn;
    const direction = this.sortDirection;

    this.data.sort((a, b) => {
      const valueA = a[column as keyof Generals.GeneralTableData];
      const valueB = b[column as keyof Generals.GeneralTableData];

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
    generalTypes: Constants.GeneralType[]
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
            if (
              !generalTypes.includes(
                Constants.GeneralType.Enum.ground_specialist
              )
            ) {
              return 0;
            }
          }
          if (buff.class == Constants.TroopClass.Enum["Ranged Troops"]) {
            if (
              !generalTypes.includes(
                Constants.GeneralType.Enum.ranged_specialist
              )
            ) {
              return 0;
            }
          }
          if (buff.class == Constants.TroopClass.Enum["Mounted Troops"]) {
            if (
              !generalTypes.includes(
                Constants.GeneralType.Enum.mounted_specialist
              )
            ) {
              return 0;
            }
          }
          if (buff.class == Constants.TroopClass.Enum["Siege Machines"]) {
            if (
              !generalTypes.includes(
                Constants.GeneralType.Enum.siege_specialist
              )
            ) {
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
    return `This allows you to compare the relative buffs of the Generals.`;
  };

  private render() {
    if (DEBUG) {
      console.log(`GeneralComparisonPage render function start`);
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

    // Add loading indicator or error message
    if (this.loading) {
      if (DEBUG) {
        console.log(`I am still loading`);
      }
      const progressPercent =
        (this.loadedCount / Math.max(this.totalCount, 1)) * 100;
      content += `
        <div class="loading">
          <div class="progress-bar-container">
            <div class="progress-bar" style="width: ${progressPercent}%"></div>
          </div>
          <div class="progress-text">
            Loading generals: ${this.loadedCount} of ${this.totalCount}
          </div>
        </div>
      `;
    } else if (this.error) {
      if (DEBUG) {
        console.error(`Render found an error`);
      }
      content += `<div class="error">Error: ${this.error}</div>`;
    } else {
      if (DEBUG) {
        console.log(`adding the virtual table now`);
      }

      // Add virtual table container
      content += `
        <div
        id="table-container"
        data-items="${encodeURIComponent(JSON.stringify(this.data))}"
        class="virtual-table-container" style=" overflow: auto;"
        >
        <sp-table
            id="sorted-virtualized-table"
            scroller="true"
        >
          <sp-table-head>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="name">General</sp-table-head-cell>
                ${
                  this.generalType &&
                  (this.generalType ===
                    Constants.GeneralType.Enum.ground_specialist ||
                    this.generalType ===
                      Constants.GeneralType.Enum.mounted_specialist ||
                    this.generalType ===
                      Constants.GeneralType.Enum.ranged_specialist ||
                    this.generalType ===
                      Constants.GeneralType.Enum.siege_specialist)
                    ? '<sp-table-head-cell sortable sort-direction="desc" sort-key="marchCapacity">March Capacity</sp-table-head-cell>'
                    : ""
                }
                <sp-table-head-cell sortable sort-direction="desc" sort-key="attack">Attack</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="defense">Defense</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="hp">HP</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="attackDebuff">Attack Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="defenseDebuff">Defense Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="hpDebuff">HP Debuff</sp-table-head-cell>
            </sp-table-head>
            <sp-table-body></sp-table-body>
          </sp-table>
        </div>
      `;
    }

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

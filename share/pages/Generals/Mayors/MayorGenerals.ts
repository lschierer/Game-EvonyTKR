export const prerender = true;

import type { GetFrontmatter } from "@greenwood/cli";
import { Constants, Generals, type Buff } from "@evonytkrtips/schemas";
import collection from "@evonytkrtips/assets/collections/generals";

import {
  getAscendingSummary,
  getSpecialitySummary,
  getSkillbookSummary,
  mergeBuffSummaries,
} from "../../../lib/BuffSummaryService.ts";
import debugFunction from "../../../lib/debug.ts";

const DEBUG = debugFunction(new URL(import.meta.url).pathname);

const getFrontmatter: GetFrontmatter = async () => {
  return Promise.resolve({
    title: "Mayor General Comparison",
    layout: "standard",
    imports: ['/components/generals/MayorGeneralsTable.ts type="module"'],
  });
};

export { getFrontmatter };

// This class will be pre-rendered by Greenwood
export default class MayorGeneralComparisonPage extends HTMLElement {
  private data: Generals.MayorGeneralTableData[] = [];
  private error: string | null = null;
  private loading = true;
  private loadedCount = 0;
  private totalCount = 0;
  private buffFilter: Constants.BuffActivation[] = [
    Constants.BuffActivation.Enum.Overall,
    Constants.BuffActivation.Enum.Defense,
  ];
  accessor generalType: Constants.GeneralType =
    Constants.GeneralType.Enum.mayor;
  private sortColumn: string = "groundAttackDebuff";
  private sortDirection: "asc" | "desc" = "desc";

  constructor() {
    super();
    if (DEBUG) {
      console.log(`MayorGeneralComparisonPage being constructed`);
    }
  }

  // This method will be called during SSR
  async connectedCallback() {
    if (DEBUG) {
      console.log("MayorGeneralComparisonPage connected, loading data");
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

            // Filter for mayor generals
            if (!valid.data.type.includes(this.generalType)) {
              this.totalCount--;
              continue;
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

            // Create table data object with only debuff columns
            const tableData: Generals.MayorGeneralTableData = {
              name: valid.data.name,
              // Ground troops debuffs
              groundAttackDebuff: 0,
              groundDefenseDebuff: 0,
              groundHPDebuff: 0,
              // Mounted troops debuffs
              mountedAttackDebuff: 0,
              mountedDefenseDebuff: 0,
              mountedHPDebuff: 0,
              // Ranged troops debuffs
              rangedAttackDebuff: 0,
              rangedDefenseDebuff: 0,
              rangedHPDebuff: 0,
              // Siege machines debuffs
              siegeAttackDebuff: 0,
              siegeDefenseDebuff: 0,
              siegeHPDebuff: 0,
            };

            // Process each buff - only looking for debuffs
            for (const buff of mergedSummary.summary) {
              const wanted = this.filterBuffs(buff);
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

                // Only process debuffs
                if (isDebuff) {
                  // Determine which troop class this buff applies to
                  const troopClass =
                    buff.class || Constants.TroopClass.Enum.All;

                  // Add values to the appropriate debuff fields based on troop class and attribute
                  if (buff.attribute === "Attack") {
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Ground Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.groundAttackDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Mounted Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.mountedAttackDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Ranged Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.rangedAttackDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Siege Machines"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.siegeAttackDebuff += buff.totalValue;
                    }
                  } else if (buff.attribute === "Defense") {
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Ground Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.groundDefenseDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Mounted Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.mountedDefenseDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Ranged Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.rangedDefenseDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Siege Machines"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.siegeDefenseDebuff += buff.totalValue;
                    }
                  } else if (buff.attribute === "HP") {
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Ground Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.groundHPDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Mounted Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.mountedHPDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Ranged Troops"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.rangedHPDebuff += buff.totalValue;
                    }
                    if (
                      troopClass ===
                        Constants.TroopClass.Enum["Siege Machines"] ||
                      troopClass === Constants.TroopClass.Enum.All
                    ) {
                      tableData.siegeHPDebuff += buff.totalValue;
                    }
                  }
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

      // Sort data by default column
      this.sortData();
    }
  }

  private sortData() {
    const column = this.sortColumn;
    const direction = this.sortDirection;

    this.data.sort((a, b) => {
      const valueA = a[column as keyof Generals.MayorGeneralTableData];
      const valueB = b[column as keyof Generals.MayorGeneralTableData];

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

  protected filterBuffs(buff: Buff.SummarizedBuff): number {
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

    // Only return value if it's a debuff
    return isDebuff ? buff.totalValue : 0;
  }

  protected pageDescription = () => {
    return `This allows you to compare the debuff capabilities of Mayor Generals, broken down by troop type.`;
  };

  private render() {
    if (DEBUG) {
      console.log(`MayorGeneralComparisonPage render function start`);
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
        console.log(`adding the virtual tables now`);
      }

      // Store the data for all tables
      content += `
        <div
        id="mayor-table-container"
        data-items="${encodeURIComponent(JSON.stringify(this.data))}"
        class="virtual-table-container" style="overflow: auto;"
        >
      `;

      // Table 1: Attack Debuffs
      content += `
        <h2>Attack Debuffs</h2>
        <sp-table
            id="mayor-attack-table"
            scroller="true"
            data-table-type="attack"
        >
          <sp-table-head>
                <sp-table-head-cell>#</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="name">General</sp-table-head-cell>

                <!-- Attack Debuffs -->
                <sp-table-head-cell sortable sort-direction="desc" sort-key="groundAttackDebuff">Ground Attack Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="mountedAttackDebuff">Mounted Attack Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="rangedAttackDebuff">Ranged Attack Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="siegeAttackDebuff">Siege Attack Debuff</sp-table-head-cell>
            </sp-table-head>
            <sp-table-body></sp-table-body>
        </sp-table>
      `;

      // Table 2: HP Debuffs
      content += `
        <h2>HP Debuffs</h2>
        <sp-table
            id="mayor-hp-table"
            scroller="true"
            data-table-type="hp"
        >
          <sp-table-head>
                <sp-table-head-cell>#</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="name">General</sp-table-head-cell>

                <!-- HP Debuffs -->
                <sp-table-head-cell sortable sort-direction="desc" sort-key="groundHPDebuff">Ground HP Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="mountedHPDebuff">Mounted HP Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="rangedHPDebuff">Ranged HP Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="siegeHPDebuff">Siege HP Debuff</sp-table-head-cell>
            </sp-table-head>
            <sp-table-body></sp-table-body>
        </sp-table>
      `;

      // Table 3: Defense Debuffs
      content += `
        <h2>Defense Debuffs</h2>
        <sp-table
            id="mayor-defense-table"
            scroller="true"
            data-table-type="defense"
        >
          <sp-table-head>
                <sp-table-head-cell>#</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="name">General</sp-table-head-cell>

                <!-- Defense Debuffs -->
                <sp-table-head-cell sortable sort-direction="desc" sort-key="groundDefenseDebuff">Ground Defense Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="mountedDefenseDebuff">Mounted Defense Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="rangedDefenseDebuff">Ranged Defense Debuff</sp-table-head-cell>
                <sp-table-head-cell sortable sort-direction="desc" sort-key="siegeDefenseDebuff">Siege Defense Debuff</sp-table-head-cell>
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

    // Set the innerHTML of the element
    this.innerHTML = content;
  }
}

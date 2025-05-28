export const prerender = true;

import type { GetFrontmatter, Compilation } from "@greenwood/cli";

import { getFrontmatter as comparisonFrontmatter } from "../../comparison.ts";
import GeneralComparisonPage from "../../comparison.ts";
import { Constants } from "@evonytkrtips/schemas";

const getFrontmatter: GetFrontmatter = async (
  compilation: Compilation,
  route: string
) => {
  const cf = await comparisonFrontmatter(compilation, route);
  cf.title = "Generic Archers Generals Comparison";
  return cf;
};

export { getFrontmatter };

export default class AttackingCavComparison extends GeneralComparisonPage {
  constructor() {
    super();
    super.buffFilterChanged("overall");
    super.generalType = Constants.GeneralType.Enum.ranged_specialist;
  }

  protected override pageDescription = () => {
    return `This allows you to compare the relative buffs of the Archers Generals during player-versus-player situations where only generic buffs are active.`;
  };
}

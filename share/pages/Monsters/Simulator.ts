import type { GetFrontmatter, Frontmatter } from "@greenwood/cli";

type ExtendedFrontmatter = Omit<Frontmatter, "data"> & {
  data?: {
    [key: string]: string | string[];
  };
};

function getBody() {
  return `
  <article class="page">

    <h2 class="spectrum-Heading spectrum-Heading--sizeXL">
      Credits and Instructions
    </h2>
    <section class="credits">
      This is based almost entirely on the spreadsheet created by
      <a href="https://www.youtube.com/@DerrickDefies">Derrick Defies</a>
      and provided by him in his Discord server. I have attempted to
      faithfully recreate it here.
    </section>
    <section class="instructions">
      <ol>
        <li>Attack the Alliance monster with a full mounted march.</li>
        <li>
          Insert the damage from the report in the "Boss Damage" field.
        </li>
        <li>
          Input your Troop type and March size in the indicated fields.
        </li>
        <li>
          Adjust the Alliance Boss modifier.
          <ul>
            <li>
              The original spreadsheet is unclear how to figure out the
              *correct* value to put here.
            </li>
            <li>Per the original, 100% works.</li>
            <li>
              Per the original, do not use 20%, Evony as a bug in their
              programming.
            </li>
          </ul>
        </li>
        <li>
          Accurately fill out your flat buffs for that troop type. This goes
          on line 15, and you will only fill in 3 of the available nine
          columns (one attack column, one defense column, one hp column).
          Ensure you look at each of the following (and anything I might
          have missed)
          <ul>
            <li>Refines for anything that can be refined.</li>
            <li>Covenants</li>
            <li>Flexible specialties</li>
            <li>Castles that might have buffs simply for owning them</li>
            <li>Detail Tab</li>
            <li>General Skins</li>
          </ul>
        </li>
      </ol>
    </section>
    <h2 class="spectrum-Heading spectrum-Heading--sizeXL">The Simulator</h2>
    <section class="simulator">
      <script src="/components/Monsters/Simulator/index.ts" type="module"></script>
      <monster-simulator></monster-simulator>
    </section>
  </article>
  `;
}

const getFrontmatter: GetFrontmatter = async () => {
  /*start work around for GetFrontmatter requiring async */
  await new Promise((resolve) => setTimeout(resolve, 1));
  /* end workaround */

  const title = "Monster Simulator";
  const fm: ExtendedFrontmatter = {
    title,
    layout: "standard",
    data: {
      authors: ["Luke Schierer", "Derrick Defies"],
    },
  };
  return fm as Frontmatter;
};

export { getFrontmatter, getBody };

import * as z from "zod";

const input = `
1 StarAttacking Mounted Troop Defense +20% and HP +20%
2 StarAttacking Troop Death into Wounded Rate +8%
3 StarMounted Troop Attack +30%, Mounted Troop and Ground Troop Defense +15%
4 StarAttacking Mounted Troop Attack +20%, Attacking Ground Troop and Mounted Troop HP +20%
5 StarWhen attacking, Ground Troop and Mounted Troop Attack +40%, Mounted Troop Defense and HP +25%
`;

const attributeMap: Record<string, string> = {
  Attack: "Attack",
  Defense: "Defense",
  HP: "HP",
  "Troop Death into Wounded Rate": "Death to Wounded",
  "March Size Increase": "March Size Capacity",
};

const classMap: Record<string, string> = {
  "Ground Troop": "Ground Troops",
  "Mounted Troop": "Mounted Troops",
  "Ranged Troop": "Ranged Troops",
  "Siege Machine": "Siege Machines",
};

const conditionMap: Record<string, string> = {
  Attacking: "Attacking",
  "When attacking": "Attacking",
};

const levelMap = {
  "1": "red1",
  "2": "red2",
  "3": "red3",
  "4": "red4",
  "5": "red5",
} as const;

function parseLine(line: string) {
  const match = line.match(/^(\d) Star(.*)$/);
  if (!match) return null;
  const level = levelMap[match[1] as keyof typeof levelMap];
  const rest = match[2].trim();

  const buffs: any[] = [];

  // Split on , or " and "
  const parts = rest.split(/,| and /).map((p) => p.trim());

  for (let part of parts) {
    let condition: string[] = [];
    for (const cond in conditionMap) {
      if (part.includes(cond)) {
        condition.push(conditionMap[cond]);
        part = part.replace(cond, "").trim();
      }
    }

    let match = part.match(
      /(.*?)(Attack|Defense|HP|Troop Death into Wounded Rate)\s*\+(\d+)%/,
    );
    if (!match) continue;

    const fullAttr = match[2].trim();
    const number = parseInt(match[3]);

    const attr = attributeMap[fullAttr] || fullAttr;
    const troopMatch = match[1]
      .trim()
      .match(/(Mounted|Ground|Ranged|Siege) Troop/);
    const classStr = troopMatch ? classMap[troopMatch[0]] : undefined;

    buffs.push({
      attribute: attr,
      value: { number, unit: "percentage" },
      ...(classStr ? { class: classStr } : {}),
      ...(condition.length ? { condition } : {}),
    });
  }

  return {
    level,
    buff: buffs,
  };
}

function parseInput(input: string) {
  const lines = input.trim().split("\n");
  const ascending = lines.map(parseLine).filter(Boolean);
  return { ascending };
}

console.log(JSON.stringify(parseInput(input), null, 2));

#!/usr/bin/env python3
"""
Extract monster simulator data from Derrick Defies Monster Simulator Excel file.

Outputs YAML files compatible with Game::EvonyTKR data patterns:
- share/collections/data/monsters/monsters.yaml - all monster data
- share/collections/data/monster_simulator/troop_base_stats.yaml - tier/type base stats
- share/collections/data/monster_simulator/modifiers.yaml - world boss, alliance boss modifiers
"""

import sys
from pathlib import Path
from openpyxl import load_workbook
import yaml

# Project root
PROJECT_ROOT = Path(__file__).parent.parent
EXCEL_FILE = PROJECT_ROOT / "share" / "Derrick Defies Monster Simulator K50.xlsx"
OUTPUT_DIR = PROJECT_ROOT / "share" / "collections" / "data"


def extract_monsters(wb):
    """Extract monster data from Monsters tab."""
    ws = wb["Monsters"]

    # Column mappings based on row 2 headers
    # We'll read them dynamically but here are the key ones:
    # A: Order #, B: Name, C: Alt Name, D: Monster #, E: Level
    # K: Stamina, L: # of troops, M: Attack, N: Defense, O: HP
    # AN-AQ: Ground/Archer/Mounted/Siege modifiers (as percentages)
    # Column 95 (CQ): Monster type (0=Boss, 1=Common, 2=Summon, etc.)

    monsters = []

    # Get header row to find column indices
    headers = {}
    for col in range(1, ws.max_column + 1):
        val = ws.cell(row=2, column=col).value
        if val:
            headers[val] = col

    print(f"Found headers: {list(headers.keys())[:15]}...")

    # Read monster data starting from row 3
    for row in range(3, ws.max_row + 1):
        order_num = ws.cell(row=row, column=1).value
        if order_num is None:
            continue

        name = ws.cell(row=row, column=2).value
        if not name:
            continue

        monster = {
            "order": int(order_num) if order_num else None,
            "name": str(name).strip() if name else None,
            "alt_name": ws.cell(row=row, column=3).value,
            "monster_id": int(ws.cell(row=row, column=4).value) if ws.cell(row=row, column=4).value else None,
            "level": int(ws.cell(row=row, column=5).value) if ws.cell(row=row, column=5).value else None,
            "stamina": int(ws.cell(row=row, column=11).value) if ws.cell(row=row, column=11).value else None,
            "troop_count": int(ws.cell(row=row, column=12).value) if ws.cell(row=row, column=12).value else None,
            "attack": int(ws.cell(row=row, column=13).value) if ws.cell(row=row, column=13).value else None,
            "defense": int(ws.cell(row=row, column=14).value) if ws.cell(row=row, column=14).value else None,
            "hp": int(ws.cell(row=row, column=15).value) if ws.cell(row=row, column=15).value else None,
        }

        # Spawn rates (columns F-J, areas 1-5)
        spawn_rates = []
        for col in range(6, 11):
            rate = ws.cell(row=row, column=col).value
            spawn_rates.append(int(rate) if rate else 0)
        monster["spawn_rates"] = spawn_rates

        # Troop type modifiers (columns AN-AQ, indices 40-43)
        # These are percentages that modify damage based on troop type
        modifiers = {}
        modifier_cols = {"ground": 40, "archer": 41, "mounted": 42, "siege": 43}
        for troop_type, col in modifier_cols.items():
            val = ws.cell(row=row, column=col).value
            if val is not None:
                modifiers[troop_type] = float(val) / 100.0  # Convert percentage to decimal
        if modifiers:
            monster["troop_modifiers"] = modifiers

        # Monster type (column CQ = 95, but may vary)
        # 0=Boss, 1=Common, 2=Summon, 3=Collection, 4=Resource, 5=Pyramid
        monster_type_col = 95
        monster_type = ws.cell(row=row, column=monster_type_col).value
        if monster_type is not None:
            type_map = {
                0: "boss",
                1: "common",
                2: "summon",
                3: "collection",
                4: "resource",
                5: "pyramid",
            }
            monster["monster_type"] = type_map.get(int(monster_type), "unknown")

        # Rewards
        rewards = {}
        monarch_xp = ws.cell(row=row, column=18).value
        if monarch_xp:
            rewards["monarch_xp"] = int(monarch_xp)
        prestige = ws.cell(row=row, column=19).value
        if prestige:
            rewards["prestige"] = int(prestige)
        alliance_points = ws.cell(row=row, column=20).value
        if alliance_points:
            rewards["alliance_points"] = int(alliance_points)
        general_xp = ws.cell(row=row, column=54).value
        if general_xp:
            rewards["general_xp"] = int(general_xp)
        if rewards:
            monster["rewards"] = rewards

        # Only add if we have meaningful data
        if monster["name"] and monster["level"]:
            monsters.append(monster)

    print(f"Extracted {len(monsters)} monsters")
    return monsters


def extract_reference_tables(wb):
    """Extract reference table data for troop stats and modifiers."""
    ws = wb["Reference Table"]

    data = {
        "world_boss_modifiers": {},
        "tier_modifiers_vs_boss": {},
        "alliance_boss_modifiers": {},
        "troop_base_attack": {},
        "troop_base_defense": {},
        "troop_base_hp": {},
    }

    # World Boss modifiers (rows 2-7)
    # Row 2: headers (Lord of Lava=321, Thunder Scorpion=322, etc.)
    # Rows 4-7: Siege, Archer, Mounted, Ground modifiers
    world_bosses = {}
    boss_names = {
        "C": ("Lord of Lava", 321),
        "D": ("Thunder Scorpion", 322),
        "E": ("Behemoth King", 323),
        "F": ("Bird of Hurricane", 324),
    }
    for col_letter, (name, order) in boss_names.items():
        col = ord(col_letter) - ord("A") + 1
        world_bosses[name] = {
            "order": order,
            "siege": float(ws.cell(row=4, column=col).value or 1),
            "archer": float(ws.cell(row=5, column=col).value or 1),
            "mounted": float(ws.cell(row=6, column=col).value or 1),
            "ground": float(ws.cell(row=7, column=col).value or 1),
        }
    data["world_boss_modifiers"] = world_bosses

    # Tier modifiers vs Boss monsters (rows 10-14)
    # Row 10: headers T1-T17
    # Rows 11-14: Siege, Archer, Mounted, Ground
    tier_mods = {}
    tiers = ["T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10",
             "T11", "T12", "T13", "T14", "T15", "T16", "T17"]
    for i, tier in enumerate(tiers):
        col = 3 + i  # Starting at column C
        tier_mods[tier] = {
            "siege": float(ws.cell(row=11, column=col).value or 1),
            "archer": float(ws.cell(row=12, column=col).value or 1),
            "mounted": float(ws.cell(row=13, column=col).value or 1),
            "ground": float(ws.cell(row=14, column=col).value or 1),
        }
    data["tier_modifiers_vs_boss"] = tier_mods

    # Alliance Boss modifiers (rows 17-21)
    alliance_mods = {}
    for i, tier in enumerate(tiers):
        col = 3 + i
        val_siege = ws.cell(row=18, column=col).value
        val_archer = ws.cell(row=19, column=col).value
        val_mounted = ws.cell(row=20, column=col).value
        val_ground = ws.cell(row=21, column=col).value
        if any([val_siege, val_archer, val_mounted, val_ground]):
            alliance_mods[tier] = {
                "siege": float(val_siege) if val_siege else 1,
                "archer": float(val_archer) if val_archer else 1,
                "mounted": float(val_mounted) if val_mounted else 1,
                "ground": float(val_ground) if val_ground else 1,
            }
    data["alliance_boss_modifiers"] = alliance_mods

    # Troop base Attack stats (rows 31-48)
    # Row 31: headers Ground, Archer, Mounted, Siege
    # Rows 32-48: T1-T17
    base_attack = {}
    for i, tier in enumerate(tiers):
        row = 32 + i
        base_attack[tier] = {
            "ground": int(ws.cell(row=row, column=3).value or 0),
            "archer": int(ws.cell(row=row, column=4).value or 0),
            "mounted": int(ws.cell(row=row, column=5).value or 0),
            "siege": int(ws.cell(row=row, column=6).value or 0),
        }
    data["troop_base_attack"] = base_attack

    # Troop base Defense stats (rows 49-66)
    base_defense = {}
    for i, tier in enumerate(tiers):
        row = 50 + i
        base_defense[tier] = {
            "ground": int(ws.cell(row=row, column=3).value or 0),
            "archer": int(ws.cell(row=row, column=4).value or 0),
            "mounted": int(ws.cell(row=row, column=5).value or 0),
            "siege": int(ws.cell(row=row, column=6).value or 0),
        }
    data["troop_base_defense"] = base_defense

    # Troop base HP stats (rows 67-84)
    base_hp = {}
    for i, tier in enumerate(tiers):
        row = 68 + i
        base_hp[tier] = {
            "ground": int(ws.cell(row=row, column=3).value or 0),
            "archer": int(ws.cell(row=row, column=4).value or 0),
            "mounted": int(ws.cell(row=row, column=5).value or 0),
            "siege": int(ws.cell(row=row, column=6).value or 0),
        }
    data["troop_base_hp"] = base_hp

    return data


def extract_formulas(wb):
    """Extract key formulas from Monster Simulator tab for documentation."""
    from openpyxl.worksheet.formula import ArrayFormula

    ws = wb["Monster Simulator"]

    formulas = {
        "description": "Key formulas extracted from Monster Simulator tab",
        "player_stats": {},
        "monster_stats": {},
        "damage_calculations": {},
    }

    # Key formula cells we identified earlier
    formula_cells = {
        # Player final stats
        "player_attack_buff_pct": "J23",
        "player_attack_buff_flat": "K23",
        "player_final_attack": "L23",
        "player_defense_buff_pct": "J24",
        "player_defense_buff_flat": "K24",
        "player_final_defense": "L24",
        "player_hp_buff_pct": "J25",
        "player_hp_buff_flat": "K25",
        "player_final_hp": "L25",

        # Troop modifier
        "troop_modifier": "E28",

        # Monster stats (with debuffs)
        "monster_final_attack": "K28",
        "monster_final_defense": "K29",
        "monster_final_hp": "K30",

        # Other calculations
        "troop_modifier_lookup": "D32",
        "minimum_troops_to_kill": "M32",
        "true_attack_buff": "M46",
        "true_hp_buff": "N54",
        "true_defense_buff": "N62",
    }

    for name, coord in formula_cells.items():
        cell = ws[coord]
        if isinstance(cell.value, ArrayFormula):
            formulas["player_stats" if "player" in name else "monster_stats" if "monster" in name else "damage_calculations"][name] = {
                "cell": coord,
                "formula": cell.value.text,
            }
        elif isinstance(cell.value, str) and cell.value.startswith("="):
            formulas["player_stats" if "player" in name else "monster_stats" if "monster" in name else "damage_calculations"][name] = {
                "cell": coord,
                "formula": cell.value,
            }

    return formulas


def save_yaml(data, filepath):
    """Save data to YAML file with nice formatting."""
    filepath.parent.mkdir(parents=True, exist_ok=True)

    # Custom representer for cleaner output
    def str_representer(dumper, data):
        if '\n' in data:
            return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')
        return dumper.represent_scalar('tag:yaml.org,2002:str', data)

    yaml.add_representer(str, str_representer)

    with open(filepath, 'w', encoding='utf-8') as f:
        yaml.dump(data, f, default_flow_style=False, allow_unicode=True, sort_keys=False, width=120)

    print(f"Saved: {filepath}")


def main():
    print(f"Loading Excel file: {EXCEL_FILE}")

    if not EXCEL_FILE.exists():
        print(f"ERROR: Excel file not found: {EXCEL_FILE}")
        sys.exit(1)

    wb = load_workbook(EXCEL_FILE, data_only=False)
    print(f"Sheets: {wb.sheetnames}")

    # Extract and save monsters
    print("\n--- Extracting Monsters ---")
    monsters = extract_monsters(wb)
    monsters_dir = OUTPUT_DIR / "monsters"
    save_yaml({"monsters": monsters}, monsters_dir / "monsters.yaml")

    # Extract and save reference tables
    print("\n--- Extracting Reference Tables ---")
    ref_tables = extract_reference_tables(wb)
    sim_dir = OUTPUT_DIR / "monster_simulator"
    save_yaml(ref_tables, sim_dir / "reference_tables.yaml")

    # Extract and save formulas for documentation
    print("\n--- Extracting Formulas ---")
    formulas = extract_formulas(wb)
    save_yaml(formulas, sim_dir / "formulas_reference.yaml")

    print("\n--- Summary ---")
    print(f"Monsters extracted: {len(monsters)}")
    print(f"Reference tables: {list(ref_tables.keys())}")
    print(f"\nOutput files:")
    print(f"  - {monsters_dir / 'monsters.yaml'}")
    print(f"  - {sim_dir / 'reference_tables.yaml'}")
    print(f"  - {sim_dir / 'formulas_reference.yaml'}")


if __name__ == "__main__":
    main()

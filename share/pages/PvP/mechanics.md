---
title: game mechanics
author: Luke Schierer
---


- battle field size is fixed despite the many sites.
- traps have priorities, but will target anything
- no troop actually acts differently.
  - all attack top down,
  - by attack value, not tier.
  - flat refines in particular can mess up the tiering versus attack value
- counter attack is when the wounded in a layer attack troops in range
- walls do little to nothing
- with rally or reinforcement, count each as a separate layer of the same overall even if
  the same tier/type
- Derrick Defies claims debuffs are not cumulative across the rally/reinforcement.
- He says sub cities are an exception to this.
- Mixed marches _in general_ work best against weaker opponents (and oddly barbs)
- for attacking marches
  - relatively small layers below your attacking layer
  - thick top tier ground layer on non-ground marches
- due to range siege attacks
  - low tier siege attacks ground first
  - mid tier attack range and t1-t4 siege first
  - t11 will attack t4 and then high tier first because t4 has to move closer so it is in
    range first, and is the top of the lowest range tier.

Damage = NA(k) \* A/(A+D)

N = number of troops in layer
A = Attack
D = Defense
k = troop modifier

Damage/(Enemy HP) = kills

round down

troop priority

| att/def | Siege | Ranged | Mounted | Ground |
| Siege | 1 | 2 | 4 | 3 |
| Range | 4 | 2 | 1 | 3 |
| Mounted | 3 | 4 | 2 | 1 |
| Ground | 4 | 3 | 2 | 1 |

Priorities are based on _what is in range in the given turn_

troop modifiers

| attacker | defender | modifier |
| -------- | -------- | -------- |
| Siege    | Siege    | 0.5      |
| Siege    | Ground   | 0.3      |
| Siege    | Range    | 0.4      |
| Siege    | Mounted  | 0.347    |
| Ranged   | Siege    | 1.105    |
| Ranged   | Ground   | 0.67     |
| Ranged   | Range    | 1        |
| Ranged   | Mounted  | 1.2      |
| Mounted  | Siege    | 0.93333  |
| Mounted  | Ground   | 1.2      |
| Mounted  | Range    | 0.804    |
| Mounted  | Mounted  | 1        |
| Ground   | Siege    | 1.1      |
| Ground   | Ground   | 1        |
| Ground   | Range    | 1        |
| Ground   | Mounted  | 0.7      |

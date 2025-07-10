% File: buff_patterns.pl
% All DCG patterns for parsing different buff structures

% Subcity Pattern 1: Matrix expansion
% initial attribute with no troop type + bifurcated with troop type attribute + shared conditions
buff_pattern(Buffs) -->
  optional_verb, [the],
  subcity_attribute_list(Attributes1),
  [by], [ValueAtom1],
  [and],
  troop_list(Troops2),
  subcity_attribute_list(Attributes2),
  [by], [ValueAtom2],
  [in], [subordinate], [city],
  merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    % Convert subcity attributes to full subcity form
    maplist(add_subcity_prefix, Attributes1, SubCityAttrs1),
    maplist(add_subcity_prefix, Attributes2, SubCityAttrs2),
    % Create buffs with converted attributes
    expand_matrix([''], SubCityAttrs1, Value1, [UniqueConditions], Buffs1),
    expand_matrix(Troops2, SubCityAttrs2, Value2, [UniqueConditions], Buffs2),
    append(Buffs1, Buffs2, Buffs),
    format('DEBUG: Final buffs: ~w~n', [Buffs])
  }.

% Pattern: Alternate matrix variant with + values
% initial shared condition + bifurcated troop type + bifurcated attribute with individual values
buff_pattern(Buffs) -->
  [ConditionWord],
  { condition([ConditionStr]), atom_string(ConditionWord, ConditionStr) },
  troop_list(Troops),
  attribute_list(Attributes1),
  [+], [ValueAtom1],
  [and], attribute_list(Attributes2),
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, Attributes1, Value1, [ConditionStr], Buffs1),
    expand_matrix(Troops, Attributes2, Value2, [ConditionStr], Buffs2),
    append(Buffs1, Buffs2, Buffs)
  }.

% Pattern 2: Standard matrix expansion
buff_pattern(Buffs) -->
  optional_verb,
  troop_list(Troops),
  attribute_list(Attributes),
  [by], [ValueAtom],
  [when], merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom, Value),
    expand_matrix(Troops, Attributes, Value, [UniqueConditions], Buffs)
  }.

% Pattern 3: Matrix expansion with generic + specific attributes
% initial attribute with no troop type + standard troop type attribute clause + shared conditions
buff_pattern(Buffs) -->
  optional_verb, [the],
  attribute_list(Attributes1),
  [by], [ValueAtom1],
  [and],
  troop_list(Troops2),
  attribute_list(Attributes2),
  [by], [ValueAtom2],
  [when], [general], [is], merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    % Create buffs for both troop/attribute/value combinations
    expand_matrix( [''], Attributes1, Value1, [UniqueConditions], Buffs1),
    expand_matrix(Troops2, Attributes2, Value2, [UniqueConditions], Buffs2),
    append(Buffs1, Buffs2, Buffs)
  }.

% Pattern 4: Matrix expansion (bifurcated object of the verb, but shared conditions)
buff_pattern(Buffs) -->
  optional_verb,
  troop_list(Troops1),
  attribute_list(Attributes1),
  [by], [ValueAtom1], [and],
  troop_list(Troops2),
  attribute_list(Attributes2),
  [by], [ValueAtom2],
  [when], [general], [is], merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    % Create buffs for both troop/attribute/value combinations
    expand_matrix(Troops1, Attributes1, Value1, [UniqueConditions], Buffs1),
    expand_matrix(Troops2, Attributes2, Value2, [UniqueConditions], Buffs2),
    append(Buffs1, Buffs2, Buffs)
  }.

% Pattern 5: Subcity matrix expansion (legacy pattern)
buff_pattern(Buffs) -->
  [increases],[the],attribute_list(Attribute1),[by],
  ValueAtom1,[and],[troops],attribute_list(Attribute2),[by],
  ValueAtom2,[in],[subordinate],[city], merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    % Create buffs for both troop/attribute/value combinations
    expand_matrix( [''], Attribute1, Value1, [UniqueConditions], Buffs1),
    expand_matrix([''], Attribute2, Value2, [UniqueConditions], Buffs2),
    append(Buffs1, Buffs2, Buffs)
  }.

% Pattern 1: Simple one-buff (most general - should be last)
buff_pattern([buff(AttributeAtom, TroopAtom, Value, UniqueConditions)]) -->
  optional_verb,
  troop(TroopAtom),
  attribute(AttributeAtom),
  [by], [ValueAtom],
  [when], optional_subject, optional_verb,
  merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom, Value)
  },
  { format("DEBUG: Matched simple buff: troop=~w attr=~w value=~w cond=~w~n",
    [TroopAtom, AttributeAtom, Value, UniqueConditions])
  }.

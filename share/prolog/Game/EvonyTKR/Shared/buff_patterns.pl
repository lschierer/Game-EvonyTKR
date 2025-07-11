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
    format("DEBUG: Subcity Pattern 1 Final buffs: ~w~n", [Buffs])
  }.



% Debuff Pattern 1: Alternate matrix variant with + values
% initial debuff indicator, initial condition,  single attribute without troop type
buff_pattern(Buffs) -->
  { format("DEBUG: Trying Debuff Pattern 1~n", []) },
  short_condition(Enemy),
  { format("DEBUG: Passed short_condition(~w)~n", [Enemy]) },
  short_condition(CondPhrase),
  { format("DEBUG: Passed short_condition(~w)~n", [CondPhrase]) },
  troop_list(Troops),
  { format("DEBUG: Passed troop_list(~w)~n", [Troops]) },
  attribute_list(Attrs),
  { format("DEBUG: Passed attribute_list(~w)~n", [Attrs]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value])
  },
  {
    list_to_set([Enemy,CondPhrase], CanonicalConds),
    format("DEBUG: CanonicalConds are '~w'~n", [CanonicalConds]),
    expand_matrix(Troops, Attrs, Value, [CanonicalConds], Buffs),
    format("DEBUG: Debuff Pattern 1 matched: Cond='~w' Troops='~w' Attrs='~w'~n", [CanonicalConds, Troops, Attrs])
  }.

% Pattern: 1C1AV1AandAV
% 1C: shared condition
% 1AV: there is an attribute with its own value
% 1AandAV: there is an 'Attribute and Attribute Value" clause
% total: 3 buffs.
buff_pattern(Buffs) -->
  [ConditionWord],
  { condition([ConditionStr]), atom_string(ConditionWord, ConditionStr) },
  troop_list(Troops),
  attribute(Attribute1),
  [+], [ValueAtom1],
  attribute(Attribute2), [and], attribute(Attribute3),
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, [Attribute1], Value1, [ConditionStr], B1),
    expand_matrix(Troops, [Attribute2, Attribute3], Value2, [ConditionStr], B2),
    append(B1, B2, Buffs),
    format("DEBUG: 1C1AV1AandAV alt: attr1 + val1; attr2 and attr3 + val2~n", [])
  }.

% Pattern 1: Alternate matrix variant with + values
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
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: Pattern 1")
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
    expand_matrix(Troops, Attributes, Value, [UniqueConditions], Buffs),
    format("DEBUG: Pattern 2")
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
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: Pattern 3")
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
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: Pattern 4")
  }.



% 1 Buff Pattern 1: initial condition, no troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1 Buff Pattern 1'~n", []) },
  [when], [general], [is],
  consume_condition(CanonCond),
  {
    format("DEBUG: Passed short_condition(~w)~n", [CanonCond])
  },
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [buff(AttributeAtom, '', Value, [CanonCond])],
    format("DEBUG: Matched 1 Buff Pattern 1: attr='~w' value='~w' cond='~w' buff='~w'~n",
           [AttributeAtom, Value, SingleCond, Buffs])
  }.

% 1C1TAV1AV
% 1C = 1 shared condition
% 1TAV = 1 troop attribute value clause
% 1AV = 1 attribute value clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1C1TAV1AV~n", []) },
  [when],
  consume_condition(CanonCond),
  { format("DEBUG: Passed short_condition(~w)~n", [CanonCond]) },
  troop(Troop1),
  { format("DEBUG: Passed troop(~w)~n", [Troop1]) },
  attribute(Attributes1),
  { format("DEBUG: Passed attribute(~w)~n", [Attributes1]) },
  [+], [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  [troops], attribute(Attributes2),
  { format("DEBUG: Passed attribute(~w)~n", [Attributes2]) },
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: Passed ValueAtom2(~w)~n", [Value2])
  },
  {
    extract_value(ValueAtom2, Value2),
    Buff1 = buff(Attributes1, Troop1, Value1, [CanonCond]),
    Buff2 = buff(Attributes2, '',     Value2, [CanonCond]),
    Buffs = [Buff1, Buff2],
    format("DEBUG: 1C1TAV1AV buffs: '~w'~n", [Buffs])
  }.

% 1 Buff Pattern 2: initial condition, with troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1 Buff Pattern 2'~n", []) },
  consume_condition(CanonCond),
  {
    format("DEBUG: Passed short_condition(~w)~n", [CanonCond])
  },
  troop(TroopAtom),
  { format("DEBUG: Passed troop(~w)~n", [TroopAtom]) },
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [buff(AttributeAtom, TroopAtom, Value, [CanonCond])],
    format("DEBUG: Matched 1 Buff Pattern 2: attr='~w' value='~w' cond='~w' buff='~w'~n",
            [AttributeAtom, Value, SingleCond, Buffs])
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

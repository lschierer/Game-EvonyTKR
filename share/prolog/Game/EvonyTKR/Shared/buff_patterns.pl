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
    format("DEBUG: Subcity Pattern 1 matched Final buffs: ~w~n", [Buffs])
  }.

% Pattern: 2C1AV
% 2C: 2 Conditions
% 1AV: 1 [No|Generic] Troop Attribute Value clause
buff_pattern(Buffs) -->
  {format("Trying '2C1AV'~n")},
  condition(Cond1), condition(Cond2),optional_troop(''),
  attribute(Attr1),[+],[ValueAtom1],
  {
    format("DEBUG: matched terms for '2C1AV' Cond1='~w', cond2='~w' attr1='~w' ValueAtom1='~w'~n", [Cond1, Cond2, Attr1, ValueAtom1]),
    extract_value(ValueAtom1, Value1),
    extract_condition_atoms(Cond1, PCond1),
    extract_condition_atoms(Cond2, PCond2),
    list_to_set([PCond1, PCond2], PCond),
    Buffs = [buff(Attr1, '', Value1, PCond)],
    format("DEBUG: matched '2C1AV': buff='~w'~n", [Buffs])
  }.

% Pattern: 1C1TLAV1AandAV
% 1C: shared condition
% 1TLAV: there is one or more troop types then an attribute with its own value
% 1AandAV: there is an 'Attribute and Attribute Value" clause
% total: 3 buffs.
buff_pattern(Buffs) -->
  {format("Trying '1C1TLAV1AandAV'~n")},
  condition(ConditionWord),
  troop_list(Troops),
  attribute(Attribute1),
  [+], [ValueAtom1],
  attribute(Attribute2), [and], attribute(Attribute3),
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, [Attribute1], Value1, [ConditionWord], B1),
    expand_matrix(Troops, [Attribute2, Attribute3], Value2, [ConditionWord], B2),
    append(B1, B2, Buffs),
    format("DEBUG: 1C1TLAV1AandAV matched~n" )
  }.

% 1C1TL1ALVandALV
% 1 shared condition
% 1 shared troop list
% 1 Attribute list Value and Attribute list Value clause
buff_pattern(Buffs) -->
  {format("Trying '1C1TL1ALVandALV'~n)")},
  condition(ConditionWord),
  troop_list(Troops),
  attribute_list(Attributes1),
  [+], [ValueAtom1],
  [and], attribute_list(Attributes2),
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, Attributes1, Value1, [ConditionWord], Buffs1),
    expand_matrix(Troops, Attributes2, Value2, [ConditionWord], Buffs2),
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: '1C1TL1ALVandALV' matched")
  }.

% Pattern 2: Standard matrix expansion
% this appears to need the specificity of [when], [general],
% to prevent merged_conditions() from going haywire
buff_pattern(Buffs) -->
  { format("DEBUG: Trying Pattern 2~n") },
  optional_verb,
  troop_list(Troops),
  { format("DEBUG: Pattern 2 troop_list: '~w'~n", [Troops]) },
  attribute_list(Attributes),
  { format("DEBUG: Pattern 2 Attributes: '~w'~n", [Attributes]) },
  [by], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Pattern 2 Value: '~w'~n", [Value])
  },
  [when], [general], merged_conditions(UniqueConditions),
  { format("DEBUG: Pattern 2 UniqueConditions: '~w'~n", [UniqueConditions]) },
  {
    expand_matrix(Troops, Attributes, Value, [UniqueConditions], Buffs),
    format("DEBUG: Pattern 2 matched Buffs: '~w'~n", [Buffs])
  }.

% Pattern 3: Matrix expansion with generic + specific attributes
% initial attribute with no troop type + standard troop type attribute clause + shared conditions
buff_pattern(Buffs) -->
  {format("Trying 'Pattern 3'~n")},
  optional_verb, [the],
  attribute_list(Attributes1),
  [by], [ValueAtom1],
  [and],
  troop_list(Troops2),
  attribute_list(Attributes2),
  [by], [ValueAtom2],
  optional_when_general_is, merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    % Create buffs for both troop/attribute/value combinations
    expand_matrix( [''], Attributes1, Value1, [UniqueConditions], Buffs1),
    expand_matrix(Troops2, Attributes2, Value2, [UniqueConditions], Buffs2),
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: Pattern 3 matched~n")
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
  optional_when_general_is, merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    % Create buffs for both troop/attribute/value combinations
    expand_matrix(Troops1, Attributes1, Value1, [UniqueConditions], Buffs1),
    expand_matrix(Troops2, Attributes2, Value2, [UniqueConditions], Buffs2),
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: Pattern 4 matched~n")
  }.

% 1C1A1V
% 1C: one condition
% 1A: one attribute
% 1V: one value
% no troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1A1V'~n", []) },
  optional_when_general_is,
  condition(CanonCond),
  {
    format("DEBUG: Passed condition(~w)~n", [CanonCond])
  },
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    extract_condition_atoms(CanonCond, PrintableConditions),
    Buffs = [buff(AttributeAtom, '', Value, PrintableConditions)],
    format("DEBUG: matched '1C1A1V': attr='~w' value='~w' cond='~w' buff='~w'~n",
           [AttributeAtom, Value, SingleCond, Buffs])
  }.

% 1C1ATV1AV
% 1 Shared Condition
% 1 Troop Attribute Value clause
% 1 Attribuve Value (no troop) clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1ATV1AV'~n")  },
  [when],
  condition(Cond1),
  {
    format("DEBUG: Passed condition(~w)~n", [Cond1])
  },
  troop(Troop1),
  { format("DEBUG: Passed troop(~w)~n", [Troop1]) },
  attribute(Attr1), [+],[ValueAtom1],
  { format("DEBUG: Passed attribute(~w)~n", [Attr1]) },
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  optional_troop(''),  { format("DEBUG: found generic troop type~n") },
  attribute(Attr2), [+],[ValueAtom2],
  { format("DEBUG: Passed attribute(~w)~n", [Attr2]) },
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: Passed ValueAtom2(~w)~n", [Value2])
  },
  {
    extract_condition_atoms(Cond1, PCond1),
    Buff1 = buff(Attr1, Troop1, Value1, PCond1),
    Buff2 = buff(Attr2, '', Value2, PCond1),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1C1ATV1AV' matched buffs: '~w'~n", [Buffs])
  }.

% 1C1AV1ATV
% 1 Shared Condition
% 1 Attribuve Value (no troop) clause
% 1 Troop Attribute Value clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1AV1ATV'~n")  },
  [when],
  condition(Cond1),
  {
    extract_condition_atoms(Cond1, PCond1),
    format("DEBUG: Passed condition(~w)~n", [PCond1])
  },
  optional_troop(''),  { format("DEBUG: found generic troop type~n") },
  attribute(Attr1), [+],[ValueAtom1],
  { format("DEBUG: Passed attribute(~w)~n", [Attr1]) },
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  troop(Troop2),
  { format("DEBUG: Passed troop(~w)~n", [Troop2]) },
  attribute(Attr2), [+],[ValueAtom2],
  { format("DEBUG: Passed attribute(~w)~n", [Attr2]) },
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: Passed ValueAtom2(~w)~n", [Value2])
  },
  {
    Buff1 = buff(Attr1, '', Value1, PCond1),
    Buff2 = buff(Attr2, Troop2, Value2, PCond1),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1C1AV1ATV' matched buffs: '~w'~n", [Buffs])
  }.

% 1C1CAV1TAV
% 1 Shared Condition
% 1 Condition Attribute Value (no troop) clause
% 1 Troop Attribuve Value clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1C1CAV1TAV~n")  },
  [when],
  condition(Cond1),
  condition(Cond2),
  attribute(Attr1), [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1)
  },
  troop(Troop2),
  attribute(Attr2), [+],[ValueAtom2],
  {
    extract_value(ValueAtom2, Value2)
  },
  {
    Buff1 = buff(Attr1, '', Value1, [Cond1, Cond2]),
    Buff2 = buff(Attr2, Troop2, Value2, [Cond1]),
    Buffs = [Buff1, Buff2],
    format("DEBUG: 1C1CAV1TAV matched buffs: '~w'~n", [Buffs])
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
    format("DEBUG: 1C1TAV1AV matched buffs: '~w'~n", [Buffs])
  }.

% 1C1TAV2A1V
% 1C = 1 shared condition
% 1TAV = 1 troop attribute value clause
% 2A1V = no troop 2 attribute 1 value matrix clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1C1TAV2A1V~n", []) },
  [when],
  condition(Cond1),
  { format("DEBUG: Passed condition(~w)~n", [Cond1]) },
  troop(Troop1),
  { format("DEBUG: Passed troop(~w)~n", [Troop1]) },
  attribute(Attr1),
  { format("DEBUG: Passed attribute(~w)~n", [Attr1]) },
  [+], [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  optional_troop(''), attribute(Attr2a), [and], attribute(Attr2b),
  { format("DEBUG: Passed attribute(~w)~n", [Attr2a]) },
  { format("DEBUG: Passed attribute(~w)~n", [Attr2b]) },
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: Passed ValueAtom2(~w)~n", [Value2])
  },
  {
    format("DEBUG: matched all for '1C1TAV2A1V'~n"),
    extract_value(ValueAtom2, Value2),
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2a = buff(Attr2a, '',   Value2, [Cond1]),
    Buff2b = buff(Attr2b, '',   Value2, [Cond1]),
    Buffs = [Buff1, Buff2a, Buff2b],
    format("DEBUG: 1C1TAV2A1V matched buffs: '~w'~n", [Buffs])
  }.

% 1C1T1A1V
% 1C: 1 condition
% 1T: 1 Troop
% 1A: 1 attribute
% 1V: 1 value
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1T1A1V'~n", []) },
  condition(CanonCond),
  {
    extract_condition_atoms(CanonCond, PrintableConditions),
    format("DEBUG: Passed condition(~w)~n", [PrintableConditions])
  },
  troop(TroopAtom),
  { format("DEBUG: Passed troop(~w)~n", [TroopAtom]) },
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [buff(AttributeAtom, TroopAtom, Value, PrintableConditions)],
    format("DEBUG: matched '1C1T1A1V': attr='~w' value='~w' cond='~w' buff='~w'~n",
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
  { format("DEBUG: matched simple buff: troop=~w attr=~w value=~w cond=~w~n",
    [TroopAtom, AttributeAtom, Value, UniqueConditions])
  }.

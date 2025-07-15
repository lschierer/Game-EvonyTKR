% File: buff_patterns.pl
% All DCG patterns for parsing different buff structures

% Subcity Pattern: Sub1SaV
% this has all sorts of constants
% because I do not care to add
% this many special cases to the
% global Constants file.
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Sub1SaV'~n")},
  optional_verb, subcity_attribute(Attr1),
  [by], [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  optional_when_general_is, [the],[mayor],
  {
    Buffs = [
      buff(Attr1,'',Value1,[when_city_mayor_for_this_subcity])
    ],
    format("DEBUG: 'Sub1SaV' matched: '~w'~n", [Buffs])
  }.

% Subcity Pattern: Sub1CSACV
% 1 Condition Attribute Condition Value
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Sub1CSACV'~n")},
  optional_when_general_is,condition(Cond1),
  subcity_attribute(Attr1), condition(Cond2),
  optional_plus,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1]),
    list_to_set([Cond1, Cond2], PCond),
    Buffs = [buff(Attr1,'',Value1,PCond)],
    format("DEBUG: 'Sub1CSACV' matched: '~w'~n", [Buffs])
  }.


% Subcity Pattern 1: Matrix expansion
% initial attribute with no troop type + bifurcated with troop type attribute + shared conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Subcity Pattern 1'~n")},
  optional_verb, [the],
  subcity_attribute(Attribute1),
  {format("DEBUG:subcity_attribute: '~w'~n", [Attribute1])},
  [by], [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  },
  [and],
  troop(Troop2),
  {format("DEBUG: troop: '~w'~n", [Troop2])},
  subcity_attribute(Attribute2),
  {format("DEBUG:subcity_attribute: '~w'~n", [Attribute2])},
  [by], [ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  },
  condition(Cond1),optional_when_general_is,condition(Cond2),
  {
    list_to_set([Cond1, Cond2], PCond),
    format("DEBUG: PCond: '~w'~n", PCond),
    % Create buffs with converted attributes
    Buff1 = buff( Attribute1,'', Value1, PCond),
    Buff2 = buff(Attribute2,Troop2, Value2, PCond),
    Buffs = [Buff1,Buff2],
    format("DEBUG: Subcity Pattern 1 matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1CACV
% 1 Condition Attribute Condition Value
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CACV'~n")},
  optional_when_general_is,condition(Cond1),
  attribute(Attr1),
  {format("DEBUG: attribute: '~w'~n", [Attr1])},
  condition(Cond2),
  optional_plus,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1]),
    list_to_set([Cond1, Cond2], PCond),
    Buffs = [buff(Attr1,'',Value1,PCond)],
    format("DEBUG: '1CACV' matched: '~w'~n", [Buffs])
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
    list_to_set([Cond1, Cond2], PCond),
    Buffs = [buff(Attr1, '', Value1, PCond)],
    format("DEBUG: matched '2C1AV': buff='~w'~n", [Buffs])
  }.

% 1C2T1AV2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T1AV2A1V'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_plus, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), [and], attribute(Attr3),
  optional_plus, [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1,Troop1,Value1,[Cond1]),
      buff(Attr1,Troop2,Value1,[Cond1]),
      buff(Attr2,Troop1,Value2,[Cond1]),
      buff(Attr2,Troop2,Value2,[Cond1]),
      buff(Attr3,Troop1,Value2,[Cond1]),
      buff(Attr3,Troop2,Value2,[Cond1])
    ],
    format("DEBUG: '1C2T1AV2A1V' matched: Buffs: '~w'~n", [Buffs])
  }.

% 1C2T1AVand1C1T2A1VC
% 1C: one condition
% 2 Troop Types
% 1AV: one attribute value clause
% 1 Condition
% 1 Troop, 2 Attributes
% 1 Value, 1 additional condition
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C2T1AVand1C1T2A1VC'~n", []) },
  optional_verb, condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), [by], [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  [and], condition(Cond2),
  troop(Troop3), attribute(Attr2), [and],
  attribute(Attr3), [by], [ValueAtom2],
  {extract_value(ValueAtom2, Value2)},
  optional_when_general_is, condition(Cond3),
  {
    list_to_set([Cond1,Cond3], PCondA),
    list_to_set([Cond2,Cond3], PCondB),
    Buffs = [
      buff(Attr1, Troop1, Value1, PCondA),
      buff(Attr1, Troop2, Value1, PCondA),
      buff(Attr2, Troop3, Value2, PCondB),
      buff(Attr3, Troop3, Value2, PCondB)
    ],
    format("DEBUG: matched '1C2T1AVand1C1T2A1VC': '~w'~n", [Buffs])
  }.

% Pattern: 1TAVand1Aand2T1AV2C
% 1 Troop Attribute Value clause
% 1 Generic Attribute and 2 Troop Types
% 1 Attribute Value, the Value applies
% to the Generic Attribute as well,
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("Trying '1TAVand1Aand2T1AV2C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  [by], [ValueAtom1],
  { extract_value(ValueAtom1, Value1) },
  [and], optional_verb, attribute(Attr2), [and],
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr3), [by], [ValueAtom2],
  { extract_value(ValueAtom2, Value2) },
  optional_when_general_is, condition(Cond1),
  condition(Cond2),
  {
    list_to_set([Cond1,Cond2], PCond),
    Buff1 = buff(Attr1, Troop1, Value1, PCond),
    Buff2 = buff(Attr2, '', Value2, PCond),
    Buff3 = buff(Attr3, Troop2, Value2, PCond),
    Buff4 = buff(Attr3, Troop3, Value2, PCond),
    Buffs = [Buff1, Buff2, Buff3, Buff4],
    format("DEBUG: '1TAVand1Aand2T1AV2C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1T2A1Vand2T1AV2C
% 1 Troop
% 2 Attributes with a single value
% and 2 Troops
% 1 Attribute Value clause
% 2 shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1Vand2T1AV2C'~n")},
  optional_verb, troop(Troop1),
  attribute(Attr1), [and], attribute(Attr2),
  [by], [ValueAtom1],
  {extract_value(ValueAtom1,Value1)}, [and],
  optional_verb, troop(Troop2), [and],
  troop(Troop3), attribute(Attr3), [by],
  [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  optional_when_general_is,
  condition(Cond1),condition(Cond2),
  {
    list_to_set([Cond1,Cond2],PCond),
    Buff1 = buff(Attr1, Troop1, Value1, PCond),
    Buff2 = buff(Attr2, Troop1, Value1, PCond),
    Buff3 = buff(Attr3, Troop2, Value2, PCond),
    Buff4 = buff(Attr3, Troop3, Value2, PCond),
    Buffs = [Buff1, Buff2, Buff3, Buff4],
    format("DEBUG: '1T2A1Vand2T1AV2C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1T2A1Vand1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1Vand1AV1C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  [and], attribute(Attr2), [by], [ValueAtom1],
  {extract_value(ValueAtom1,Value1)}, [and],
  attribute(Attr3), [by], [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  optional_when_general_is, condition(Cond1),
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1]),
      buff(Attr2, Troop1, Value1, [Cond1]),
      buff(Attr3, '', Value2, [Cond1])
    ],
    format("DEBUG: '1T2A1Vand1AV1C' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C2TAV1CT2A1V
% 1 NOT SHARED Condition
% 2 Troops with
% 1 Attribute Value clause
% 1 NOT SHARED Condition
% 1 Troop with
% 2 Attributes that have a
% 1 Value
buff_pattern(Buffs) -->
  condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_plus,
  [ValueAtom1], {extract_value(ValueAtom1,Value1)},
  condition(Cond2),troop(Troop3),
  attribute(Attr2), [and], attribute(Attr3),
  optional_plus,[ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attr1, Troop2, Value1, [Cond1]),
    Buff3 = buff(Attr2, Troop3, Value2, [Cond2]),
    Buff4 = buff(Attr3, Troop3, Value2, [Cond2]),
    Buffs = [Buff1, Buff2, Buff3, Buff4],
    format("DEBUG: '1C2TAV1CT2A1V' matched: '~w'~n", [Buffs])
  }.


% Pattern: 1TAV1AV
% 1 Troop Attribute Value clause
% 1 Attribute Value clause
buff_pattern(Buffs) -->
  {format("Trying '1TAV1AV'~n")},
  troop(Troop1), attribute(Attr1),
  optional_plus, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), optional_plus,
  [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buff1 = buff(Attr1, Troop1, Value1, ['leading the army']),
    Buff2 = buff(Attr2, '', Value2, ['leading the army']),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1TAV1AV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1C2TAV1AV
% 1 shared condition
% 2Troop Attribute Value
% 1 No Troup Attribute Value
buff_pattern(Buffs) -->
  {format("Trying '1C2TAV1AV'~n")},
  optional_when,condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_plus,[ValueAtom1],
  { extract_value(ValueAtom1, Value1)},
  attribute(Attr2),optional_plus,[ValueAtom2],
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1,Troop1,Value1,[Cond1]),
    Buff2 = buff(Attr1,Troop2,Value1,[Cond1]),
    Buff3 = buff(Attr2,'',Value2,['leading the army']),
    Buffs = [Buff1, Buff2, Buff3],
    format("DEBUG: '1C2TAV1AV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C1TAVandAV
% 1 shared condition
% 1 Troop Attribute Value and Attribute Value
buff_pattern(Buffs) -->
  {format("Trying '1C1TAVandAV'~n")},
  optional_when,condition(Cond1),
  troop(Troop1),attribute(Attr1),
  optional_plus,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  },
  [and],attribute(Attr2),optional_plus,[ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  },
  {
    list_to_set([Cond1], PCond),
    Buff1 = buff(Attr1,Troop1,Value1,PCond),
    Buff2 = buff(Attr2,Troop1,Value2,PCond),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1C1TAVandAV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1TAVandAV
% 1 shared condition
% 1 Troop Attribute Value and Attribute Value
buff_pattern(Buffs) -->
  {format("Trying '1TAVandAV'~n")},
  optional_when,troop(Troop1),attribute(Attr1),
  optional_plus,[ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  [and],attribute(Attr2),optional_plus,[ValueAtom2],
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1,Troop1,Value1,[]),
    Buff2 = buff(Attr2,Troop1,Value2,[]),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1TAVandAV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C1TAVandTAV1C
% 1 shared condition (no commas or periods)
% 1TAV: 1 Troop Attribute Value clause
% and junction
% TA1V: 1 Troop Attribute Value clause
% 1 shared condition (no commas or periods)
buff_pattern(Buffs) -->
  {format("Trying '1C1TAVandTAV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), attribute(Attr1),
  [by], [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: troop1: '~w' attribute1: '~w' ValueAtom1: '~w'~n", [Troop1, Attr1, Value1])
  },
  [and], troop(Troop2), attribute(Attr2),
  [by], [ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: troop2: '~w' attribute2: '~w' ValueAtom2: '~w'~n", [Troop2, Attr2, Value2])
  },
  optional_when_general_is, condition(Cond2),
  {
    list_to_set([Cond1,Cond2], PCond),
    format("DEBUG: PCond: '~w'~n", [PCond]),
    Buff1 = buff(Attr1, Troop1, Value1, PCond),
    format("buff1: '~w'~n", [Buff1])
  },
  {
    Buff2 = buff(Attr2, Troop2, Value2, PCond),
    format("buff1: '~w'~n", [Buff2])
  },
  {
    Buffs = [Buff1 , Buff2],
    format("DEBUG: '1C1TAVandTAV1C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1TAVand2TA1V1C
% 1TAV: 1 Troop Attribute Value clause
% and junction with optional verb
% 2TA1V: 2 Troop 2 Attribute 1 Value clause
% 1 shared condition (no commas or periods)
buff_pattern(Buffs) -->
  {format("Trying '1TAVand2TA1V1C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  [by], [ValueAtom1],[and], optional_verb,
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr2), [and], attribute(Attr3),
  [by], [ValueAtom2], optional_when_general_is,
  condition(Cond1),
  { extract_value(ValueAtom1, Value1) },
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attr2, Troop2, Value2, [Cond1]),
    Buff3 = buff(Attr2, Troop3, Value2, [Cond1]),
    Buff4 = buff(Attr3, Troop2, Value2, [Cond1]),
    Buff5 = buff(Attr3, Troop3, Value2, [Cond1]),
    Buffs = [Buff1 , Buff2, Buff3, Buff4, Buff5],
    format("DEBUG: '1TAVand2TA1V1C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1CTAVand1C2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAVand1C2T1AV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), attribute(Attr1),
  [by], [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  [and], condition(Cond2),
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr2), [by], [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  optional_when_general_is,condition(Cond3),
  {
    list_to_set([Cond1,Cond3], PCondA),
    list_to_set([Cond2,Cond3], PCondB),
    Buffs = [
      buff(Attr1, Troop1, Value1, PCondA),
      buff(Attr2, Troop2, Value2, PCondB),
      buff(Attr2, Troop3, Value2, PCondB)
    ],
    format("DEBUG: '1CTAVand1C2T1AV1C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1C2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T1AV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), [by], [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  optional_when_general_is,condition(Cond2),
  {
    list_to_set([Cond1,Cond2], PCondA),
    Buffs = [
      buff(Attr1, Troop1, Value1, PCondA),
      buff(Attr1, Troop2, Value1, PCondA)
    ],
    format("DEBUG: '1C2T1AV1C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1C1TLAV1AandAV
% 1C: shared condition
% 1TLAV: there is one or more troop types then an attribute with its own value
% 1AandAV: there is an 'Attribute and Attribute Value" clause
% total: 3 buffs.
buff_pattern(Buffs) -->
  {format("Trying '1C1TLAV1AandAV'~n")},
  condition(Cond1),troop_list(Troops),
  attribute(Attribute1), [+], [ValueAtom1],
  attribute(Attribute2), [and], attribute(Attribute3),
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, [Attribute1], Value1, [Cond1], B1),
    expand_matrix(Troops, [Attribute2, Attribute3], Value2, [Cond1], B2),
    append(B1, B2, Buffs),
    format("DEBUG: 1C1TLAV1AandAV matched~n" )
  }.

% Pattern: 1CTAV2A1V
% 1 Troop Attribute Value clause
% 1 Attribute Value clause
buff_pattern(Buffs) -->
  {format("Trying '1CTAV2A1V'~n")},
  condition(Cond1),
  troop(Troop1), attribute(Attr1),
  optional_plus, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), [and], attribute(Attr3),
  optional_plus, [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attr2, Troop1, Value2, []),
    Buff3 = buff(Attr3, Troop1, Value2, []),
    Buffs = [Buff1, Buff2, Buff3],
    format("DEBUG: '1CTAV2A1V' matched: '~w'~n", [Buffs])
  }.


% 2T1AVand2T1AV1C
% 2 Troops
% 1 Attribute Value clause
% and 2 Troops
% 1 Attribute Value clause
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AVand2T1AV1C'~n")},
  optional_verb, troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), [by], [ValueAtom1], {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  }, [and], optional_verb, troop(Troop3), [and], troop(Troop4),
  attribute(Attr2), [by], [ValueAtom2], {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  }, optional_when_general_is, condition(Cond1),
   condition(Cond2),
  {
    list_to_set([Cond1, Cond2], PCond),
    Buff1 = buff(Attr1, Troop1, Value1, PCond),
    Buff2 = buff(Attr1, Troop2, Value1, PCond),
    Buff3 = buff(Attr2, Troop3, Value2, PCond),
    Buff4 = buff(Attr2, Troop4, Value2, PCond),
    Buffs = [Buff1, Buff2, Buff3, Buff4],
    format("DEBUG: '2T1AVand2T1AV1C' matched: '~w'~n", [Buffs])
  }.

% 2T1AVand1AV1C
% 2 Troops
% 1 Attribute Value clause
% and 1 Attribute Value clause
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AVand1AV2C'~n")},
  optional_verb, troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), [by], [ValueAtom1], {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  }, [and], attribute(Attr2), [by], [ValueAtom2], {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  }, optional_when_general_is, condition(Cond1),
  condition(Cond2),
  {
    list_to_set([Cond1, Cond2], PCond),
    Buff1 = buff(Attr1, Troop1, Value1, PCond),
    Buff2 = buff(Attr1, Troop2, Value1, PCond),
    Buff3 = buff(Attr2, Troop1, Value2, PCond),
    Buff4 = buff(Attr2, Troop2, Value2, PCond),
    Buffs = [Buff1, Buff2, Buff3, Buff4 ],
    format("DEBUG: '2T1AVand1AV2C' matched: '~w'~n", [Buffs])
  }.

% 2T2xAV
% 2 Troops
% 2x  Attribute Value clauses
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T2xAV'~n")},
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_plus, [ValueAtom1], {
    extract_value(ValueAtom1, Value1) },
  [and], attribute(Attr2), optional_plus,
  [ValueAtom2], { extract_value(ValueAtom2, Value2) },
 {
    Buff1 = buff(Attr1, Troop1, Value1, []),
    Buff2 = buff(Attr1, Troop2, Value1, []),
    Buff3 = buff(Attr2, Troop1, Value2, []),
    Buff4 = buff(Attr2, Troop2, Value2, []),
    Buffs = [Buff1, Buff2, Buff3, Buff4 ],
    format("DEBUG: '2T2xAV' matched: '~w'~n", [Buffs])
  }.

% 2T2xAV1andAV2C
% 2 Troops
% 2x Attribute Value clause
% 1 and Attribute Value clauses
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T2xAV1andAV2C'~n")},
  optional_verb, troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), [by], [ValueAtom1], {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  }, attribute(Attr2), [by], [ValueAtom2], {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  }, [and], attribute(Attr3), [by], [ValueAtom3], {
    extract_value(ValueAtom3, Value3),
    format("DEBUG: ValueAtom3: '~w'~n", [Value3])
  }, optional_when_general_is, condition(Cond1),
  condition(Cond2),
  {
    list_to_set([Cond1, Cond2], PCond),
    Buff1 = buff(Attr1, Troop1, Value1, PCond),
    Buff2 = buff(Attr1, Troop2, Value1, PCond),
    Buff3 = buff(Attr2, Troop1, Value2, PCond),
    Buff4 = buff(Attr2, Troop2, Value2, PCond),
    Buff5 = buff(Attr3, Troop1, Value3, PCond),
    Buff6 = buff(Attr3, Troop2, Value3, PCond),
    Buffs = [Buff1, Buff2, Buff3, Buff4 , Buff5, Buff6],
    format("DEBUG: '2T2xAV1andAV2C' matched: '~w'~n", [Buffs])
  }.

% 1TAVand2T1AV2C
% 1 Troop Attribute Value clause
% 2 Troop 1Attribute Value clause
% 2 shared conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAVand2T1AV2C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_by, [ValueAtom1],{
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  },
  [and],optional_verb,troop(Troop2),[and],
  troop(Troop3),attribute(Attr2),optional_by,
  [ValueAtom2],{
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  },optional_when_general_is,condition(Cond1),
  condition(Cond2),
  {
    list_to_set([Cond1,Cond2], PCond),
    Buff1 = buff(Attr1,Troop1,Value1,PCond),
    Buff2 = buff(Attr2,Troop2,Value2,PCond),
    Buff3 = buff(Attr2,Troop3,Value2,PCond),
    Buffs = [Buff1,Buff2,Buff3],
    format("DEBUG: '1TAVand2T1AV2C' matched: '~w'~n", [Buffs])
  }.

% 1C1TL1ALVandALV
% 1 shared condition
% 1 shared troop list
% 1 Attribute list Value and Attribute list Value clause
buff_pattern(Buffs) -->
  {format("Trying '1C1TL1ALVandALV'~n")},
  condition(Cond1),
  troop_list(Troops),
  attribute_list(Attributes1),
  [+], [ValueAtom1],
  [and], attribute_list(Attributes2),
  [+], [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, Attributes1, Value1, [Cond1], Buffs1),
    expand_matrix(Troops, Attributes2, Value2, [Cond1], Buffs2),
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

% 1C2T2xAV
% no troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C2T2xAV'~n", []) },
  condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_plus, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)}, [and],
  attribute(Attr2), optional_plus, [ValueAtom2],
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1]),
      buff(Attr1, Troop2, Value1, [Cond1]),
      buff(Attr2, Troop1, Value2, [Cond1]),
      buff(Attr2, Troop2, Value2, [Cond1])
    ],
    format("DEBUG: matched '1C2T2xAV': '~w'~n", [Buffs])
  }.

% 1C2T1A1V
% 1C: one condition
% 2 Troop Types
% 1A: one attribute
% 1V: one value
% no troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C2T1A1V'~n", []) },
  condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  {
    format("DEBUG: Troop1: '~w'~n", [Troop1]),
    format("DEBUG: Troop1: '~w'~n", [Troop2])
  },
  attribute(Attr1),
  { format("DEBUG: Attr1: '~w'~n", [Attr1]) },
  optional_plus, [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [
      buff(Attr1, Troop1, Value, [Cond1]),
      buff(Attr1, Troop2, Value, [Cond1])
    ],
    format("DEBUG: matched '1C2T1A1V': '~w'~n", [Buffs])
  }.


% 1C1A1V
% 1C: one condition
% 1A: one attribute
% 1V: one value
% no troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1A1V'~n", []) },
  optional_when_general_is,
  condition(Cond1),
  {
    format("DEBUG: Passed condition(~w)~n", [CanonCond])
  },
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [buff(AttributeAtom, '', Value, [Cond1])],
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
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attr2, '', Value2, [Cond1]),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1C1ATV1AV' matched buffs: '~w'~n", [Buffs])
  }.

% 1C1AV1ATV
% 1 Shared Condition
% 1 Attribuve Value (no troop) clause
% 1 Troop Attribute Value clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1AV1ATV'~n")  },
  [when], condition(Cond1), optional_troop(''),
  { format("DEBUG: found generic troop type~n") },
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
    Buff1 = buff(Attr1, '', Value1, [Cond1]),
    Buff2 = buff(Attr2, Troop2, Value2, [Cond1]),
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

% Pattern: 1TAV2T2A1V
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1TAV2T2A1V'~n")  },
  troop(Troop1),attribute(Attr1),
  optional_plus, [ValueAtom1],
  { extract_value(ValueAtom1, Value1) },
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr2), [and], attribute(Attr3),
  optional_plus, [ValueAtom2],
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1, Troop1, Value1, []),
    Buff2 = buff(Attr2, Troop2, Value2, []),
    Buff3 = buff(Attr3, Troop2, Value2, []),
    Buff4 = buff(Attr2, Troop3, Value2, []),
    Buff5 = buff(Attr3, Troop3, Value2, []),
    Buffs = [Buff1, Buff2, Buff3, Buff4, Buff5],
    format("DEBUG: '1TAV2T2A1V' matched: buffs: '~w'~n", [Buffs])
  }.

% 1C1TAV1AV
% 1C = 1 shared condition
% 1TAV = 1 troop attribute value clause
% 1AV = 1 attribute value clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1C1TAV1AV~n", []) },
  [when],
  condition(Cond1), troop(Troop1),
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
    Buff1 = buff(Attributes1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attributes2, '',     Value2, [Cond1]),
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
  condition(Cond1), troop(TroopAtom),
  { format("DEBUG: Passed troop(~w)~n", [TroopAtom]) },
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  [+], [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [buff(AttributeAtom, TroopAtom, Value, [Cond1])],
    format("DEBUG: matched '1C1T1A1V': attr='~w' value='~w' cond='~w' buff='~w'~n",
            [AttributeAtom, Value, Cond1, Buffs])
  }.

% Pattern: 1CAV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CAV'~n")},
  optional_when,condition(Cond1),
  troop(Troop1), attribute(Attr1),
  [ValueAtom1],
  { extract_value(ValueAtom1, Value1) },
  {
    Buffs = [buff( Attr1, Troop1, Value1, [Cond1])],
    format("DEBUG: '1CAV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1VA2C
% 1 Value Attribute 2 Conditions
% No Troops
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1VA2C'~n")},
  optional_when, optional_verb, optional_subject,
  [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1)
  },
  attribute(Attr1),
  {format("DEBUG: Attr1: '~w'~n", [Attr1])},
  condition(Cond1), optional_when_general_is,
  condition(Cond2),
  {
    list_to_set([Cond1,Cond2], PCond),
    Buffs = [buff( Attr1, '', Value1, PCond )],
    format("DEBUG: '1VA2C' matched: '~w'~n", [Buffs])
  }.

% Pattern 1C1TAV
% 1 shared condition
% 1 troop attribute value clause
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C1TAV'~n")},
  optional_when, condition(Cond1),
  troop(Troop1),
  {format("DEBUG: troop: '~w'~n", [Troop1])},
  attribute(Attr1),
  {format("DEBUG: attribute: '~w'~n", [Attr1])},
  optional_plus,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  },
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1])
    ],
    format("DEBUG: '1C1TAV' matched: '~w'~n",
    [Buffs])
  }.


% Pattern 1: Simple one-buff (most general - should be last)
buff_pattern([buff(AttributeAtom, TroopAtom, Value, UniqueConditions)]) -->
  optional_when, optional_verb,
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

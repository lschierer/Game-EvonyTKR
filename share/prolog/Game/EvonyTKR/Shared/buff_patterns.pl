% File: buff_patterns.pl
% All DCG patterns for parsing different buff structures

% Subcity Pattern: Sub1SaV
%
% because I do not care to add
% this many special cases to the
% global Constants file.
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Sub1SaV'~n")},
  optional_when, optional_when_general_is,
  optional_verb, subcity_attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  optional_when_general_is, condition(Cond1),
  {
    Buffs = [
      buff(Attr1,'',Value1,[Cond1])
    ],
    format("DEBUG: 'Sub1SaV' matched: '~w'~n", [Buffs])
  }.

% Subcity Pattern: 1SubAV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1SubAV2C'~n")},
  optional_when, optional_when_general_is,
  optional_verb, subcity_attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  condition(Cond1),
  optional_when_general_is, condition(Cond2),
  {
    Buffs = [
      buff(Attr1,'',Value1,[Cond1,Cond2])
    ],
    format("DEBUG: '1SubAV2C' matched: '~w'~n", [Buffs])
  }.

% Subcity Pattern: Sub1CSACV
% 1 Condition Attribute Condition Value
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Sub1CSACV'~n")},
  optional_when_general_is,condition(Cond1),
  subcity_attribute(Attr1), condition(Cond2),
  optional_value_adj,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1]),
    list_to_set([Cond1, Cond2], PCond),
    Buffs = [buff(Attr1,'',Value1,PCond)],
    format("DEBUG: 'Sub1CSACV' matched: '~w'~n", [Buffs])
  }.

% Subcity Pattern: Sub1SACV
% 1  SubAttribute Condition Value
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Sub1SACV'~n")},
  optional_when, optional_when_general_is,
  subcity_attribute(Attr1), condition(Cond1),
  optional_value_adj,[ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  {
    Buffs = [
      buff(Attr1,'',Value1,[Cond1])
    ],
    format("DEBUG: 'Sub1SACV' matched: '~w'~n", [Buffs])
  }.


% Subcity Pattern 1: Matrix expansion
% initial attribute with no troop type + bifurcated with troop type attribute + shared conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying 'Subcity Pattern 1'~n")},
  optional_verb, [the],
  subcity_attribute(Attribute1),
  {format("DEBUG:subcity_attribute: '~w'~n", [Attribute1])},
  optional_value_adj, [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  },
  [and],
  troop(Troop2),
  {format("DEBUG: troop: '~w'~n", [Troop2])},
  subcity_attribute(Attribute2),
  {format("DEBUG:subcity_attribute: '~w'~n", [Attribute2])},
  optional_value_adj, [ValueAtom2],
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

% Pattern: 1TSubAV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1TSubAV2C'~n")},
  optional_verb, troop(Troop1), subcity_attribute(Attr1),
  optional_value_adj, [ValueAtom1], condition(Cond1),
  optional_when_general_is, condition(Cond2),
  {extract_value(ValueAtom1, Value1)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond2])
    ]
  },
  {format("DEBUG: '1TSubAV2C' matched: '~w'~n",[Buffs])}.

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
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  [and], condition(Cond2),
  troop(Troop3), attribute(Attr2), [and],
  attribute(Attr3), optional_value_adj, [ValueAtom2],
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

% Pattern: 1C2T1AV1Aand1C2T1AV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1C2T1AV1Aand1C2T1AV'~n")},
  condition(Cond1), troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  attribute(Attr2a), [and], condition(Cond2),
  troop(Troop2a), [and], troop(Troop2b), attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a, Value1, [Cond1]),
      buff(Attr1, Troop1b, Value1, [Cond1]),
      buff(Attr2a, '', Value2,  []),
      buff(Attr2b, Troop2a, Value2, [Cond2]),
      buff(Attr2b, Troop2b, Value2, [Cond2])
    ]
  },
  {format("DEBUG: '1C2T1AV1Aand1C2T1AV' matched: '~w'~n", [Buffs])}.

% Pattern: 1TAVand2TA1V1C
% 1TAV: 1 Troop Attribute Value clause
% and junction with optional verb
% 2TA1V: 2 Troop 2 Attribute 1 Value clause
% 1 shared condition (no commas or periods)
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAVand2TA1V1C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],[and], optional_verb,
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom2], optional_when_general_is,
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

% Pattern: 1T3A1Vand1AVC
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T3A1Vand1AVC'~n")},
  optional_verb, troop(Troop1),
  attribute(Attr1a), attribute(Attr1b), [and], attribute(Attr1c),
  optional_value_adj, [ValueAtom1], [and], attribute(Attr2),
  optional_value_adj, [ValueAtom2], optional_when_general_is,
  condition(Cond2),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1a, Troop1, Value1, []),
      buff(Attr1b, Troop1, Value1, []),
      buff(Attr1c, Troop1, Value1, []),
      buff(Attr2, '', Value2, [])
    ]
  },
  {format("DEBUG: '1T3A1Vand1AVC' matched: '~w'~n", [Buffs])}.

% Pattern: 1CTAVand1C2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAVand1C2T1AV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  [and], condition(Cond2),
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
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

% Pattern: 1CTAVand1CT2A1VC
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAVand1CT2A1VC'~n")},
  optional_verb, condition(Cond1), troop(Troop1),
  attribute(Attr1), optional_value_adj,
  [ValueAtom1], [and],
  condition(Cond2), troop(Troop2),
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(CondS),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1,   Troop1, Value1, [Cond1,CondS]),
      buff(Attr2a,  Troop2, Value2, [Cond2,CondS]),
      buff(Attr2b,  Troop2, Value2, [Cond2,CondS])
    ],
    format("DEBUG: '1CTAVand1CT2A1VC' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1CTAVand1CTAV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAVand1CTAV1C'~n")},
  optional_verb, condition(Cond1), troop(Troop1),
  attribute(Attr1), optional_value_adj,
  [ValueAtom1], [and], optional_verb,
  condition(Cond2), troop(Troop2),
  attribute(Attr2), optional_value_adj,
  [ValueAtom2],
  optional_when_general_is, condition(Cond3),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond3]),
      buff(Attr2, Troop2, Value2, [Cond2,Cond3])
    ],
    format("DEBUG: '1CTAVand1CTAV1C' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1CTAVand1T1T2A1V1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAVand1T1T2A1V1C'~n")},
  optional_verb, condition(Cond1), troop(Troop1),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  [and], optional_verb, optional_your, troop(Troop2),
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond2),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond2]),
      buff(Attr2a, Troop2, Value2, [Cond2]),
      buff(Attr2b, Troop2, Value2, [Cond2])
    ]
  },
  {format("DEBUG: '1CTAVand1T1T2A1V1C' matched: Buffs: '~w'~n", [Buffs])}.


% Pattern: 1CTAVand1TAV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAVand1TAV1C'~n")},
  optional_verb, optional_the, condition(Cond1),
  troop(Troop1), attribute(Attr1), optional_value_adj,
  [ValueAtom1], {extract_value(ValueAtom1, Value1)},
  [and], troop(Troop2), attribute(Attr2), optional_value_adj,
  [ValueAtom2], {extract_value(ValueAtom2, Value2)},
  optional_when_general_is, condition(Cond2),
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond2]),
      buff(Attr2, Troop2, Value2, [Cond1,Cond2])
    ],
    format("DEBUG: '1CTAVand1TAV1C' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C1TAV1AandAV
% 1C: shared condition
% 1TAV: there is one or more troop types then an attribute with its own value
% 1AandAV: there is an 'Attribute and Attribute Value" clause
% total: 3 buffs.
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C1TAV1AandAV'~n")},
  condition(Cond1),troop_list(Troops),
  attribute(Attribute1), optional_value_adj, [ValueAtom1],
  attribute(Attribute2), [and], attribute(Attribute3),
  optional_value_adj, [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, [Attribute1], Value1, [Cond1], B1),
    expand_matrix(Troops, [Attribute2, Attribute3], Value2, [Cond1], B2),
    append(B1, B2, Buffs),
    format("DEBUG: 1C1TAV1AandAV matched~n" )
  }.

% Pattern: 1C2T1AVand1C2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1C2T1AVand1C2T1AV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  [and], condition(Cond2),
  troop(Troop3), [and], troop(Troop4),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom2, Value2)},
  optional_when_general_is, condition(Cond3),
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1, Cond3]),
      buff(Attr1, Troop2, Value1, [Cond1, Cond3]),
      buff(Attr2, Troop3, Value2, [Cond2, Cond3]),
      buff(Attr2, Troop4, Value2, [Cond2, Cond3])
    ],
    format("DEBUG: '1C2T1AVand1C2T1AV1C' matched: '~w'~n'", [Buffs])
  }.

% Pattern: 1C2T1AVand1TAV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1C2T1AVand1TAV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  [and], optional_verb, optional_your, troop(Troop2),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(CondS1),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a,  Value1, [Cond1, CondS1]),
      buff(Attr1, Troop1b,  Value1, [Cond1, CondS1]),
      buff(Attr2, Troop2,   Value2, [CondS1])
    ],
    format("DEBUG: '1C2T1AVand1TAV1C' matched: '~w'~n'", [Buffs])
  }.

% Pattern: 1C2T1AVand1CTAV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1C2T1AVand1CTAV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  [and], condition(Cond2), troop(Troop2),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(CondS1),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a,  Value1, [Cond1, CondS1]),
      buff(Attr1, Troop1b,  Value1, [Cond1, CondS1]),
      buff(Attr2, Troop2,   Value2, [Cond2, CondS1])
    ],
    format("DEBUG: '1C2T1AVand1CTAV1C' matched: '~w'~n'", [Buffs])
  }.

% Pattern: 1C2T1AVand2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1C2T1AVand2T1AV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1], [and],
  optional_verb, optional_your,
  troop(Troop2a), [and], troop(Troop2b), attribute(Attr2),
  optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond2),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1,  Troop1a, Value1, [Cond1]),
      buff(Attr1,  Troop1b, Value1, [Cond1]),
      buff(Attr2,  Troop2a, Value2, [Cond2]),
      buff(Attr2,  Troop2b, Value2, [Cond2])
    ]
  },
  {format("DEBUG: '1C2T1AVand2T1AV1C' matched: '~w'~n", [Buffs])}.


% Pattern: 1TAV2T1AVand1AV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1TAV2T1AVand1AV2C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  troop(Troop2a), [and], troop(Troop2b),
  attribute(Attr2a), optional_value_adj, [ValueAtom2a], [and],
  attribute(Attr2b), optional_value_adj, [ValueAtom2b],
  optional_when_general_is, condition(Cond2a), condition(Cond2b),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2a,Value2a)},
  {extract_value(ValueAtom2b,Value2b)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2a, Troop2a, Value2a, [Cond2a,Cond2b]),
      buff(Attr2a, Troop2b, Value2a, [Cond2a,Cond2b]),
      buff(Attr2b, Troop2a, Value2b, [Cond2a,Cond2b]),
      buff(Attr2b, Troop2b, Value2b, [Cond2a,Cond2b])
    ]
  },
  {format("DEBUG: '1TAV2T1AVand1AV2C' matched: '~w'~n", [Buffs])}.

% Pattern: 1CTAVAVand1C2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1CTAVAVand1C2T1AV1C'~n")},
  optional_verb, condition(Cond1), troop(Troop1),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  [and], condition(Cond2), troop(Troop2), [and],
  troop(Troop3), attribute(Attr3), optional_value_adj,
  [ValueAtom3], optional_when_general_is, condition(Cond3),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {extract_value(ValueAtom3,Value3)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond3]),
      buff(Attr2, Troop1, Value2, [Cond1,Cond3]),
      buff(Attr3, Troop2, Value3, [Cond2,Cond3]),
      buff(Attr3, Troop3, Value3, [Cond2,Cond3])
    ]
  },
  {format("DEBUG: '1CTAVAVand1C2T1AV1C' matched: '~w'~n'", [Buffs])}.

% Pattern: 1TAVand1Aand2T1AV2C
% 1 Troop Attribute Value clause
% 1 Generic Attribute and 2 Troop Types
% 1 Attribute Value, the Value applies
% to the Generic Attribute as well,
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAVand1Aand2T1AV2C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  { extract_value(ValueAtom1, Value1) },
  [and], optional_verb, attribute(Attr2), [and],
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr3), optional_value_adj, [ValueAtom2],
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

% Pattern: 1C1CTAVand1C2T1AV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C1C1TAVand1C2T1AV'~n")},
  optional_when_general_is, condition(CondS1),
  condition(Cond1), troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1], [and],
  condition(Cond2), troop(Troop2a), [and], troop(Troop2b),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1,   Troop1,   Value1,   [CondS1, Cond1]),
      buff(Attr2,   Troop2a,   Value2,   [CondS1, Cond2]),
      buff(Attr2,   Troop2b,   Value2,   [CondS1, Cond2])
    ]
  },
  {format("DEBUG: '1C1C1TAVand1C2T1AV' matched: '~w'~n",[Buffs])}.

% Pattern: 1C1CTAVand1C2T1AV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C1CTAVand1C2T1AV'~n")},
  optional_when_general_is, condition(CondS1),
  condition(Cond1), troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1], [and],
  condition(Cond2), troop(Troop2a), [and], troop(Troop2b),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1,   Troop1,   Value1,   [CondS1, Cond1]),
      buff(Attr2,   Troop2a,   Value2,   [CondS1, Cond2]),
      buff(Attr2,   Troop2b,   Value2,   [CondS1, Cond2])
    ]
  },
  {format("DEBUG: '1C1CTAVand1C2T1AV' matched: '~w'~n",[Buffs])}.

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
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)}, [and],
  optional_verb, troop(Troop2), [and],
  troop(Troop3), attribute(Attr3), optional_value_adj,
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

% Pattern: 1T2A1Vand2T1AV1C
% 1 Troop
% 2 Attributes with a single value
% and 2 Troops
% 1 Attribute Value clause
% 2 shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1Vand2T1AV1C'~n")},
  optional_verb, troop(Troop1),
  attribute(Attr1a), [and], attribute(Attr1b),
  optional_value_adj, [ValueAtom1], [and],
  optional_verb, troop(Troop2a), [and],
  troop(Troop2b), attribute(Attr2),
  optional_value_adj, [ValueAtom2],
  optional_when_general_is,
  condition(Cond1),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1a,  Troop1,   Value1, [Cond1]),
      buff(Attr1b,  Troop1,   Value1, [Cond1]),
      buff(Attr2,   Troop2a,  Value2, [Cond1]),
      buff(Attr2,   Troop2b,  Value2, [Cond1])
    ],
    format("DEBUG: '1T2A1Vand2T1AV1C' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1T2A1Vand1TAV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1Vand1TAV2C'~n")},
  optional_verb, [the], troop(Troop1),
  attribute(Attr1), [and], attribute(Attr2),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  [and], troop(Troop2), attribute(Attr3),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom2, Value2)},
  optional_when_general_is,
  condition(Cond1),condition(Cond2),
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond2]),
      buff(Attr2, Troop1, Value1, [Cond1,Cond2]),
      buff(Attr3, Troop2, Value2, [Cond1,Cond2])
    ],
    format("DEBUG: '1T2A1Vand1TAV2C' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1T2A1Vand1AV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1Vand1AV2C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  [and], attribute(Attr2), optional_value_adj, [ValueAtom1],
  [and], attribute(Attr3), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond1), condition(Cond2),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1,Cond2]),
      buff(Attr2, Troop1, Value1, [Cond1,Cond2]),
      buff(Attr3, Troop1, Value2, [Cond1,Cond2])
    ],
    format("DEBUG: '1T2A1Vand1AV2C' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1T2A1Vand1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1Vand1AV1C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  [and], attribute(Attr2), optional_value_adj, [ValueAtom1],
  [and], attribute(Attr3), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond1),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1]),
      buff(Attr2, Troop1, Value1, [Cond1]),
      buff(Attr3, '', Value2, [Cond1])
    ],
    format("DEBUG: '1T2A1Vand1AV1C' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C1TAVandAV
% 1 shared condition
% 1 Troop Attribute Value and Attribute Value
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C1TAVandAV'~n")},
  optional_when,condition(Cond1),
  troop(Troop1),attribute(Attr1),
  optional_value_adj,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  },
  [and],attribute(Attr2),optional_value_adj,[ValueAtom2],
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
  {format("DEBUG: Trying '1TAVandAV'~n")},
  optional_when,troop(Troop1),attribute(Attr1),
  optional_value_adj,[ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  [and],attribute(Attr2),optional_value_adj,[ValueAtom2],
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
  {format("DEBUG: Trying '1C1TAVandTAV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: troop1: '~w' attribute1: '~w' ValueAtom1: '~w'~n", [Troop1, Attr1, Value1])
  },
  [and], troop(Troop2), attribute(Attr2),
  optional_value_adj, [ValueAtom2],
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

% Pattern: 1TAVand1CTAV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAVand1CTAV'~n")},
  optional_verb, optional_your, troop(Troop1),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  [and], optional_verb, condition(Cond2a), troop(Troop2),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond2b),
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2, Troop2, Value2, [Cond2a, Cond2b])
    ]
  },
  {format("DEBUG: '1TAVand1CTAV' matched: '~w'~n", [Buffs])}.

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
  attribute(Attr1), optional_value_adj,
  [ValueAtom1], {extract_value(ValueAtom1,Value1)},
  condition(Cond2),troop(Troop3),
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj,[ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attr1, Troop2, Value1, [Cond1]),
    Buff3 = buff(Attr2, Troop3, Value2, [Cond2]),
    Buff4 = buff(Attr3, Troop3, Value2, [Cond2]),
    Buffs = [Buff1, Buff2, Buff3, Buff4],
    format("DEBUG: '1C2TAV1CT2A1V' matched: '~w'~n", [Buffs])
  }.

% Pattern: 2CT2xAV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2CT2xAV'~n")},
  optional_when_general_is, condition(Cond1),
  condition(Cond2), troop(Troop1), [and],
  troop(Troop2), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  [and], attribute(Attr2), optional_value_adj,
  [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1, Cond2]),
      buff(Attr2, Troop1, Value2, [Cond1, Cond2]),
      buff(Attr1, Troop2, Value1, [Cond1, Cond2]),
      buff(Attr2, Troop2, Value2, [Cond1, Cond2])
    ],
    format("DEBUG: '2CT2xAV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 2C1T2xAV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2C1T2xAV'~n")},
  optional_when_general_is, condition(Cond1),
  condition(Cond2), troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  [and], attribute(Attr2), optional_value_adj,
  [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1, Cond2]),
      buff(Attr2, Troop1, Value2, [Cond1, Cond2])
    ],
    format("DEBUG: '2C1T2xAV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1C2T1AV1T2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T1AV1T2A1V'~n")},
  optional_when, condition(CondS1),
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  troop(Troop2),
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a, Value1, [CondS1]),
      buff(Attr1, Troop1b, Value1, [CondS1]),
      buff(Attr2a, Troop2, Value2, [CondS1]),
      buff(Attr2b, Troop2, Value2, [CondS1])
    ]
  },
  {format("DEBUG: '1C2T1AV1T2A1V' matched: '~w'~n",[Buffs])}.

% Pattern: 1C2T1AV2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T1AV2A1V'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom2],
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

% Pattern: 1TAVand1T2A1V1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAVand1T2A1V1C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1], [and],
  optional_verb, troop(Troop2),
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  optional_when_general_is,condition(Cond2),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2a, Troop2, Value2, [Cond2]),
      buff(Attr2b, Troop2, Value2, [Cond2])
    ]
  },
  {format("DEBUG: '1TAVand1T2A1V1C' matched: '~w'~n", [Buffs])}.

% Pattern: 1TAVand1TAV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAVand1TAV2C'~n")},
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1], [and],
  optional_verb, troop(Troop2), attribute(Attr2),
  optional_value_adj, [ValueAtom2], optional_when_general_is,
  condition(Cond2a), condition(Cond2b),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2, Troop2, Value2, [Cond2a, Cond2b])
    ]
  },
  {format("DEBUG: '1TAVand1TAV2C' matched: '~w'~n", [Buffs])}.

% Pattern: 2C1TAV1C2TA1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2C1TAV1C2TA1V'~n")},
  optional_when_general_is, condition(CondS1),
  condition(Cond1), troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  condition(Cond2), troop(Troop2a), [and], troop(Troop2b),
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1,   Troop1,   Value1, [CondS1,Cond1]),
      buff(Attr2a,  Troop2a,  Value2, [CondS1,Cond2]),
      buff(Attr2a,  Troop2b,  Value2, [CondS1,Cond2]),
      buff(Attr2b,  Troop2a,  Value2, [CondS1,Cond2]),
      buff(Attr2b,  Troop2b,  Value2, [CondS1,Cond2])
    ]
  },
  {format("DEBUG: '2C1TAV1C2TA1V' matched: '~w'~n",[Buffs])}.

% Pattern: 1C1T2A1V1AV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C1T2A1V1AV'~n")},
  optional_when, condition(CondS1), troop(Troop1),
  attribute(Attr1a), [and], attribute(Attr1b),
  optional_value_adj, [ValueAtom1],
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1a, Troop1, Value1, [CondS1]),
      buff(Attr1b, Troop1, Value1, [CondS1]),
      buff(Attr2, '', Value2, [])
    ]
  },
  {format("DEBUG: '1C1T2A1V1AV' matched: '~w'~n",[Buffs])}.

% 2T1AV2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AV2A1V'~n")},
  optional_verb, troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1,Troop1,Value1,[]),
      buff(Attr1,Troop2,Value1,[]),
      buff(Attr2,Troop1,Value2,[]),
      buff(Attr2,Troop2,Value2,[]),
      buff(Attr3,Troop1,Value2,[]),
      buff(Attr3,Troop2,Value2,[])
    ],
    format("DEBUG: '2T1AV2A1V' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1CACV
% 1 Condition Attribute Condition Value
% Generic Troops,
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CACV'~n")},
  optional_when_general_is, condition(Cond1),
  attribute(Attr1),optional_when,
  condition(Cond2),
  optional_value_adj,[ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    Buffs = [buff(Attr1,'',Value1,[Cond1, Cond2])],
    format("DEBUG: '1CACV' matched: '~w'~n", [Buffs])
  }.

% Pattern 1T2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T2A1V'~n")},
  troop(Troop1), attribute(Attr1), [and],
  attribute(Attr2), optional_value_adj,
  [ValueAtom1],{extract_value(ValueAtom1,Value1)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2, Troop1, Value1, [])
    ],
    format("DEBUG: '1T2A1V' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1TAV1AV
% 1 Troop Attribute Value clause
% 1 Attribute Value clause
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAV1AV'~n")},
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), optional_value_adj,
  [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buff1 = buff(Attr1, Troop1, Value1, []),
    Buff2 = buff(Attr2, '', Value2, []),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1TAV1AV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1C2TAV1AV
% 1 shared condition
% 2Troop Attribute Value
% 1 No Troup Attribute Value
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2TAV1AV'~n")},
  optional_when,condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj,[ValueAtom1],
  { extract_value(ValueAtom1, Value1)},
  attribute(Attr2),optional_value_adj,[ValueAtom2],
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1,Troop1,Value1,[Cond1]),
    Buff2 = buff(Attr1,Troop2,Value1,[Cond1]),
    Buff3 = buff(Attr2,'',Value2,[]),
    Buffs = [Buff1, Buff2, Buff3],
    format("DEBUG: '1C2TAV1AV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C2T1AV1C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T1AV1C'~n")},
  optional_verb, condition(Cond1),
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
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

% Pattern: 2T1AV1TAV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AV1TAV'~n")},
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  troop(Troop2), attribute(Attr2),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a, Value1, []),
      buff(Attr1, Troop1b, Value1, []),
      buff(Attr2, Troop2, Value2, [])
    ],
    format("DEBUG: '2T1AV1TAV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 2T1AV1AV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AV1AV'~n")},
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  attribute(Attr2),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom1,Value1)},
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a, Value1, []),
      buff(Attr1, Troop1b, Value1, []),
      buff(Attr2, '', Value2, [])
    ],
    format("DEBUG: '2T1AV1AV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1CTAV2A1V
% 1 Troop Attribute Value clause
% 1 Attribute Value clause
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAV2A1V'~n")},
  condition(Cond1),
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom2],
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
  attribute(Attr1), optional_value_adj, [ValueAtom1], {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  }, [and], optional_verb, troop(Troop3), [and], troop(Troop4),
  attribute(Attr2), optional_value_adj, [ValueAtom2], {
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

% 2T1AVand1TAV2C
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AVand1TAV2C'~n")},
  optional_verb, troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  [and], optional_verb, troop(Troop2),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond2a), condition(Cond2b),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a,  Value1, []),
      buff(Attr1, Troop1b,  Value1, [Cond2a, Cond2b]),
      buff(Attr2, Troop2,   Value2, [Cond2a, Cond2b])
    ],
    format("DEBUG: '2T1AVand1TAV2C' matched: '~w'~n", [Buffs])
  }.

% 2T1AVand1TAVC
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AVand1TAVC'~n")},
  optional_verb, troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  [and], optional_verb, troop(Troop2),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(Cond2),
  {extract_value(ValueAtom1, Value1)},
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1a,  Value1, []),
      buff(Attr1, Troop1b,  Value1, [Cond2]),
      buff(Attr2, Troop2,   Value2, [Cond2])
    ],
    format("DEBUG: '2T1AVand1TAVC' matched: '~w'~n", [Buffs])
  }.

% 2T1AVand1AV1C
% 2 Troops
% 1 Attribute Value clause
% and 1 Attribute Value clause
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AVand1AV2C'~n")},
  optional_verb, troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1], {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  }, [and], attribute(Attr2), optional_value_adj, [ValueAtom2], {
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

% 2T2xAV1andAV2C
% 2 Troops
% 2x Attribute Value clause
% 1 and Attribute Value clauses
% 2 Shared Conditions
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T2xAV1andAV2C'~n")},
  optional_verb, troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1], {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: ValueAtom1: '~w'~n", [Value1])
  }, attribute(Attr2), optional_value_adj, [ValueAtom2], {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: ValueAtom2: '~w'~n", [Value2])
  }, [and], attribute(Attr3), optional_value_adj, [ValueAtom3], {
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
  {format("DEBUG: Trying '1C1TL1ALVandALV'~n")},
  condition(Cond1),
  troop_list(Troops),
  attribute_list(Attributes1),
  optional_value_adj, [ValueAtom1],
  [and], attribute_list(Attributes2),
  optional_value_adj, [ValueAtom2],
  {
    extract_value(ValueAtom1, Value1),
    extract_value(ValueAtom2, Value2),
    expand_matrix(Troops, Attributes1, Value1, [Cond1], Buffs1),
    expand_matrix(Troops, Attributes2, Value2, [Cond1], Buffs2),
    append(Buffs1, Buffs2, Buffs),
    format("DEBUG: '1C1TL1ALVandALV' matched")
  }.


% Pattern: 1CTAV1CT2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CTAV1CT2A1V'~n")},
  condition(Cond1), troop(Troop1),
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  condition(Cond2), troop(Troop2),
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom2, Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1]),
      buff(Attr2, Troop2, Value2, [Cond2]),
      buff(Attr3, Troop2, Value2, [Cond2])
    ],
    format("DEBUG: '1CTAV1CT2A1V' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C2T3A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T2A1V'~n")},
  condition(Cond1),troop(Troop1),[and],
  troop(Troop2), attribute(Attr1),
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1]),
      buff(Attr1, Troop2, Value1, [Cond1]),
      buff(Attr2, Troop1, Value1, [Cond1]),
      buff(Attr2, Troop2, Value1, [Cond1]),
      buff(Attr3, Troop1, Value1, [Cond1]),
      buff(Attr3, Troop2, Value1, [Cond1])
    ],
    format("DEBUG: '1C2T2A1V' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C2T2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1C2T2A1V'~n")},
  condition(Cond1),troop(Troop1),[and],
  troop(Troop2), attribute(Attr1), [and],
  attribute(Attr2), optional_value_adj,
  [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, [Cond1]),
      buff(Attr1, Troop2, Value1, [Cond1]),
      buff(Attr2, Troop1, Value1, [Cond1]),
      buff(Attr2, Troop2, Value1, [Cond1])
    ],
    format("DEBUG: '1C2T2A1V' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 2T2xAV
% 2 Troops
% 2x  Attribute Value clauses
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T2xAV'~n")},
  troop(Troop1), [and], troop(Troop2),
  attribute(Attr1), optional_value_adj, [ValueAtom1], {
    extract_value(ValueAtom1, Value1) },
  [and], attribute(Attr2), optional_value_adj,
  [ValueAtom2], { extract_value(ValueAtom2, Value2) },
 {
    Buff1 = buff(Attr1, Troop1, Value1, []),
    Buff2 = buff(Attr1, Troop2, Value1, []),
    Buff3 = buff(Attr2, Troop1, Value2, []),
    Buff4 = buff(Attr2, Troop2, Value2, []),
    Buffs = [Buff1, Buff2, Buff3, Buff4 ],
    format("DEBUG: '2T2xAV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 2C1AV
% 2C: 2 Conditions
% 1AV: 1 [No|Generic] Troop Attribute Value clause
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2C1AV'~n")},
  condition(Cond1), condition(Cond2),troop(_Tx),
  attribute(Attr1),optional_value_adj,[ValueAtom1],
  {
    format("DEBUG: matched terms for '2C1AV' Cond1='~w', cond2='~w' attr1='~w' ValueAtom1='~w'~n", [Cond1, Cond2, Attr1, ValueAtom1]),
    extract_value(ValueAtom1, Value1),
    list_to_set([Cond1, Cond2], PCond),
    Buffs = [buff(Attr1, '', Value1, PCond)],
    format("DEBUG: matched '2C1AV': buff='~w'~n", [Buffs])
  }.

% Pattern: 2TA1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2TA1V'~n")},
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1a), [and], attribute(Attr1b),
  optional_value_adj,[ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  {
    Buffs = [
      buff(Attr1a, Troop1a, Value1, []),
      buff(Attr1a, Troop1b, Value1, []),
      buff(Attr1b, Troop1a, Value1, []),
      buff(Attr1b, Troop1b, Value1, [])
    ]
  },
  {format("DEBUG: '2TA1V' matched: '~w'~n", [Buffs])}.

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
  optional_value_adj, [ValueAtom],
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
  {format("DEBUG: Trying 'Pattern 3'~n")},
  optional_verb, [the],
  attribute_list(Attributes1),
  optional_value_adj, [ValueAtom1],
  [and],
  troop_list(Troops2),
  attribute_list(Attributes2),
  optional_value_adj, [ValueAtom2],
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
  optional_value_adj, [ValueAtom1], [and],
  troop_list(Troops2),
  attribute_list(Attributes2),
  optional_value_adj, [ValueAtom2],
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
  attribute(Attr1), optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)}, [and],
  attribute(Attr2), optional_value_adj, [ValueAtom2],
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
  optional_value_adj, [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [
      buff(Attr1, Troop1, Value, [Cond1]),
      buff(Attr1, Troop2, Value, [Cond1])
    ],
    format("DEBUG: matched '1C2T1A1V': '~w'~n", [Buffs])
  }.

% Pattern: 1CT2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1CT2A1V'~n")},
  condition(Cond1), troop(Troop1),
  attribute(Attr1a), [and], attribute(Attr1b),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1, Value1)},
  {
    Buffs = [
      buff(Attr1a, Troop1, Value1, [Cond1]),
      buff(Attr1b, Troop1, Value1, [Cond1])
    ]
  },
  {format("DEBUG: '1CT2A1V' matched: '~w'~n", [Buffs])}.

% Pattern: 1C1ATV1AV
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
  attribute(Attr1), optional_value_adj,[ValueAtom1],
  { format("DEBUG: Passed attribute(~w)~n", [Attr1]) },
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  troop(_Tx),  { format("DEBUG: found generic troop type~n") },
  attribute(Attr2), optional_value_adj,[ValueAtom2],
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
  [when], condition(Cond1), troop(_Tx),
  { format("DEBUG: found generic troop type~n") },
  attribute(Attr1), optional_value_adj,[ValueAtom1],
  { format("DEBUG: Passed attribute(~w)~n", [Attr1]) },
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  troop(Troop2),
  { format("DEBUG: Passed troop(~w)~n", [Troop2]) },
  attribute(Attr2), optional_value_adj,[ValueAtom2],
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
  attribute(Attr2), optional_value_adj,[ValueAtom2],
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
  optional_value_adj, [ValueAtom1],
  { extract_value(ValueAtom1, Value1) },
  troop(Troop2), [and], troop(Troop3),
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom2],
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

% Pattern: 1TAV2T1AV
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1TAV2T1AV'~n")  },
  troop(Troop1),attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  troop(Troop2a), [and], troop(Troop2b),
  attribute(Attr2), optional_value_adj, [ValueAtom2],
  { extract_value(ValueAtom1, Value1) },
  { extract_value(ValueAtom2, Value2) },
  {
    Buffs= [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2, Troop2a, Value2, []),
      buff(Attr2, Troop2b, Value2, [])
    ],
    format("DEBUG: '1TAV2T1AV' matched: '~w'~n", [Buffs])
  }.

% Pattern: 1TAV1T2A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying: '1TAV1T2A1V'~n")},
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  troop(Troop2), attribute(Attr2a), [and],
  attribute(Attr2b), optional_value_adj,
  [ValueAtom2], {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2a, Troop2, Value2, []),
      buff(Attr2b, Troop2, Value2, [])
    ]
  },
  {format("DEBUG: '1TAV1T2A1V' matched: Buffs: '~w'~n", [Buffs])}.

% 1C1TAV1AV
% 1C = 1 shared condition
% 1TAV = 1 troop attribute value clause
% 1AV = 1 attribute value clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1TAV1AV'~n") },
  [when],
  condition(Cond1), troop(Troop1),
  { format("DEBUG: Passed troop(~w)~n", [Troop1]) },
  attribute(Attributes1),
  { format("DEBUG: Passed attribute(~w)~n", [Attributes1]) },
  optional_value_adj, [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  [troops], attribute(Attributes2),
  { format("DEBUG: Passed attribute(~w)~n", [Attributes2]) },
  optional_value_adj, [ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: Passed ValueAtom2(~w)~n", [Value2])
  },
  {
    extract_value(ValueAtom2, Value2),
    Buff1 = buff(Attributes1, Troop1, Value1, [Cond1]),
    Buff2 = buff(Attributes2, '',     Value2, [Cond1]),
    Buffs = [Buff1, Buff2],
    format("DEBUG: '1C1TAV1AV' matched: buffs: '~w'~n", [Buffs])
  }.

% 1C1TAV2A1V
% 1C = 1 shared condition
% 1TAV = 1 troop attribute value clause
% 2A1V = no troop 2 attribute 1 value matrix clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1C1TAV2A1V~n") },
  [when],
  condition(Cond1),
  { format("DEBUG: Passed condition(~w)~n", [Cond1]) },
  troop(Troop1),
  { format("DEBUG: Passed troop(~w)~n", [Troop1]) },
  attribute(Attr1),
  { format("DEBUG: Passed attribute(~w)~n", [Attr1]) },
  optional_value_adj, [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    format("DEBUG: Passed ValueAtom1(~w)~n", [Value1])
  },
  troop(_Tx), attribute(Attr2a), [and], attribute(Attr2b),
  { format("DEBUG: Passed attribute(~w)~n", [Attr2a]) },
  { format("DEBUG: Passed attribute(~w)~n", [Attr2b]) },
  optional_value_adj, [ValueAtom2],
  {
    extract_value(ValueAtom2, Value2),
    format("DEBUG: Passed ValueAtom2(~w)~n", [Value2])
  },
  {
    extract_value(ValueAtom2, Value2),
    Buff1 = buff(Attr1, Troop1, Value1, [Cond1]),
    Buff2a = buff(Attr2a, '',   Value2, [Cond1]),
    Buff2b = buff(Attr2b, '',   Value2, [Cond1]),
    Buffs = [Buff1, Buff2a, Buff2b],
    format("DEBUG: 1C1TAV2A1V matched buffs: '~w'~n", [Buffs])
  }.

% 1TAV2A1V1C
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1TAV2A1V1C~n", []) },
  optional_verb, troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  optional_when_general_is, condition(CondS),
  { extract_value(ValueAtom1, Value1) },
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1, Troop1, Value1, [CondS]),
    Buff2a = buff(Attr2a, Troop1,   Value2, [CondS]),
    Buff2b = buff(Attr2b, Troop1,   Value2, [CondS]),
    Buffs = [Buff1, Buff2a, Buff2b],
    format("DEBUG: 1TAV2A1V1C matched buffs: '~w'~n", [Buffs])
  }.


% 1TAV2A1V
% 1TAV = 1 troop attribute value clause
% 2A1V = Implied first troop 2 attribute 1 value matrix clause
buff_pattern(Buffs) -->
  { format("DEBUG: Trying 1TAV2A1V~n", []) },
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  { extract_value(ValueAtom1, Value1) },
  attribute(Attr2a), [and], attribute(Attr2b),
  optional_value_adj, [ValueAtom2],
  { extract_value(ValueAtom2, Value2) },
  {
    Buff1 = buff(Attr1, Troop1, Value1, []),
    Buff2a = buff(Attr2a, Troop1,   Value2, []),
    Buff2b = buff(Attr2b, Troop1,   Value2, []),
    Buffs = [Buff1, Buff2a, Buff2b],
    format("DEBUG: 1TAV2A1V matched buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1AV1TAV
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1AV1TAV'~n")},
  attribute(Attr1), optional_value_adj,
  [ValueAtom1],{extract_value(ValueAtom1,Value1)},
  troop(Troop2), attribute(Attr2),
  optional_value_adj, [ValueAtom2],
  {extract_value(ValueAtom2,Value2)},
  {
    Buffs = [
      buff(Attr1, '', Value1,[]),
      buff(Attr2, Troop2, Value2, [])
    ],
    format("DEBUG: '1AV1TAV' matched: Buffs: '~w'~n", [Buffs])
  }.

% Pattern: 1C1A1V
% 1C: one condition
% 1A: one attribute
% 1V: one value
% no troops
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1A1V'~n", []) },
  optional_when_general_is,
  condition(Cond1),
  attribute(AttributeAtom),
  { format("DEBUG: Passed attribute(~w)~n", [AttributeAtom]) },
  optional_value_adj, [ValueAtom],
  {
    extract_value(ValueAtom, Value),
    format("DEBUG: Passed Value: '~w'~n", [Value]),
    Buffs = [buff(AttributeAtom, '', Value, [Cond1])],
    format("DEBUG: matched '1C1A1V': Buffs: '~w'~n", [Buffs])
  }.

% 1C1TAV1C
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1TAV1C'~n", []) },
  optional_when, optional_verb,
  condition(Cond1a), troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  optional_when_general_is, condition(Cond1b),
  {
    extract_value(ValueAtom1, Value1),
    Buffs = [buff(Attr1, Troop1, Value1, [Cond1a,Cond1b])],
    format("DEBUG: matched '1C1TAV1C': '~w'~n", [Buffs])
  }.

% 1C1TAV
% 1C: 1 condition
% 1T: 1 Troop
% 1A: 1 attribute
% 1V: 1 value
buff_pattern(Buffs) -->
  { format("DEBUG: Trying '1C1TAV'~n", []) },
  optional_when_general_is, optional_when,
  condition(Cond1),
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {
    extract_value(ValueAtom1, Value1),
    Buffs = [buff(Attr1, Troop1, Value1, [Cond1])],
    format("DEBUG: matched '1C1TAV': '~w'~n", [Buffs])
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



% Pattern 1: Simple one-buff (most general - should be last)
buff_pattern([buff(AttributeAtom, TroopAtom, Value, UniqueConditions)]) -->
  optional_when, optional_verb,
  troop(TroopAtom),
  attribute(AttributeAtom),
  optional_value_adj, [ValueAtom],
  [when], optional_subject, optional_verb,
  merged_conditions(UniqueConditions),
  {
    extract_value(ValueAtom, Value)
  },
  { format("DEBUG: matched simple buff: troop=~w attr=~w value=~w cond=~w~n",
    [TroopAtom, AttributeAtom, Value, UniqueConditions])
  }.

% Pattern: 1T3A1V
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1T3A1V'~n")},
  troop(Troop1), attribute(Attr1),
  attribute(Attr2), [and], attribute(Attr3),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  {
    Buffs = [
      buff(Attr1, Troop1, Value1, []),
      buff(Attr2, Troop1, Value1, []),
      buff(Attr3, Troop1, Value1, [])
    ]
  },
  {format("DEBUG: '1T3A1V' matched: '~w'~n", [Buffs])}.


% Pattern 2T1AV
% Only attribute and value present
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '2T1AV'~n")},
  troop(Troop1a), [and], troop(Troop1b),
  attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  { Buffs = [
      buff(Attr1, Troop1a, Value1,[]),
      buff(Attr1, Troop1b, Value1,[])
    ],
    format("DEBUG: '2T1AV' matched: '~w'~n", [Buffs])
  }.

% Pattern 1TAV
% Only attribute and value present
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1TAV'~n")},
  troop(Troop1), attribute(Attr1),
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  { Buffs = [ buff(Attr1, Troop1, Value1,[]) ],
    format("DEBUG: '1TAV' matched: '~w'~n", [Buffs])
  }.

% Pattern 1ACV
% Only attribute and value present
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1ACV'~n")},
  attribute(Attr1), condition(Cond1),
  optional_verb,
  optional_value_adj, [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  {
    Buffs = [
      buff(Attr1, '', Value1,[Cond1])
    ],
    format("DEBUG: '1ACV' matched: '~w'~n", [Buffs])
  }.


% Pattern 1AV
% Only attribute and value present
buff_pattern(Buffs) -->
  {format("DEBUG: Trying '1AV'~n")},
  attribute(Attr1), optional_value_adj,
  [ValueAtom1],
  {extract_value(ValueAtom1,Value1)},
  {
    Buffs = [
      buff(Attr1, '', Value1,[])
    ],
    format("DEBUG: '1AV' matched: '~w'~n", [Buffs])
  }.

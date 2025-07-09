:- include('EvonyBuffDictionary').

buff(buff(Attribute, Troop, Value, Condition)) -->
    [increases],
    troop(TroopWords),
    attribute(AttributeWords),
    [by],
    [ValueAtom],
    condition(ConditionWords),
    { atomic_list_concat(AttributeWords, ' ', Attribute),
      atomic_list_concat(TroopWords, ' ', Troop),
      atomic_list_concat(ConditionWords, ' ', Condition),
      atom_number(ValueAtom, Value)
    }.

troop(Words) --> { troop(Words) }.
attribute(Words) --> { attribute(Words) }.
condition(Words) --> [when], { condition(Words) }.

buffs([B|Bs]) --> buff(B), buffs(Bs).
buffs([]) --> [].

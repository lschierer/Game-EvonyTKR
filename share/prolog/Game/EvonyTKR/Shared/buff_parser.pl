% File: buff_parser.pl

:- include('EvonyBuffDictionary').
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- style_check(-singleton).

% Top-level DCG to extract a flat list of buffs from a tokenized sentence
sentence_buffs(All) -->
    buff(Buffs),
    sentence_buffs(Rest),
    { append(Buffs, Rest, All) }.
sentence_buffs([]) --> [].

% Entry point for a single buff pattern
buff(B) --> buff_pattern(B).

% Pattern 1: Simple one-buff
buff_pattern(Buffs) -->
  optional_verb,
  troop(TroopAtom),
  attribute(AttributeAtom),
  [by], [ValueAtom],
  [when], optional_subject, optional_verb,
  condition_phrase(ConditionSets),
  { format("DEBUG: Flattened condition sets: ~w~n", [ConditionSets]) },
  {
    extract_value(ValueAtom, Value),
    maplist({TroopAtom, AttributeAtom, Value}/[Conds, B]>>(
      B = buff(AttributeAtom, TroopAtom, Value, Conds)
    ), ConditionSets, Buffs)
  },
  { format("DEBUG: Matched simple buff: troop=~w attr=~w value=~w cond=~w~n",
            [TroopAtom, AttributeAtom, Value, ConditionSets]) }.

% Pattern 2: Matrix expansion
buff_pattern(Buffs) -->
  optional_verb,
  troop_list(Troops),
  attribute_list(Attributes),
  [by], [ValueAtom],
  [when], condition_phrase(Condition),
  { extract_value(ValueAtom, Value),
    expand_matrix(Troops, Attributes, Value, Condition, Buffs) }.

% Pattern 3: Another matrix variant
buff_pattern(Buffs) -->
  optional_verb,
  troop_list(Troops),
  attribute_list(Attributes),
  [by], [ValueAtom],
  [when], condition_phrase(ConditionList),
  { extract_value(ValueAtom, Value),
    expand_matrix(Troops, Attributes, Value, ConditionList, Buffs) }.


% Helper for + value extraction
extract_value_plus(ValueAtom, Value) :-
    atom_number(ValueAtom, Value).

% DCG wrapper for parsing conditions inside buff
condition_phrase(ConditionSets) -->
    remainder(Rest),
    {
        parse_complex_condition(Rest, ConditionSets)
    }.

% flatten each condition list into separate buffs
flatten_condition_sets([], []).

flatten_condition_sets([CondList | RestSets], Buffs) :-
    expand_matrix(Troops, Attributes, Value, CondList, ThisBuffs),
    flatten_condition_sets(RestSets, OtherBuffs),
    append(ThisBuffs, OtherBuffs, Buffs).

% Parse troop list: "ground troops and mounted troops"
troop_list([T1, T2]) -->
    troop(T1), [and], troop(T2).
troop_list([T]) -->
    troop(T).


% Parse attribute list: "defense and hp"
attribute_list([A1, A2]) -->
    attribute(A1), [and], attribute(A2).
attribute_list([A]) -->
    attribute(A).

% Base case
normalize_condition_tokens([], [[]]).

% Match a condition and recurse
normalize_condition_tokens(Input, Result) :-
    match_any_condition(Input, Match, Remaining, Canonical),
    normalize_condition_tokens(Remaining, RestVariants),
    findall([Canonical | Rest], member(Rest, RestVariants), Result).

% Handle disjunction: split on "or"
normalize_condition_tokens(Input, Result) :-
    append(Left, [or | Right], Input),
    normalize_condition_tokens(Left, LeftVariants),
    normalize_condition_tokens(Right, RightVariants),
    append(LeftVariants, RightVariants, Result).

% Skip unmatchable token
normalize_condition_tokens([_ | T], Result) :-
    normalize_condition_tokens(T, Result).

% Match either a canonical or synonym condition prefix
% Match mapped synonyms
match_any_condition(Input, Match, Remaining, Canonical) :-
    condition_syn(Match, Canonical),
    append(Match, Remaining, Input),
    format("DEBUG: matched synonym: ~w -> ~w~n", [Match, Canonical]).

% Match canonical phrases directly
match_any_condition(Input, Match, Remaining, Canonical) :-
    condition(Match),
    append(Match, Remaining, Input),
    atomics_to_string(Match, ' ', Canonical),
    format("DEBUG: matched canonical: ~w -> ~w~n", [Match, Canonical]).

parse_complex_condition(Tokens, ConditionSets) :-
    format('DEBUG: Raw condition tokens: ~w~n', [Tokens]),
    normalize_condition_tokens(Tokens, ConditionSets),
    forall(
        member(Cond, ConditionSets),
        format('DEBUG: Normalized Canonicals: ~w~n', [Cond])
    ).

% Expand the matrix: create all troop×attribute combinations
expand_matrix(Troops, Attrs, Value, ConditionSets, Buffs) :-
    findall(buff(Attr, Troop, Value, Conds),
      (
        member(Troop, Troops),
        member(Attr, Attrs),
        member(Conds, ConditionSets)
      ),
      Buffs).


% Helper to extract numeric value from percentage
extract_value(ValueAtom, Value) :-
    atom_string(ValueAtom, ValueStr),
    number_string(Value, ValueStr).

optional_verb --> [increases].
optional_verb --> [reduces].
optional_verb --> [is].
optional_verb --> [].
optional_subject --> [general].
optional_subject --> [].

is_junk_token('.') :- !.
is_junk_token(',') :- !.
is_junk_token('%') :- !.
is_junk_token('"') :- !.
is_junk_token('“') :- !.
is_junk_token('”') :- !.
is_junk_token('’') :- !.
is_junk_token(_) :- fail.

% entry point from main
parse_all_buffs(InputStr, FlatBuffs) :-
    normalize_space(string(Cleaned), InputStr),
    split_string(Cleaned, ".", "", Sentences),
    maplist(string_lower, Sentences, Lowered),
    maplist(tokenize_and_parse, Lowered, BuffLists),
    append(BuffLists, FlatBuffs).

tokenize_and_parse(SentenceStr, Buffs) :-
    tokenize_atom(SentenceStr, RawTokens),
    exclude(is_junk_token, RawTokens, Tokens),
    format('DEBUG: Tokens about to be matched: ~w~n', [Tokens]),
    ( phrase(sentence_buffs(Buffs), Tokens) ->
        true
    ;   Buffs = []  % fallback on failure
    ),
    format('DEBUG: Buffs are: ~w~n', [Buffs]).

% perl and prolog do not agree on what does
% and does not need to be quoted

print_buffs([]).
print_buffs([buff(Attr, Troop, Val, Conds)|Rest]) :-
    format("buff(~w,~w,~w,[", [Attr, Troop, Val]),
    print_conditions(Conds),
    format("])~n", []),
    print_buffs(Rest).

print_conditions([]).
print_conditions([C]) :-
    format("~q", [C]).
print_conditions([C|Cs]) :-
    format("~q,", [C]),
    print_conditions(Cs).

main :-
    read(InputAtom),
    atom_string(InputAtom, InputStr),
    parse_all_buffs(InputStr, Buffs),
    print_buffs(Buffs),
    halt.

wrap_normalize(In, Out) :-
    ( catch(normalize_space(string(Out), In), E, (print_message(error, E), Out = "")) ).

:- initialization(main).

% File: buff_parser.pl

:- include('EvonyBuffDictionary').
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- style_check(-singleton).

% Top-level DCG to extract a flat list of buffs from a tokenized sentence
sentence_buffs(All) -->
  buff(Buffs),
  sentence_buffs(Rest),
  { append(Buffs, Rest, All) }.
sentence_buffs([]) --> [].

% Entry point for a single buff pattern
buff(B) --> buff_pattern(B).


% Pattern 3: Alternate matrix variant
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

% Pattern 2: Matrix expansion (bifurcated object of the verb, but shared conditions.)
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

% Pattern 1: Simple one-buff
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





% DCG wrapper for parsing and flattening condition sets
merged_conditions(UniqueConditions) -->
  remainder(Rest),
  {
    extract_condition_atoms(Rest, UniqueConditions)
  }.

extract_condition_atoms(Tokens, UniqueConditions) :-
  format('DEBUG: Raw condition tokens: ~w~n', [Tokens]),

  % Check if we need disjunctive processing (contains "or" or complex overlaps)
  ( needs_disjunctive_processing(Tokens) ->
    % Complex case: use the full disjunctive logic
    findall(Flat,
      (
      disjunct_variant(Tokens, Variant),
      normalize_condition_tokens(Variant, Flat),
      format('DEBUG: Normalized Canonicals: ~w~n', [Flat])
      ),
      AllConds),
    flatten(AllConds, FlatAll),
    list_to_set(FlatAll, ConditionsSet)
  ;
    % Simple case: use greedy longest-match processing
    greedy_condition_match(Tokens, ConditionsSet),
    format('DEBUG: Greedy match result: ~w~n', [ConditionsSet])
  ),

  % ADD THIS LINE: Actually assign the result to UniqueConditions
  UniqueConditions = ConditionsSet,

  format("DEBUG: Final condition sets: ~w~n", [UniqueConditions]).

% Greedy longest-match for simple cases
greedy_condition_match([], []).
greedy_condition_match(Tokens, [Condition|Rest]) :-
    % Find the longest possible match starting from current position
    find_longest_match(Tokens, Match, Remaining, Condition),
    !,  % Cut to prevent backtracking
    greedy_condition_match(Remaining, Rest).
greedy_condition_match([_|Tokens], Conditions) :-
    % Skip unmatched token and continue
    greedy_condition_match(Tokens, Conditions).

% Find the single longest match at the current position
find_longest_match(Tokens, BestMatch, BestRemaining, BestCondition) :-
    findall([Len, Match, Remaining, Condition],
            (match_any_condition(Tokens, Match, Remaining, Condition),
              length(Match, Len)),
            Matches),
    Matches \= [],  % Ensure we found at least one match
    sort(1, @>=, Matches, [[_MaxLen, BestMatch, BestRemaining, BestCondition]|_]).


% Normalize a single token list variant into canonical conditions
normalize_condition_tokens([], []).
normalize_condition_tokens(Input, [Canonical|Rest]) :-
  match_any_condition(Input, Match, Remaining, Canonical),
  normalize_condition_tokens(Remaining, Rest).
normalize_condition_tokens([_|T], Norm) :-
  normalize_condition_tokens(T, Norm). % skip unmatched

% Detect if disjunctive processing is needed
needs_disjunctive_processing(Tokens) :-
  member(or, Tokens), !.
needs_disjunctive_processing(Tokens) :-
  % Add other complex cases here if needed
  fail.

% Generate disjunctive split variants from input tokens
disjunct_variant(Tokens, Left) :-
  append(Prefix, [or | Suffix], Tokens),
  append(PrefixStart, LeftDisj, Prefix),
  append(RightDisj, SuffixEnd, Suffix),
  append(PrefixStart, LeftDisj, Left).

disjunct_variant(Tokens, Right) :-
  append(Prefix, [or | Suffix], Tokens),
  append(PrefixStart, _LeftDisj, Prefix),
  append(RightDisj, _SuffixEnd, Suffix),
  append(PrefixStart, RightDisj, Right).

disjunct_variant(Tokens, Tokens). % default: no "or"


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




% Expand the matrix: create all troop×attribute combinations
expand_matrix(Troops, Attrs, Value, ConditionSets, Buffs) :-
  findall(buff(Attr, Troop, Value, Conds),
   (
      member(Troop, Troops),
      member(Attr, Attrs),
      member(Conds, ConditionSets)
   ),
   Buffs).



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


% Helper to extract numeric value from percentage
extract_value(ValueAtom, Value) :-
  atom_string(ValueAtom, ValueStr),
  number_string(Value, ValueStr).

% Helper for + value extraction
extract_value_plus(ValueAtom, Value) :-
  atom_number(ValueAtom, Value).

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
  ;  Buffs = [] % fallback on failure
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

print_buffs_json([]).
print_buffs_json([buff(Attr, Troop, Val, Conds)|Rest]) :-
  Buff = json([attribute=Attr, troop=Troop, value=Val, conditions=Conds]),
  with_output_to(string(JSONStr), json_write_dict(current_output, Buff, [compact(true)])),
  format("~s~n", [JSONStr]),
  print_buffs_json(Rest).

main :-
  read(InputAtom),
  atom_string(InputAtom, InputStr),
  parse_all_buffs(InputStr, Buffs),
  print_buffs(Buffs),    % For DEBUG: human-readable output
  print_buffs_json(Buffs),  % For machine parsing
  halt.

wrap_normalize(In, Out) :-
  ( catch(normalize_space(string(Out), In), E, (print_message(error, E), Out = "")) ).

:- initialization(run_if_script).

run_if_script :-
  current_prolog_flag(argv, Argv),
  Argv \= [],
  main.

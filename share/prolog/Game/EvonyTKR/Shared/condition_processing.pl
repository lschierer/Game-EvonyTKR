% File: condition_processing.pl
% All condition parsing and processing logic using modern DCG rules

% Normalize empty conditions in buff lists
normalize_empty_conditions([], []).
normalize_empty_conditions([buff(Attr, Troop, Val, Conds)|Rest], [buff(Attr, Troop, Val, NormConds)|NormRest]) :-
  normalize_single_condition(Conds, NormConds),
  normalize_empty_conditions(Rest, NormRest).

% Handle different empty condition formats
normalize_single_condition([], []) :- !.           % Empty list stays empty
normalize_single_condition('', []) :- !.           % Empty string becomes empty list
normalize_single_condition([Cond], [Cond]) :- !.   % Single condition stays as-is
normalize_single_condition(Conds, Conds).          % Multiple conditions stay as-is

% === Simple condition matchers ===

% Matches a single condition phrase (including synonyms) and returns its canonical string
short_condition(CondStr) -->
    condition(Match),
    {
        atomics_to_string(Match, ' ', CondStr),
        format("DEBUG: short_condition matched: ~w -> ~w~n", [Match, CondStr])
    }.

% Matches a single condition phrase from start of input and returns canonical string
consume_condition(Canonical) -->
    remainder(Tokens),
    {
        greedy_condition_match(Tokens, [Canonical])
    }.

% === Merged condition sets (multiple conditions, possibly with "or") ===

merged_conditions(UniqueConditions) -->
    remainder(Rest),
    {
        extract_condition_atoms(Rest, UniqueConditions)
    }.

% === Helper for Disjunction ===

% for use inside disjunct_variant loops
normalize_condition_tokens([], []) :- !.

normalize_condition_tokens(Input, [Canonical|Rest]) :-
    match_any_condition(Input, _Match, Remaining, Canonical),
    !,
    normalize_condition_tokens(Remaining, Rest).

normalize_condition_tokens([_|T], Rest) :-
    normalize_condition_tokens(T, Rest).

% === Disjunction and greedy parsing ===

% Main condition extraction logic - handles both simple and complex cases
extract_condition_atoms(Tokens, UniqueConditions) :-
  format('DEBUG: Raw condition tokens: ~w~n', [Tokens]),
  ( needs_disjunctive_processing(Tokens) ->
      % Use tolerant match for overlapping disjunctions
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
      % Default: greedy one-pass match
      greedy_condition_match(Tokens, ConditionsSet),
      format('DEBUG: Greedy match result: ~w~n', [ConditionsSet])
  ),
  UniqueConditions = ConditionsSet,
  format("DEBUG: Final condition sets: ~w~n", [UniqueConditions]).

% Detect if disjunctive processing is needed
needs_disjunctive_processing(Tokens) :-
    member(or, Tokens), !.
needs_disjunctive_processing(_) :- fail.

% === Greedy matcher ===

greedy_condition_match([], []).
greedy_condition_match(Tokens, [Canonical | Rest]) :-
    find_longest_match(Tokens, Match, Remaining, Canonical),
    !,
    greedy_condition_match(Remaining, Rest).
greedy_condition_match([_ | Tokens], Rest) :-
    greedy_condition_match(Tokens, Rest).

find_longest_match(Tokens, BestMatch, BestRemaining, BestCanonical) :-
    findall([Len, Match, Remaining, Canonical],
        ( match_any_condition(Tokens, Match, Remaining, Canonical),
          length(Match, Len)
        ),
        Matches),
    Matches \= [],
    sort(1, @>=, Matches, [[_Len, BestMatch, BestRemaining, BestCanonical] | _]).

% === Condition matching ===

match_any_condition(Input, Match, Remaining, Canonical) :-
  phrase(condition(Match), Input, Remaining),
  atomics_to_string(Match, ' ', Canonical),
  format("DEBUG: matched condition: ~w -> ~w~n", [Match, Canonical]).

match_any_condition(_, _, _, _) :-
  format("DEBUG: match_any_condition failed on input~n"),
  fail.

% === Disjunctive splits ===

% Only generate exactly two variants: one for each disjunct branch
disjunct_variant(Tokens, Left) :-
    append(Left, [or | _], Tokens),
    Left \= [],
    format("DEBUG: disjunct_variant LEFT: ~w~n", [Left]).

disjunct_variant(Tokens, RightWithContext) :-
    append(Left, [or | Right], Tokens),
    Left \= [],
    Right \= [],
    % Optional: extract context prefix
    prefix_context(Left, Context),
    append(Context, Right, RightWithContext),
    format("DEBUG: disjunct_variant RIGHT: ~w~n", [RightWithContext]).

% Helper to extract a context (e.g., ['brings', 'any'])
% For now, just return first 2 tokens of Left if present
prefix_context(Left, Context) :-
    length(Left, L),
    ( L >= 2 -> append(Context, _, Left), length(Context, 2)
    ; Context = Left ).

% === Token cleanup ===

tokens_to_match([], _) --> [].

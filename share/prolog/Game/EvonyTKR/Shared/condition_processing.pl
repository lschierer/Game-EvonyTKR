% File: condition_processing.pl
% All condition parsing and processing logic

% DCG wrapper for parsing and flattening condition sets
merged_conditions(UniqueConditions) -->
  remainder(Rest),
  {
    extract_condition_atoms(Rest, UniqueConditions)
  }.

% Main condition extraction logic - handles both simple and complex cases
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

  % Assign the result to UniqueConditions
  UniqueConditions = ConditionsSet,
  format("DEBUG: Final condition sets: ~w~n", [UniqueConditions]).

% Detect if disjunctive processing is needed
needs_disjunctive_processing(Tokens) :-
  member(or, Tokens), !.
needs_disjunctive_processing(_Tokens) :-
  % Add other complex cases here if needed
  fail.

% === GREEDY MATCHING (for simple cases) ===

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

% === DISJUNCTIVE PROCESSING (for complex cases with "or") ===

% Generate disjunctive split variants from input tokens
disjunct_variant(Tokens, Left) :-
  append(Prefix, [or | Suffix], Tokens),
  append(PrefixStart, LeftDisj, Prefix),
  append(_RightDisj, _SuffixEnd, Suffix),
  append(PrefixStart, LeftDisj, Left).

disjunct_variant(Tokens, Right) :-
  append(Prefix, [or | Suffix], Tokens),
  append(PrefixStart, _LeftDisj, Prefix),
  append(RightDisj, SuffixEnd, Suffix),
  append(PrefixStart, RightDisj, Right).

disjunct_variant(Tokens, Tokens). % default: no "or"

% Normalize a single token list variant into canonical conditions
normalize_condition_tokens([], []).
normalize_condition_tokens(Input, [Canonical|Rest]) :-
  match_any_condition(Input, _Match, Remaining, Canonical),
  normalize_condition_tokens(Remaining, Rest).
normalize_condition_tokens([_|T], Norm) :-
  normalize_condition_tokens(T, Norm). % skip unmatched

% === CONDITION MATCHING ===

% Match either a canonical or synonym condition prefix
% Match mapped synonyms first (more specific)
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

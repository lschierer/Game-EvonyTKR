% File: parsing_helpers.pl
% Helper predicates for parsing troops, attributes, values, etc.

% === TROOP PARSING ===

% Parse troop list: "ground troops and mounted troops" or just "troops"
troop_list([T1, T2]) -->
  troop(T1), [and], troop(T2).
troop_list([T]) -->
  troop(T).

% === ATTRIBUTE PARSING ===

% Parse attribute list: "defense and hp"
attribute_list([A1, A2]) -->
  attribute(A1), [and], attribute(A2).
attribute_list([A]) -->
  attribute(A).

% Parse subcity attribute list
subcity_attribute_list([A1, A2]) -->
  subcity_attribute(A1), [and], subcity_attribute(A2).
subcity_attribute_list([A1]) -->
  subcity_attribute(A1).

% Helper to add subcity prefix
add_subcity_prefix(BaseAttr, SubCityAttr) :-
    atom_concat('subcity_', BaseAttr, SubCityAttr).

% === VALUE EXTRACTION ===

% Helper to extract numeric value from percentage (standard format)
extract_value(ValueAtom, Value) :-
  atom_string(ValueAtom, ValueStr),
  number_string(Value, ValueStr).

% Helper for + value extraction (plus format)
extract_value_plus(ValueAtom, Value) :-
  atom_number(ValueAtom, Value).

% === MATRIX EXPANSION ===

% Expand the matrix: create all troop×attribute combinations
expand_matrix(Troops, Attrs, Value, ConditionSets, Buffs) :-
    findall(buff(Attr, Troop, Value, NormalizedConds),
      (
        member(Troop, Troops),
        member(Attr, Attrs),
        member(Conds, ConditionSets),
        normalize_condition_format(Conds, NormalizedConds)  % Normalize conditions
      ),
      Buffs).

% Normalize condition format to always be a list
normalize_condition_format(Conds, [Conds]) :-
    atom(Conds), !.  % Single atom -> wrap in list
normalize_condition_format(Conds, Conds) :-
    is_list(Conds).  % Already a list -> keep as-is

% === OPTIONAL ELEMENTS ===

% --- Reusable Optional Phrase Fragments ---
optional_when_general_is --> [when], [general], [is] ; [].
optional_when_general --> [when], [general] ; [].
optional_general_is --> ([general], [is]) ; [].
optional_when --> [when] ; [].
optional_verb --> [increases] ; [reduces] ; [grants]; [].
optional_verb --> [increases].
optional_verb --> [reduces].
optional_verb --> [is].
optional_verb --> [].
optional_subject --> [general] ; [you]; [].
optional_value_adj --> [+] ; [by] ; [by] ,[another] ; [].
optional_plus --> [+] ; [].
optional_by   --> [by] ; [].
optional_the  --> [the] ; [].


% Optional troop placeholder that binds to '' if not matched
troop('') -->
  [troop].
troop('') -->
  [troops].



% === TOKEN FILTERING ===

% to identify when the sentence splitter resulted in an
% empty string.
string_empty(S) :- string_length(S, Len), Len =:= 0.

% Identify junk tokens to filter out
is_junk_token('a') :- !.
is_junk_token('an') :- !.
is_junk_token('.') :- !.
is_junk_token(',') :- !.
is_junk_token('%') :- !.
is_junk_token('"') :- !.        % ASCII double quote
is_junk_token('“') :- !.        % Unicode left double quote
is_junk_token('”') :- !.        % Unicode right double quote
is_junk_token('’') :- !.        % Unicode right single quote/apostrophe
is_junk_token('\'') :- !.       % ASCII single quote/apostrophe (escaped)
is_junk_token(_) :- fail.


% === UTILITY PREDICATES ===

% Wrapper for normalize_space with error handling
wrap_normalize(In, Out) :-
  ( catch(normalize_space(string(Out), In), E, (print_message(error, E), Out = "")) ).

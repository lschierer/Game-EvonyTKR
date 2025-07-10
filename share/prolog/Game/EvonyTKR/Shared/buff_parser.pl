% File: buff_parser.pl

:- include('EvonyBuffDictionary').
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).

% Rule to filter out definite articles  (except 'the' because I need it)
filter_articles --> [a], !, filter_articles. % Similarly, handle "a" and "an" if needed.
filter_articles --> [an], !, filter_articles.
filter_articles --> [Word], { \+ is_article(Word) }, filter_articles. % If not an article, include the word and continue filtering.
filter_articles --> []. % Base case: an empty list is processed as is.

% Predicate to check if a word is a definite article
is_article(the).
is_article(a).
is_article(an).

% Parse the input string into sentences, then into buffs
parse_text(Buffs) -->
    sentences(SentenceStrings),
    { format('DEBUG: Found sentences: ~w~n', [SentenceStrings]),
      maplist(parse_sentence_to_buffs, SentenceStrings, BuffLists),
      append(BuffLists, Buffs)
    }.

% Parse sentences separated by periods
sentences([S|Ss]) -->
    string_without(".", Codes),
    { Codes \= [], atom_codes(S, Codes) },
    ( "." -> sentences(Ss) ; {Ss = []} ).
sentences([]) --> [].


% Convert a sentence string to buffs
parse_sentence_to_buffs(SentenceAtom, Buffs) :-
    format('DEBUG: Parsing sentence: ~w~n', [SentenceAtom]),
    atom_string(SentenceAtom, SentenceStr),
    split_string(SentenceStr, ' ', ' ', TokenStrings),
    maplist(atom_string, Tokens, TokenStrings),
    format('DEBUG: Sentence tokens: ~w~n', [Tokens]),
    ( phrase(sentence_buffs(Buffs), Tokens) ->
        format('DEBUG: Sentence parsed successfully: ~w~n', [Buffs])
    ; format('DEBUG: Sentence parsing failed~n', []),
      Buffs = []
    ).


% Parse buffs within a single sentence (tokenized)
sentence_buffs([B|Bs]) --> buff(B), sentence_buffs(Bs).
sentence_buffs(Buffs) --> buff(Buffs).
sentence_buffs([]) --> [].

% Pattern 1: Original simple pattern
buff(buff(AttributeAtom, TroopAtom, Value, ConditionList)) -->
    optional_verb,
    troop(TroopAtom),      % Automatically tries all troop patterns
    attribute(AttributeAtom), % Automatically tries all attribute patterns
    [by], [ValueAtom],
    [when], optional_subject, optional_verb, condition_phrase(ConditionList),
    { extract_value(ValueAtom, Value) }.

% Pattern 2: Matrix expansion for "increases ground troops and mounted troops defense and hp by 40%..."
buff(Buffs) -->
    optional_verb,
    troop_list(Troops),
    attribute_list(Attributes),
    [by], [ValueAtom],
    [when], condition_phrase(Condition),
    { extract_value(ValueAtom, Value),
      expand_matrix(Troops, Attributes, Value, Condition, Buffs)
    }.

% Pattern 3: not quite so Matrixed for Sentence tokens: [increases,ground,troops,and,mounted,troops,attack,by,15%,when,general,brings,any,dragon]
buff(Buffs) -->
    optional_verb,
    troop_list(Troops),
    attribute_list(Attributes),
    [by], [ValueAtom],
    [when], condition_phrase(ConditionList),
    { extract_value(ValueAtom, Value) }.


% Helper for + value extraction
extract_value_plus(ValueAtom, Value) :-
    atom_number(ValueAtom, Value).

% Helper for condition words
condition_word(marching) --> [marching].
condition_word(attacking) --> [attacking].

% Helper for location words
location_word('in-city') --> ['in-city'].
location_word('out-city') --> ['out-city'].

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

% Parse individual troop names
troop_name(ground_troops) --> [ground], [troops].
troop_name(mounted_troops) --> [mounted], [troops].

% Parse condition phrase
condition_phrase(Conditions) -->
    condition_tokens(TokenList),
    { parse_complex_condition(TokenList, Conditions) }.

% Extract all tokens from "when" to end of sentence
condition_tokens([T|Ts]) --> [T], condition_tokens(Ts).
condition_tokens([]) --> [].

parse_complex_condition(TokenList, Conditions) :-
    % Normalize tokens: handle both single token and multi-token substitutions
    condition_map(TokenList, NormalizedTokens),
    %format('DEBUG: Original tokens: ~w~n', [TokenList]),
    %format('DEBUG: Normalized tokens: ~w~n', [NormalizedTokens]),

    % Convert to set for subset matching
    list_to_set(NormalizedTokens, TokenSet),
    %format('DEBUG: Token set: ~w~n', [TokenSet]),

    findall(Condition, (
        condition(ConditionStrings),
        %format('DEBUG: Trying condition: ~w~n', [ConditionStrings]),
        % Convert condition strings to atoms
        maplist(atom_string, ConditionAtoms, ConditionStrings),
        %format('DEBUG: Condition as atoms: ~w~n', [ConditionAtoms]),
        ( subset(ConditionAtoms, TokenSet) ->
            ( atomic_list_concat(ConditionStrings, ' ', Condition)%,
              %format('DEBUG: MATCHED condition: ~w -> ~w~n', [ConditionAtoms, Condition])
            )
        ; ( %format('DEBUG: FAILED to match condition: ~w~n', [ConditionAtoms]),
            fail
          )
        )
    ), Conditions).


% Normalize the entire token list to handle multi-token patterns
condition_map([], []).
condition_map([leading, the, army, to, attack, monsters|Rest], ['against monsters'|NormalizedRest]) :-
    !,
condition_map([to, attack|Rest], [attacking|NormalizedRest]) :-
    !,
    condition_map(Rest, NormalizedRest).
condition_map([any|Rest], [a|NormalizedRest]) :-
    !,
    condition_map(Rest, NormalizedRest).
condition_map([Token|Rest], [Token|NormalizedRest]) :-
    condition_map(Rest, NormalizedRest).


% Expand the matrix: create all troop×attribute combinations
expand_matrix(Troops, Attributes, Value, Condition, Buffs) :-
    findall(buff(Attr, Troop, Value, Condition),
            (member(Troop, Troops), member(Attr, Attributes)),
            Buffs).


% Helper to extract numeric value from percentage
extract_value(ValueAtom, Value) :-
    atom_string(ValueAtom, ValueStr),
    string_concat(NumStr, "%", ValueStr),
    number_string(Value, NumStr).

optional_verb --> [increases].
optional_verb --> [reduces].
optional_verb --> [is].
optional_verb --> [].
optional_subject --> [general].
optional_subject --> [].

main :-
  read_term(InputString, []),
  format('DEBUG: Input string: ~w~n', [InputString]),
  atom_codes(InputString, Codes),
  ( phrase(parse_text(Buffs), Codes) ->
      format('DEBUG: Parse succeeded with: ~w~n', [Buffs]),
      write_term(Buffs, [quoted(true)]), nl
  ; format('DEBUG: Parse failed~n', []),
    writeln('[]') ),
  halt.

:- initialization(main).

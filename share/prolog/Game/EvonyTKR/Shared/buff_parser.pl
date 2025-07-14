% File: buff_parser.pl
% Main entry point for buff parsing - delegates to specialized modules

:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).
:- style_check(-singleton).
:- discontiguous troop/3.
:- include('EvonyBuffDictionary').
:- include('buff_patterns').
:- include('condition_processing').
:- include('parsing_helpers').
:- include('output_formatting').
:- use_module(library(error)).

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).

ensure_grounded(Term) :-
    ( ground(Term)
    -> true
    ;  throw(error(instantiation_error, Term))
    ).

% Main entry point from command line
main :-
  set_stream(user_input, encoding(utf8)),
  set_stream(user_output, encoding(utf8)),
  set_stream(user_error, encoding(utf8)),
  read(InputAtom),
  atom_string(InputAtom, InputStr),
  parse_all_buffs(InputStr, Buffs),
  print_buffs(Buffs),    % For DEBUG: human-readable output
  maplist(ensure_grounded, Buffs), % ensure everything is actually there to print
  print_buffs_json(Buffs),  % For machine parsing
  halt.

% Entry point from main - parses input string into buffs
parse_all_buffs(InputStr, FlatBuffs) :-
  normalize_space(string(Cleaned), InputStr),
  split_string(Cleaned, ".;", "", Sentences),
  maplist(string_lower, Sentences, Lowered),
  maplist(tokenize_and_parse, Lowered, BuffLists),
  append(BuffLists, FlatBuffs).

% Tokenize and parse a single sentence
tokenize_and_parse(SentenceStr, Buffs) :-
  tokenize_atom(SentenceStr, RawTokens),
  exclude(is_junk_token, RawTokens, Tokens),
  format('DEBUG: Tokens about to be matched: ~w~n', [Tokens]),
  ( phrase(sentence_buffs(Buffs), Tokens) ->
    true
  ;  Buffs = [] % fallback on failure
  ),
  format('DEBUG: Buffs are: ~w~n', [Buffs]).

% Top-level DCG to extract a flat list of buffs from a tokenized sentence
sentence_buffs(All) -->
  buff(Buffs),
  sentence_buffs(Rest),
  { append(Buffs, Rest, All) }.
sentence_buffs([]) --> [].

% Entry point for a single buff pattern
buff(B) --> buff_pattern(B).

:- initialization(run_if_script).

run_if_script :-
  current_prolog_flag(argv, Argv),
  Argv \= [],
  main.

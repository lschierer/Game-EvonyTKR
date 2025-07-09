% File: buff_parser.prolog

:- include('EvonyBuffGrammar').
:- use_module(library(readutil)).
:- initialization(main).

main :-
  read_line(Line),
  atom_codes(Atom, Line),
  tokenize_atom(Atom, Tokens),
  ( phrase(buffs(Buffs), Tokens) ->
      write_term(Buffs, [quoted(true)]), nl
  ; writeln('[]') ),
  halt.

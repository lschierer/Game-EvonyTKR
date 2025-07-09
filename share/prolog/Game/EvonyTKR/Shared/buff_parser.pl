% File: buff_parser.prolog

:- include('EvonyBuffDictionary').
:- include('EvonyBuffGrammar').
:- initialization(main).

main :-
    read(Tokens),  % expect a list like: [increases, mounted, troops, attack, by, 50]
    ( phrase(buffs(Buffs), Tokens) ->
        write_term(Buffs, [quoted(true)]), nl
    ; write('[]')
    ),
    halt.

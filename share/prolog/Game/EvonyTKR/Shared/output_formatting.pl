% File: output_formatting.pl
% All output formatting and printing logic

% === HUMAN-READABLE OUTPUT ===

% Print buffs in human-readable format for debugging
print_buffs([]).
print_buffs([buff(Attr, Troop, Val, Conds)|Rest]) :-
  format("buff(~w,~w,~w,[", [Attr, Troop, Val]),
  print_conditions(Conds),
  format("])~n", []),
  print_buffs(Rest).

% Print condition lists with proper formatting
print_conditions([]) :- !.  % Empty list - print nothing
print_conditions([C]) :-
  format("~q", [C]).
print_conditions([C|Cs]) :-
  format("~q,", [C]),
  print_conditions(Cs).

% === JSON OUTPUT ===

% Print buffs in JSON format for machine parsing
print_buffs_json([]).
print_buffs_json([buff(Attr, Troop, Val, Conds)|Rest]) :-
  normalize_json_conditions(Conds, JsonConds),
  Buff = json([attribute=Attr, troop=Troop, value=Val, conditions=JsonConds]),
  with_output_to(string(JSONStr), json_write_dict(current_output, Buff, [compact(true)])),
  format("~s~n", [JSONStr]),
  print_buffs_json(Rest).

% Normalize conditions for JSON output
normalize_json_conditions([], []) :- !.      % Empty list stays empty
normalize_json_conditions(Conds, Conds).    % Non-empty conditions stay as-is

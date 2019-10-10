% ---------------------------------------------------------
% prolog_vars_to_json_vars/2
% ---------------------------------------------------------

prolog_vars_to_json_vars(Term, Term) :-
    reset_gensym,
    term_variables(Term, Variables),
    generate_symbols(Variables).

generate_symbols([]).
generate_symbols([Var|Vars]) :-
    gensym('V', Var),
    generate_symbols(Vars).
    
% ---------------------------------------------------------
% json_vars_to_prolog_vars/2
% ---------------------------------------------------------
 
json_vars_to_prolog_vars(JsonVars, PrologVars) :-
    json_vars_to_prolog_vars(JsonVars, PrologVars, [], Vars),
    fix_variables(Vars).

json_vars_to_prolog_vars([], [], Vars, Vars).

json_vars_to_prolog_vars([Key=List1|Rest1], [Key=List2|Rest2], Rest3, Vars) :-
    Key = 'Var',
    associate_jvars_with_pvars(List1, List2, Rest3, List4), 
    json_vars_to_prolog_vars(Rest1, Rest2, List4, Vars).

json_vars_to_prolog_vars([Key=C1|Rest1], [Key=V1|Rest2], Rest3, Vars) :-
    Key = 'Var',
    atomic(C1),
    json_vars_to_prolog_vars(Rest1, Rest2, [C1=V1|Rest3], Vars).

json_vars_to_prolog_vars(json(List1), json(List2), Vars1, Vars2) :-
    json_vars_to_prolog_vars(List1, List2, Vars1, Vars2).

json_vars_to_prolog_vars([json(List1)|Rest1], [json(List2)|Rest2], Vars1, Vars3) :-
    json_vars_to_prolog_vars(List1, List2, Vars1, Vars2),
    json_vars_to_prolog_vars(Rest1, Rest2, Vars2, Vars3).

json_vars_to_prolog_vars([Key=List1|Rest1], [Key=List2|Rest2], Vars1, Vars3) :-
    json_vars_to_prolog_vars(List1, List2, Vars1, Vars2),
    json_vars_to_prolog_vars(Rest1, Rest2, Vars2, Vars3).
  
json_vars_to_prolog_vars([Key=Value|Rest1], [Key=Value|Rest2], Vars1, Vars2) :-
    json_vars_to_prolog_vars(Rest1, Rest2, Vars1, Vars2).

json_vars_to_prolog_vars([Term|Rest1], [Term|Rest2], Vars1, Vars2) :-
    json_vars_to_prolog_vars(Rest1, Rest2, Vars1, Vars2).

json_vars_to_prolog_vars(Term, Term, Vars, Vars).

associate_jvars_with_pvars(List1, List2, Rest3, List4) :-
   associate_variables(List1, List2, List3),
   append(List3, Rest3, List4).

associate_variables([], [], []).
associate_variables([JVar|JVars], [PVar|PVars], [JVar=PVar|Rest]) :-
   associate_variables(JVars, PVars, Rest).

fix_variables(Vars) :-
   fix_variables(Vars, Vars, Vars).

fix_variables([], Vars, Vars).

fix_variables([C1=V1|Rest1], Vars1, Vars3) :-
    fix_variable(C1=V1, Vars1, Vars2),
    fix_variables(Rest1, Vars2, Vars3).

fix_variable(_, [], []).

fix_variable(C1=V1, [C2=V2|Rest1], [C1=V1|Rest2]) :-
    C1 == C2,
    V1 = V2,
    fix_variable(C1=V1, Rest1, Rest2).

fix_variable(C1=V1, [C2=V2|Rest1], [C2=V2|Rest2]) :-
    fix_variable(C1=V1, Rest1, Rest2).	
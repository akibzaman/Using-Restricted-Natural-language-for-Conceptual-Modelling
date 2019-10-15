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
	
%--------------------------------------------------------------------------------------

lr_attribute_match(V, SV, P1, P3):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:attribute, dt:DT, arg:X, sem:Sem]), Ent),
  search_v_att_lr(Ent, P1, V), downcase_list(V,V1), append(V1, P3, P1).
  
search_v_att_lr([], _, _) :- false.

search_v_att_lr([En|Ent], P1, P2):-
  downcase_list(En,V), 
  (sublist(V, P1) -> P2 = En ; search_v_att_lr(Ent, P1, P2)).

downcase_list(AnyCaseList, DownCaseList):-
  maplist(downcase_atom, AnyCaseList, DownCaseList).
  
sublist(Sub, List) :-
   sublist_(List, Sub).
   
sublist_([], []).
sublist_([H|T], Sub) :-
	sublist__(T, H, Sub).

sublist__([], H, [H]).
sublist__([], _, []).
sublist__([H|T], X, [X|Sub]) :-
  sublist__(T, H, Sub).
sublist__([H|T], _, Sub) :-
  sublist__(T, H, Sub).
  
downcase_noun(WForm, W, L1, L2):-
  downcase_list(WForm, W), append(W, L2, L1).
  
morphology(W, Wo):-
	  (sub_atom(W,_, 2, 0, C), (C == sh; C = ch)); (sub_atom(W,_,1,0,P), (P == s; P == z; P == x)) -> atom_concat(W,es,Wo) ; 
	  (sub_atom(W,Q,1,0,L), (L == y)) -> sub_atom(W,_,Q,1,L1), atom_concat(L1,ies,Wo) ; atom_concat(W,s,Wo).  

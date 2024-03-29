:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:-consult(p_to_j).


:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#'). 

 s([mode:M, type:entity, sem:json(['Atom'=Sem])]) --> 
   np([mode:M, num:sg, type:entity, pos:subj, sem:Sem]), 
   [is, an, entity, type], ['.'].

 np([mode:M, num:N, type:T, pos:P, sem:Sem]) --> 
   noun([mode:M, num:N, type:T, pos:P, sem:Sem]). 

 noun([mode:proc, num:N, type:entity, pos:P, sem:Sem]) --> 
   lexical_rule([cat:noun, num:N, type:entity, pos:P, sem:Sem]). 

 noun([mode:gen, num:N, type:entity, pos:P, sem:Sem]) --> 
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:_X, sem:Sem])}, 
   WForm. 

%------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, pos:P, sem:Sems], List1, List2) :-
   process_noun([wform:WForm, List5, List1, List2]), 
   downcase_list(WForm, DWForm), atomic_list_concat(DWForm, ' ', WForm_sg), morphology(WForm_sg, WForm_pl),
   Sems = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]),
   Semo = json(['Rel'=entity, 'Ind'=List5, 'Var'=Y]),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:subj, arg:X, sem:Sems])),
   assert(lexicon([cat:noun, wform:[WForm_sg], num:sg, type:entity, pos:obj, arg:X, sem:Semo])),
   assert(lexicon([cat:noun, wform:[WForm_pl], num:pl, type:entity, pos:obj, arg:X, sem:Semo])).
   
process_noun([wform:List3, List5, List1, List2]) :-
   append(List3, [is, an, entity, type, '.'], List1), 
   List2 = [is, an, entity, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5).
   %Sem = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]).

  
test1 :-
    s([mode:proc, type:entity, sem:PrologTerm],
      ['Student', is, an, entity, type, '.'], []),
	prolog_vars_to_json_vars(PrologTerm, JSONTerm),
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    write(JSON).
	
test2 :-
	JSON = '{
				"Atom": 
						{
							"Rel":"entity", 
							"Ind":"student", 
							"Var":"X"
						}
			}',
	atom_json_term(JSON, JSONTerm, [as(atom)]),
    s([mode:gen, type:entity, sem:JSONTerm], S, []), write(S), nl, nl.
	

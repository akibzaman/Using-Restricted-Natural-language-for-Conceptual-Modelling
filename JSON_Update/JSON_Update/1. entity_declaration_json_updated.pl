:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:- consult(predicates).      

s([mode:M, type:entity, sem:json(['Atom'=Sem])]) --> 
  np([mode:M, num:sg, type:entity, pos:subj, sem:Sem]), 
  [is, an, entity, type], ['.'].

np([mode:M, num:N, type:T, pos:P, sem:Sem]) --> 
  noun([mode:M, num:N, type:T, pos:P, sem:Sem]). 

noun([mode:proc, num:N, type:entity, pos:subj, sem:Sem]) -->        
  lexical_rule([cat:noun, num:N, type:entity, pos:subj, sem:Sem]). 

noun([mode:gen, num:N, type:entity, pos:subj, sem:Sem]) -->         
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:_X, sem:Sem])}, 
  WForm. 

%------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, pos:subj, sem:Sems], List1, List2) :-  
   process_noun([wform:WForm, List5, List1, List2]), 
   downcase_list(WForm, DWForm), last(WForm, L), morphology(L, Lp), 
   append([W], [L], DWForm), append([W], [Lp], WLp), 
   Sems = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]),
   Semo = json(['Rel'=entity, 'Ind'=List5, 'Var'=Y]),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:subj, arg:_, sem:Sems])),
   assert(lexicon([cat:noun, wform:DWForm, num:sg, type:entity, pos:obj, arg:_, sem:Semo])),
   assert(lexicon([cat:noun, wform:WLp, num:pl, type:entity, pos:obj, arg:_, sem:Semo])).
   
process_noun([wform:List3, List5, List1, List2]) :-
   append(List3, [is, an, entity, type, '.'], List1), 
   List2 = [is, an, entity, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5).

  
test1 :-
    s([mode:proc, type:entity, sem:PrologTerm],
      ['Fish', tank, is, an, entity, type, '.'], []),
	prolog_vars_to_json_vars(PrologTerm, JSONTerm),
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    writeq(JSON).
	
test2 :-
	JSON = '{
				"Atom": 
						{
							"Rel":"entity", 
							"Ind":"fish_tank", 
							"Var":"X"              
						}
			}',
	atom_json_term(JSON, JSONTerm, [as(atom)]),
	json_vars_to_prolog_vars(JSONTerm, PrologVars),
    s([mode:gen, type:entity, sem:PrologVars], S, []),!, 
    writeq(S), nl, nl.                                  
	

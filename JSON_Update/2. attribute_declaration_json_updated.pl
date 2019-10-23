:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:- consult(predicates).


s([mode:M, type:attribute, sem:json(['Atom'=Sem])]) --> 
  np([mode:M, num:N, type:attribute, pos:subj, dt:DT, sem:Sem]),       % LOOKS LIKE pos:subj IS NOT REQUIRED ANYMORE; THIS IS ODD
  [is, of, DT, data, type, '.'].

np([mode:M, num:N, type:attribute, pos:subj, dt:DT, sem:Sem]) -->
   noun([mode:M, num:N, type:attribute, pos:subj, dt:DT, sem:Sem]).

noun([mode:proc, num:N, type:attribute, pos:subj, dt:DT, sem:Sem])-->     % NOW WE HAVE n AND NOT noun AS PREDICATE NAME
   lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, pos:subj, dt:DT, arg:_X, sem:Sem]).
   
noun([mode:gen, num:N, type:attribute, pos:subj, dt:DT, sem:Sem])-->
   { lexicon([cat:noun, wform:WForm, num:N, type:attribute, pos:subj, dt:DT, arg:_X, sem:Sem]) },
   WForm. 

%---------------------------------------------------------------------------------------------

lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, pos:subj, dt:DT, arg:_X, sem:Sem], L1, L2):-
   process_attribute([wform:WForm, dt:DT, arg:X, sem:Sem], L1, L2), downcase_list(WForm, DWForm),
   assert(lexicon([cat:noun, wform:DWForm, num:sg, type:attribute, pos:obj, dt:DT, arg:X, sem:Sem])),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem])).
   
process_attribute([wform:List3, dt:D, arg:X, sem:Sem], List1, List2):-
   (D = integer; D = string; D = date; D = boolean),
   append(List3, [is, of, D, data, type, '.'], List1),
   List2 = [is, of, D, data, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5), V = ['_type'=D, '__text'=X],
   Sem =json(['Rel'=attribute, 'Ind'=List5, 'Var'=json(V)]).


test1 :-
    s([mode:proc, type:attribute, sem:PrologTerm],
      ['Student', name, is, of, string, data, type, '.'], []),
	prolog_vars_to_json_vars(PrologTerm, JSONTerm),
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    write(JSON).
	
test2 :-
	JSON = '{
			  "Atom": {
				"Rel":"attribute",
				"Ind":"student_name",
				"Var": {"_type":"string", "__text":"V1"} 
			  }
			}',
	atom_json_term(JSON, JSONTerm, [as(atom)]),
	json_vars_to_prolog_vars(JSONTerm, PrologVars),
    s([mode:gen, type:attribute, sem:PrologVars], S, []),!, 
	writeq(S), nl, nl.
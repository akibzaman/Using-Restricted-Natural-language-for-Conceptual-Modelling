:- use_module(library(http/json)).
:- style_check([-discontiguous, -singleton]).
:- consult(predicates).
   
s([type:const, sem:Sem]) -->               
  np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]),
  vp([num:N, type:const, arg:X, sem:S]), ['.'].
    
np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]) -->                 
  qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:Sem]),
  noun([num:N, type:const, pos:subj, arg:X, sem:R]).                          
    
np([num:_N, type:const, pos:obj, arg:X, sem:json(['Atom'=R])]) -->
  noun([num:N, type:const, pos:obj, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:Sem]) -->
  kvp([wfrm:[is, a, subtype, of]]),
  np([num:_N, type:const, pos:obj, arg:X, sem:Sem]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 
  { lexicon([cat:qnt, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.
	
noun([num:N, type:const, pos:subj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem]) }, WForm.
  
noun([num:N, type:const, pos:obj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) }, WForm.

verb([num:N, type:const, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:WForm, num:N, arg:X, arg:Y, sem:Sem]) }, WForm.


lexicon([cat:qnt, wform:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

				   
kvp([wfrm:[is, a, subtype, of]]) --> [is, a, subtype, of].									   

lexicon([cat:noun, wform:[student], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]). 

lexicon([cat:noun, wform:[graduate, student], num:sg, type:entity, pos:subj, init:no,arg:X, sem:json(['Rel'=entity, 'Ind'=graduate_student, 'Var'=X])]).

lexicon([cat:verb, wform:[is, a, subtype, of], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=subtype_of, 'Var'=[X, Y]]) ]). 


test1 :-
    s([type:const, sem:PrologTerm],
      ['Every', graduate, student, is, a, subtype, of, student, '.'], []),
    prolog_vars_to_json_vars(PrologTerm, JsonTerm),
    atom_json_term(Json, JsonTerm, [as(atom)]),
    write(Json).
	
test2 :-							   
		JSON = ' {
				  "Forall": {
					"Var":"V1",
					"Implies": {
					  "head": {
						"Atom": {"Rel":"entity", "Ind":"graduate_student", "Var":"V1"}
					  },
					  "body": {"Atom": {"Rel":"entity", "Ind":"student", "Var":"V1"}}
					}
				  }
				}',
  atom_json_term(JSON, JSONTerm, [as(atom)]),
  json_vars_to_prolog_vars(JSONTerm, PrologVars),
  s([type:const, sem:PrologVars], Sentence, []),!,
  nl, nl,
  writeq(Sentence).


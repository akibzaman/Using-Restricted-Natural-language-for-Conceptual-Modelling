:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).  
:- consult(predicates). 


s([type:fact_ob, sem:json(['Atom'=[Res, Sco]])]) -->  
  np([num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  verb([wform:[associates], num:N, type:fact_ob, arg:X, arg:L, sem:Sco]),
  ['"'], s([type:ob_bfact, sem:L]),['"'],
  ['.'].
  
np([num:N, type:T, pos:P, arg:X, sem:S]) -->
   noun([num:N, type:T, pos:P, arg:X, sem:S]).

noun([num:N, type:T, pos:subj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:yes, arg:X, sem:Sem]) },
  WForm.

noun([num:N, type:T, pos:obj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) },
  WForm.

verb([wform:[associates], num:N, type:fact_ob, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, arg:X, arg:Y, sem:Sem]) },
  [associates].


%-------------------------------------------------------------------

s([type:ob_bfact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  vp([num:N, type:ob_bfact, arg:X, sem:Sco]).
  
vp([num:N, type:ob_bfact, arg:X, sem:[P, O]])-->                   
  verb([num:N, type:ob_bfact, arg:X, arg:Y, sem:P]),
  np([num:_N, type:ob_bfact, pos:obj, arg:Y, sem:O]).

verb([num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem])-->
  {lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:Sem])}, WForm.

%--------------------------------------------------


lexicon([cat:noun, wform:['Enrolment'], num:sg, type:entity, pos:subj, init:yes, arg:X, sem:json(['Rel'=entity, 'Ind'=enrolment, 'Var'=X])]).
lexicon([cat:noun, wform:['Student'], num:sg, type:entity, pos:subj, init:yes, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
lexicon([cat:noun, wform:['program'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]).  
lexicon([cat:verb, wform:[is, enrolled, in], num:sg, type:brel, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=enrolled_in, 'Var'=[X,Y]])]).

lexicon([cat:verb, wform:[associates], num:sg, type:ob_rel, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=associates, 'Var'=X, 'Reify'=Y])]).


%------------------------------------------------

test1 :-
   s([type:fact_ob, sem:PrologTerm], ['Enrolment', associates, '"', 'Student', is, enrolled, in, program,'"', '.'], []),
   prolog_vars_to_json_vars(PrologTerm, JsonTerm), 
   atom_json_term(JSON, JsonTerm, [as(atom)]),
   write(JSON).

test2 :-
    JSON = '{
			  "Atom": [
				{"Rel":"entity", "Ind":"enrolment", "Var":"V1"},
				{
				  "Rel":"relation",
				  "Ind":"associates",
				  "Var":"V1",
				  "Reify": {
					"And": {
					  "Atom": [
						{"Rel":"entity", "Ind":"student", "Var":"V2"},
						{
						  "Rel":"relation",
						  "Ind":"enrolled_in",
						  "Var": ["V2", "V3" ]
						},
						{"Rel":"entity", "Ind":"program", "Var":"V3"}
					  ]
					}
				  }
				}
			  ]
			}',
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    json_vars_to_prolog_vars(JSONTerm, PrologVars),
    s([type:fact_ob, sem:PrologVars], S, []),!,
    writeq(S),
    nl, nl.  

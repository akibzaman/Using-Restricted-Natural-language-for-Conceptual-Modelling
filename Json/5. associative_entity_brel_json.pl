% -----------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#').  
:- consult(p_to_j). 


s([mode:M, type:fact_ob, sem:Sem]) -->  
  np([mode:M, num:N, type:fact, pos:subj, arg:X, sem:S]), 
  verb([mode:M, wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:json(['And'=json(['Atom'=L])]), sem:Sem]),
  ['"'], s([mode:M, type:ob_bfact, sem:L]),['"'],['.'].
  
np([mode:M, num:N, type:T, pos:P, arg:X, sem:S]) -->
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:S]).

noun([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) },
  WForm.

verb([mode:M, wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, sub:S, arg:X, arg:Y, sem:Sem]) },
  [associates].

%-------------------------------------------------------------------------------

s([mode:M, type:ob_bfact, sem:Sem]) --> 
  np([mode:M, num:N, type:ob_bfact, func:subj, arg:X, sco:Sem, sem:S]), 
  vp([mode:M, num:N, type:ob_bfact, arg:X, sub:S, sem:Sem]).

vp([mode:M, num:N, type:ob_bfact, arg:X, sub:S, sem:[S,Sem,O]])-->
  verb([mode:M, num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem]),
  np([mode:M, num:_N, type:ob_bfact, func:obj, arg:Y, sem:O]).
  
np([mode:proc, num:N, type:ob_bfact, func:subj, arg:X, sco:Sem, sem:S]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, arg:X, sem:S]) },
  WForm.

np([mode:proc, num:N, type:ob_bfact, func:obj, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:Y, sem:Sem]) }, 
  WForm.
  
np([mode:gen, num:N, type:ob_bfact, func:subj, arg:X, sco:[S,Sem,O], sem:S]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, arg:X, sem:S]) },
  WForm.
  
np([mode:gen, num:N, type:ob_bfact, func:obj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem])},
  WForm.
  
%verb([mode:proc, num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem]) -->
%  {lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:Sem])}.

verb([mode:M, num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem])-->
  {lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:Sem])}, WForm.

%--------------------------------------------------


lexicon([cat:noun, wform:['Enrolment'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=enrolment, 'Var'=X])]).
lexicon([cat:noun, wform:['Student'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
lexicon([cat:noun, wform:['program'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]).  
lexicon([cat:verb, wform:[is, enrolled, in], num:sg, type:brel, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=enrolled_in, 'Var'=[X,Y]])]).
lexicon([cat:verb, wform:[associates], num:sg, type:ob_rel, sub:S, arg:X, arg:Y, sem:json(['Atom'=[S,json(['Rel'=relation, 'Ind'=associates, 'Var'=X, 'Reify'=Y])]])]).


%------------------------------------------------

test1 :-
   s([mode:proc, type:fact_ob, sem:PrologTerm], ['Enrolment', associates, '"', 'Student', is, enrolled, in, program,'"', '.'], []),
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
    s([mode:gen, type:fact_ob, sem:JSONTerm], S, []),
    writeq(S),
    nl, nl.  

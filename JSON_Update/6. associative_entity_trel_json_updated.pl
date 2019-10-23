
:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
  
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- consult(predicates). 


s([type:fact_ob, sem:Sem]) -->  
  np([num:N, type:fact, pos:subj, arg:X, sem:S]), 
  verb([wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:L, sem:Sem]),
  ['"'], s([type:ob_tfact, rel:R, sem:L]),['"'],['.'].
  
np([num:N, type:T, pos:P, arg:X, sem:S]) -->
   noun([num:N, type:T, pos:P, arg:X, sem:S]).

noun([num:N, type:T, pos:P, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) },
  WForm.

verb([wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, sub:S, arg:X, arg:Y, sem:Sem]) },
  [associates].

%-------------------------------------------------------------------------------

s([type:ob_tfact, rel:R, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([num:N, type:ob_tfact, pos:subj, arg:X, sem:Res]),  
  vp([num:N, type:ob_tfact, rel:R, arg:X, arg:Y, arg:Z, sem:Sco]).

vp([num:N, type:ob_tfact, rel:R, arg:X, arg:Y, arg:Z, sem:[V,Ob1,Ob2]])-->
  v([type:ob_tfact, slot:WForm2, pos:beg, arg:X, arg:Y, arg:Z, sem:V]),
  np([num:N, type:ob_tfact, pos:obj, arg:Y, sem:Ob1]),
  v([type:ob_tfact, slot:WForm2, pos:end, arg:X, arg:Y, arg:Z, sem:V]),
  np([num:N, type:ob_tfact, pos:obj, arg:Z, sem:Ob2]).
  
v([type:ob_tfact, slot:WForm2, pos:beg, arg:X, arg:Y, arg:Z, sem:V])-->
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:trel, arg:X, label:_List1, arg:Y, label:_List2, arg:Z, sem:V]) }, WForm1.
   
v([type:ob_tfact, slot:WForm2, pos:end, arg:X, arg:Y, arg:Z, sem:V])--> WForm2.

%--------------------------------------------------


lexicon([cat:noun, wform:['Enrolment'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=enrolment, 'Var'=X])]).
lexicon([cat:noun, wform:['Student'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
lexicon([cat:noun, wform:['unit'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=unit, 'Var'=X])]).
lexicon([cat:noun, wform:['program'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]).  
lexicon([cat:verb, wform:[[enrolled, in], [studies]], num:sg, type:trel, arg:X, label:enrol_in, arg:Y, label:study, arg:Z, 
         sem:json(['Rel'=relation, 'Ind'=enrol_in__study, 'Var'=[X,Y,Z]])]).
lexicon([cat:verb, wform:[associates], num:sg, type:ob_rel, sub:S, arg:X, arg:Y, sem:json(['Atom'=[S,json(['Rel'=relation, 'Ind'=associates, 'Var'=X, 'Reify'=Y])]])]).


%------------------------------------------------

test1 :-
   s([type:fact_ob, sem:PrologTerm], ['Enrolment', associates, '"', 'Student', enrolled, in, program, studies, unit,'"', '.'], []),
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
						  "Ind":"enrol_in__study",
						  "Var": ["V2", "V3", "V4" ]
						},
						{"Rel":"entity", "Ind":"program", "Var":"V3"},
						{"Rel":"entity", "Ind":"unit", "Var":"V4"}
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

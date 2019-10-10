% -----------------------------------------------------------------------
:- use_module(library(http/json)).
:- style_check([-discontiguous, -singleton]).
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#').   
:- consult(p_to_j).

s([mode:M, type:fact_ob, sem:Sem]) -->  
  np([mode:M, num:N, type:fact, pos:subj, arg:X, sco:Sem, sem:S]),
  verb([mode:M, wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:json(['And'=json(['Atom'=L])]), sem:Sem]),
  ['"'], s([mode:M, type:ob_tfact, rel:R, sem:L]),['"'],['.'].
  
np([mode:M, num:N, type:T, pos:P, arg:X, sco:Sem, sem:S]) -->
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:S]).
   
np([mode:M, num:N, type:T, pos:P, arg:X, sem:S]) -->
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:S]).

noun([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) },
  WForm.

%noun([mode:gen, num:N, type:T, pos:P, arg:X, sem:Sem]) -->
%  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) },
%  WForm.

verb([mode:M, wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, sub:S, arg:X, arg:Y, sem:Sem]) },
  [associates].

%verb([mode:gen, wform:[associates], num:N, type:fact_ob, sub:S, arg:X, arg:Y,  sem:Sem]) -->
%  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, sub:S, arg:X, arg:Y, sem:Sem]) },
%  [associates].

%-------------------------------------------------------------------------------

s([mode:M, type:ob_tfact, rel:R, sem:L]) --> 
  np([mode:M, num:N, type:ob_tfact, pos:subj, arg:X, sco:L, sem:S]), 
  vp([mode:M, num:N, type:ob_tfact, rel:R, sub:S, arg:X, arg:Y, arg:Z, sem:L]).

vp([mode:M, num:N, type:ob_tfact, rel:R, sub:S, arg:X, arg:Y, arg:Z, sem:[S,V,Ob1,Ob2]])-->
  v([mode:M, type:ob_tfact, slot:WForm2, pos:beg, arg:X, arg:Y, arg:Z, sem:V]),
  np([mode:M, num:N, type:ob_tfact, pos:obj, arg:Y, sem:Ob1]),
  v([mode:M, type:ob_tfact, slot:WForm2, pos:end, arg:X, arg:Y, arg:Z, sem:V]),
  np([mode:M, num:N, type:ob_tfact, pos:obj, arg:Z, sem:Ob2]).
  
v([mode:M, type:ob_tfact, slot:WForm2, pos:beg, arg:X, arg:Y, arg:Z, sem:V])-->
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:trel, arg:X, label:_List1, arg:Y, label:_List2, arg:Z, sem:V]) }, WForm1.
   
v([mode:M, type:ob_tfact, slot:WForm2, pos:end, arg:X, arg:Y, arg:Z, sem:V])--> 
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:trel, arg:X, label:_List1, arg:Y, label:_List2, arg:Z, sem:V]) }, WForm2.

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
   s([mode:proc, type:fact_ob, sem:PrologTerm], ['Enrolment', associates, '"', 'Student', enrolled, in, program, studies, unit,'"', '.'], []),
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
	%json_vars_to_prolog_vars(JSONTerm, PrologTerm),
	%JSONTerm = json(['And'=json(['Atom'=Sem])]),
    s([mode:gen, type:fact_ob, sem:JSONTerm], S, []),
    writeq(S),
    nl, nl.  

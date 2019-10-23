:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)). 
  
% :- op(800, yfx, '&').
% :- op(900, yfx, ':').

:- dynamic lexicon/1.
:- consult(predicates).

%--------------------------------------------------------
% Fact Type Declaration:- Student is enrolled in program.
%--------------------------------------------------------

s([mode:M, type:fact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([mode:M, num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  vp([mode:M, num:N, type:fact, arg:X, sem:Sco]),
  ['.'].

np([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]) -->                 
  noun([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]). 

vp([mode:M, num:N, type:fact, arg:X, sem:[P, O]])-->                   
  verb([mode:M, num:N, type:fact, arg:X, arg:Y, sem:P]),
  np([mode:M, num:_N, type:fact, pos:obj, arg:Y, sem:O]).
  
noun([mode:M, num:N, type:fact, pos:obj, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:Y, sem:Sem]) }, 
  WForm.

noun([mode:M, num:N, type:fact, pos:subj, arg:X,  sem:Sem]) -->       % FIXED
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, arg:X, sem:Sem]) },
  WForm.
 
verb([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem]) -->
  lexical_rule([cat:verb, num:N, arg:X, arg:Y, sem:Sem]).

verb([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
  { lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:Sem]) }, WForm.

%------------------------------------------------------------------------------

lexical_rule([cat:verb, num:N, arg:X, arg:Y, sem:B], P1, P2) :-
  generate_sem(WForm, P1, P2, X, Y, B),
  (
  assert(lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:B]))
  ;
  retract(lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:B]))
  ).
  

generate_sem(WForm, P1, P2, X, Y, B) :-
   append(WForm, P2, P1),
   WForm \= [], 
   morphology_rel(WForm, WF),
   atomic_list_concat(WF,'_', Term),
   B = json(['Rel'=relation, 'Ind'=Term, 'Var'=[X, Y]]).

morphology_rel([L|L1], L2):-
	((L == is) -> L2 = L1) ; 
	((sub_atom(L,Q,3,0,ies) -> sub_atom(L,_,Q,3,L0), atom_concat(L0,y,L3), L2 = [L3|L1]));
	((sub_atom(L,Q,2,0,ys) -> sub_atom(L,_,Q,2,L0), atom_concat(L0,y,L3), L2 = [L3|L1]));	
	((porter_stem(L,L3), L2 = [L3|L1])).


reverse([])     --> [].
reverse([L|Ls]) --> reverse(Ls), [L].

%------------------------------------------------------------------------------
% to do remove arg:X
  
lexicon([cat:noun, wform:['Student'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
lexicon([cat:noun, wform:['Department'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]).
lexicon([cat:noun, wform:['program'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]).   
   
test1 :-
    s([mode:proc, type:fact, sem:PrologTerm],['Student', is, enrolled, in, program, '.'], []),!,
	prolog_vars_to_json_vars(PrologTerm, JsonTerm),
    atom_json_term(JSON, JsonTerm, [as(atom)]),
    write(JSON).
 
test2 :-
     JSON = '{"And": {
				"Atom": [
				  {"Rel":"entity", "Ind":"student", "Var":"X"},
				  {"Rel":"relation", "Ind":"enrolled_in", "Var": ["X", "Y" ]},
				  {"Rel":"entity", "Ind":"program", "Var":"Y"}
				]
			  }
			}',
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    json_vars_to_prolog_vars(JSONTerm, PrologVars),
    s([mode:gen, type:fact, sem:PrologVars], S, []),!,
    writeq(S),
    nl, nl.  

:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:- dynamic lexicon/1.
:- consult(predicates). 
    
%----------------------------------------------------------------------
% Fact Type Declaration:- Student is enrolled in program studies units.
%----------------------------------------------------------------------

s([mode:M, type:tfact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([mode:M, num:N, type:tfact, pos:subj, arg:X, sem:Res]), 
  vp([mode:M, num:N, type:tfact, arg:X, sem:Sco]), ['.'].

np([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]) -->                
  noun([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]). 

np1([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]) -->                
  noun([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]). 

np2([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]) -->                
  noun([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]). 

vp([mode:proc, num:N, type:tfact, arg:X, sem:[V, Ob1, Ob2]])-->
  verb([mode:proc, wform:WForm1, type:tfact]),
  np1([mode:proc, num:N, type:tfact, pos:obj, arg:Y, sem:Ob1]),
  verb([mode:proc, wform:WForm2, type:tfact]),
  np2([mode:proc, num:N, type:tfact, pos:obj, arg:Z, sem:Ob2]),
  lexical_rule([mode:proc, wform:[WForm1, WForm2], num:N, type:tfact, arg:X, arg:Y, arg:Z, sem:V]).
  
vp([mode:gen, num:N, type:tfact, arg:X, sem:[V, Ob1, Ob2]])-->
  verb([mode:gen, wform:[WForm1, WForm2], type:tfact, arg:X, arg:Y, arg:Z, sem:V]),
  np([mode:gen, num:N, type:tfact, pos:obj, arg:Y, sem:Ob1]),
  verb([mode:gen, wform:[WForm1, WForm2], type:tfact, sem:_V]),
  np([mode:gen, num:N, type:tfact, pos:obj, arg:Z, sem:Ob2]).

noun([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) }, WForm.
  
verb([mode:proc, wform:List3, type:tfact], List1, List2) :-
   append(List3, List2, List1). 

verb([mode:gen, wform:[WForm1, WForm2], type:tfact, arg:X, arg:Y, arg:Z, sem:V]) -->
   { lexicon([cat:verb, wform:[WForm1, WForm2], num:sg, type:trel, arg:X, arg:Y, arg:Z, sem:V]) }, 
   WForm1.

verb([mode:gen, wform:[WForm1, WForm2], type:tfact, sem:_V]) -->
   { lexicon([cat:verb, wform:[WForm1, WForm2], num:sg, type:trel, arg:_X, arg:_Y, arg:Z, sem:_V]) }, 
   WForm2.

   
%------------------------------------------------------------------------------

lexical_rule([mode:proc, wform:[WForm1, WForm2], num:N, type:tfact, arg:X, arg:Y, arg:Z, sem:Sem], P1, P1):-
   morphology_rel(WForm1, L1), atomic_list_concat(L1, '_', L2), 
   morphology_rel(WForm2, L3), atomic_list_concat(L3, '_', L4),  
   atomic_list_concat([L2,'__',L4], L), V = [X,Y,Z],
   Sem = json(['Rel'=relation, 'Ind'=L, 'Var'=V]),
   assert(lexicon([cat:verb, wform:[WForm1, WForm2], num:sg, type:trel, arg:X, arg:Y, arg:Z, sem:Sem])).

lower_case(Noun, NP):-
   lower_case_first_atom(Noun, NP).
   
uppercase_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_upper(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).
   
morphology_rel([L|L1], L2):-
	((L == is) -> L2 = L1) ; 
	((sub_atom(L,Q,3,0,ies) -> sub_atom(L,_,Q,3,L0), atom_concat(L0,y,L3), L2 = [L3|L1])); 
	((porter_stem(L,L3), L2 = [L3|L1])).

%----------------------------------------------------------------------------------------

lexicon([cat:noun, wform:['Student'], num:sg, type:entity, pos:subj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
lexicon([cat:noun, wform:['unit'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=unit, 'Var'=X])]).
lexicon([cat:noun, wform:['program'], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]).   
	   
%----------------------------------------------------------------------------------------  

test1 :-
    s([mode:proc, type:tfact, sem:PrologTerm],['Student',enrolled,in,program,studies,unit,'.'], []),
    prolog_vars_to_json_vars(PrologTerm, JSONTerm), 
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    write(JSON).
	
test2 :-
    JSON = '{
			  "And": {
				"Atom": [
				  {"Rel":"entity", "Ind":"student", "Var":"V1"},
				  {
					"Rel":"relation",
					"Ind":"enrol_in__study",
					"Var": ["V1", "V2", "V3" ]
				  },
				  {"Rel":"entity", "Ind":"program", "Var":"V2"},
				  {"Rel":"entity", "Ind":"unit", "Var":"V3"}
				]
			  }
			}',
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    json_vars_to_prolog_vars(JSONTerm, PrologVars),
    s([mode:gen, type:tfact, sem:PrologVars], S, []),!,
    writeq(S),
    nl, nl.  

:- style_check([-discontiguous, -singleton]).  
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(900, yfx, '^').

:- dynamic lexicon/1.
:- consult(lex).
	
%--------------------------------------------------------
% Fact Type Declaration:- Student is enrolled in program.
%--------------------------------------------------------

s([mode:M, type:fact, sem:Sem]) --> 
  np([mode:M, num:N, type:fact, func:subj, arg:X, sem:Sem]), 
  vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]), ['.'].
  
vp([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:[T1|T2]-[[T|T1]|T2]])-->
  v([mode:proc, type:fact, label:List]),
  np([mode:proc, num:N, type:fact, func:obj, arg:Y]),
  lexical_rule([cat:verb, wform:WForm, type:fact, arg:X, arg:Y, label:List, sem:T]).
  
vp([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:[[T|T1]|T2]-[T1|T2]])-->
  v([mode:gen, type:fact, sem:relation(X,Y,Z)]),
  np([mode:gen, num:N, type:fact, func:obj, arg:X, sem:[[relation(X,Y,Z)|A]|B]-[A|B]]).

np([mode:proc, num:N, type:fact, func:subj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:X]) }, WForm.
  
np([mode:proc, num:N, type:fact, func:obj, arg:X]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:X]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:subj, arg:X, sem:[[relation(X,Y,Z)|A]|B]-[A|B]]) -->
  {lexicon([cat:noun, wform:WForm, num:sg, type:entity, arg:_X, sem:X]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:obj, arg:X, sem:[[relation(X,Y,Z)|A]|B]-[A|B]]) -->
  {lexicon([cat:noun, wform:WForm, num:sg, type:const, arg:_X, sem:Y])}, WForm.
  
v([mode:proc, type:fact, label:List3], List1, L) :-
  append(List3, List2, List1), uppercase_first_atom(List2, L).
   
v([mode:gen, type:fact, sem:Sem])-->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg:X, arg:Y, sem:Sem]) }, WForm.

%------------------------------------------------------------------------------

lexical_rule([cat:verb, wform:WForm, type:fact, arg:X, arg:Y, label:List, sem:Sem], P1, P1):-
   atomic_list_concat(List, '_', L), Sem =.. [relation, X, Y, L], 
   assert(lexicon([cat:verb, wform:List, num:sg, type:brel, arg:X, arg:Y, sem:Sem])).
   
rel_lookup(W, T, R):-
	lexicon([cat:verb, wform:Wform, num:sg, type:brel, arg:X, arg:Y, sem:T]),
	(W=subj -> R = X ; R = Y).
   
lower_case(Noun, NP):-
   lower_case_first_atom(Noun, NP).
  
  
uppercase_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_upper(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).
   
test3 :-
    s([mode:proc, type:fact, sem:[[]]-L],['Student',is,enrolled,in,program, '.'], []),
    numbervars(L), write(L), nl, nl.
 
test4 :-
    Sem = [[relation(entity(A,student),entity(B,program),is_enrolled_in)|A]|B]-[A|B],
    s([mode:gen, type:fact, sem:Sem], S, []),  append(S1,['.'], S), atomic_list_concat(S1,' ',S2),
    atom_concat(S2,'.', Sen), write(Sen), nl, nl.

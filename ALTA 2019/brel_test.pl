:- style_check([-discontiguous, -singleton]).  
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#').    % infix operator '#' added

:- dynamic lexicon/1.
%:- consult(lex).
	
%--------------------------------------------------------
% Fact Type Declaration:- Student is enrolled in program.
%--------------------------------------------------------

s([mode:M, type:fact, sem:L1-L3]) --> 
  np([mode:M, num:N, type:fact, func:subj, arg:X, sem:L1-L2]), 
  vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:L2-L3]), ['.'].
  
vp([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:[T1|T2]-[[T|T1]|T2]])-->
  v([mode:proc, type:fact, label:List]),
  np([mode:proc, num:N, type:fact, func:obj, arg:Y]),
  lexical_rule([cat:verb, wform:WForm, type:fact, arg:X, arg:Y, label:List, sem:T]).
  
vp([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:[[T|T1]|T2]-[T1|T2]])-->
  v([mode:gen, type:fact, arg:X, arg:Y, sem:T]),
  np([mode:gen, num:N, type:fact, func:obj, arg:Y]).

np([mode:proc, num:N, type:fact, func:subj, arg:X#T, sem:L1-L1]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, position:beg, arg:X#T, sem:Sem]) }, WForm.
  
np([mode:proc, num:N, type:fact, func:obj, arg:X#T]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, position:mid, arg:X#T, sem:L]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:subj, arg:X#T, sem:L1-L1]) -->
  {lexicon([cat:noun, wform:WForm, num:sg, type:entity, position:beg, arg:X#T, sem:Sem]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:obj, arg:X#T]) -->
  {lexicon([cat:noun, wform:WForm, num:sg, type:entity, position:mid, arg:X#T, sem:Sem])}, WForm.
  
v([mode:proc, type:fact, label:List3], List1, List2) :-
  append(List3, List2, List1).
   
v([mode:gen, type:fact, arg:X, arg:Y, sem:T])-->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg:X, arg:Y, sem:T]) }, WForm.

%------------------------------------------------------------------------------

lexical_rule([cat:verb, wform:WForm, type:fact, arg:X#T1, arg:Y#T2, label:List, sem:Sem], P1, P1):-
   atomic_list_concat(List, '_', L), Sem =.. [relation, X, Y, L], 
   assert(lexicon([cat:verb, wform:List, num:sg, type:brel, arg:X#T1, arg:X#T2, sem:Sem])).
   
   
lower_case(Noun, NP):-
   lower_case_first_atom(Noun, NP).

lower_case_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_lower(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).  
  
uppercase_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_upper(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).
   
lexicon([cat:noun, wform:['Student'], num:N, type:entity, position:beg, arg:X#student, sem:student(X)]).    % X#student
lexicon([cat:noun, wform:['program'], num:N, type:entity, position:mid, arg:X#program, sem:program(X)]).    % X#unit
lexicon([cat:noun, wform:['Program'], num:N, type:entity, position:beg, arg:X#program, sem:program(X)]).
   
test3 :-
    s([mode:proc, type:fact, sem:[[]]-L],['Student',is,enrolled,in,program, '.'], []),
    numbervars(L), write(L), nl, nl.
 
test4 :-
    Sem = [[relation(A,B,is_enrolled_in)|X]|Y]-[X|Y],
    s([mode:gen, type:fact, sem:Sem], S, []),  append(S1,['.'], S), atomic_list_concat(S1,' ',S2),
    atom_concat(S2,'.', Sen), write(Sen), nl, nl.

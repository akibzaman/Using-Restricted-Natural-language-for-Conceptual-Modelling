:- style_check([-discontiguous, -singleton]).
:- consult(lexicon).   
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').


%-------------------------------------------------------------------------------
% Fact Type "Objectification"
% Enrolment objectifies "student is enrolled in program".
%-------------------------------------------------------------------------------

s([mode:proc, type:fact_ob, sem:[T1|T2]-[[objectify(S,T)|T1]|T2]]) -->  
  np([mode:proc, num:N, type:fact_ob, func:subj, arg:X, sem:S]),
  vp([wform:objectify]), ['"'], s([mode:proc, type:ob_fact, sem:[A|B]-[[T|A]|B]]), ['"'], ['.'],
  {assert(lexicon([cat:verb, wform:[objectifies], num:sg, type:ob_rel, arg1:S, arg2:T, sem:objectify(S,T)]))}.
  
s([mode:gen, type:fact_ob, sem:[[objectify(S,T)|T1]|T2]-[T1|T2]]) --> 
  np([mode:gen, num:sg, type:fact_ob, func:subj, arg:X, sem:S]),
  vp([wform:objectify]), ['"'], s([mode:gen, type:ob_fact, sem:T]), ['"'], ['.']. 

np([mode:proc, num:N, type:fact_ob, func:_, arg:List, sem:S]) -->
  {lexicon([cat:noun, wform:List, num:N, type:entity, arg:_X, sem:S])}, List.
  
np([mode:gen, num:N, type:fact_ob, func:_, arg:List, sem:S]) -->
  {lexicon([cat:noun, wform:List, num:N, type:entity, arg:_X, sem:S])}, List.

s([mode:proc, type:ob_fact, sem:Sem]) --> 
  np([mode:proc, num:N, type:ob_fact, func:subj, arg:X]), 
  vp([mode:proc, num:N, type:ob_fact, arg:X, arg:Y, sem:Sem]).
  
s([mode:gen, type:ob_fact, sem:Sem]) --> 
  np([mode:gen, num:sg, type:ob_fact, func:subj, arg:X, arg:Y, sem:Sem]), 
  vp([mode:gen, num:sg, type:ob_fact, arg:X, arg:Y, sem:[[Sem|T1]|T2]-[T1|T2]]).
  
vp([mode:proc, num:N, type:ob_fact, arg:X, arg:Y, sem:[T1|T2]-[[List|T1]|T2]])-->
  v([mode:proc, type:ob_fact, label:List]),
  np([mode:proc, num:N, type:ob_fact, func:obj, arg:Y]).
  
vp([mode:gen, num:N, type:ob_fact, arg:X, arg:Y, sem:[[Sem|T1]|T2]-[T1|T2]])-->
  v([mode:gen, type:ob_fact, sem:Sem]),
  np([mode:gen, num:N, type:ob_fact, func:obj, sem:Y]).

np([mode:proc, num:N, type:ob_fact, func:_, arg:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) , lower_case(WForm, WForm1)}, WForm1.
  
np([mode:gen, num:N, type:ob_fact, func:subj, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg:X, arg:Y, sem:Sem]) },
  { lexicon([cat:noun, wform:Noun, num:N, type:entity, arg:_X, sem:X]), lower_case(Noun, NP) }, NP.
  
np([mode:gen, num:N, type:ob_fact, func:obj, sem:Y]) -->
  { lexicon([cat:noun, wform:Noun, num:N, type:entity, arg:_X, sem:Y]), lower_case(Noun, NP) }, NP.
  
v([mode:proc, type:ob_fact, label:Sem]) -->
  {lexicon([cat:verb, wform:List3, num:sg, type:brel, arg:X, arg:Y, sem:Sem])}, List3.
   
v([mode:gen, type:ob_fact, sem:Sem])-->
   { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg:X, arg:Y, sem:Sem]) }, WForm.
   
vp([wform:objectify]) --> [objectifies].

%-----------------------------------------------

lower_case(Noun, NP):-
   lower_case_first_atom(Noun, NP).
   
lower_case_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_lower(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).

%------------------------------------------------

test1 :-
s([mode:proc, type:fact_ob, sem:[[]]-L],['Enrollment',objectifies,'"',student,is,enrolled,in,program,'"', '.'], []),
numbervars(L), write(L), nl, nl.

test2 :-
Sem = [[objectify(entity(A,enrollment),relation(entity(B,student),entity(C,program),is_enrolled_in))|A]|B]-[A|B],
s([mode:gen, type:fact_ob, sem:Sem], S, []),  append(S1,['.'], S), atomic_list_concat(S1,' ',S2),
atom_concat(S2,'.', Sen), write(Sen), nl, nl.

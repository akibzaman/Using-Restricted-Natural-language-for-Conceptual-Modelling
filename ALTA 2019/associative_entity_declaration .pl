% -----------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#').   


%-------------------------------------------------------------------------------
% Fact Type "Objectification"
% Input:   Enrolment objectifies "student is enrolled in program".
% Output:  [entity(A,enrollment),objectify(A,B),entity(C,student),B#relation(C,D,is_enrolled_in),entity(D,program)]
%-------------------------------------------------------------------------------

s([mode:M, type:fact_ob, sem:L1-L4]) -->  
  np([mode:M, num:N, type:fact_ob, pos:subj, arg:X, sem:L1-L2]),
  verb([mode:M, wform:[objectifies], num:N, type:fact_ob, arg:X, arg:R, sem:L2-L3]),
  ['"'],
  s([mode:M, type:ob_fact, rel:R, sem:L3-L4]),
  ['"'],
  ['.'].

np([mode:M, num:N, type:T, pos:P, arg:X, sem:L1-L2]) -->
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:L1-L2]).

noun([mode:proc, num:N, type:fact_ob, pos:P, arg:X, sem:[L1|L2]-[[L0|L1]|L2]]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:L0])},
   WForm.

noun([mode:gen, num:N, type:fact_ob, pos:P, arg:X, sem:[[L0|L1]|L2]-[L1|L2]]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:L0])},
   WForm.

verb([mode:proc, wform:[objectifies], num:N, type:fact_ob, arg:X, arg:Y, sem:[L1|L2]-[[L0|L1]|L2]]) -->
  { lexicon([cat:verb, wform:[objectifies], num:N, type:ob_rel, arg:X, arg:Y, sem:L0]) },
  [objectifies].

verb([mode:gen, wform:[objectifies], num:N, type:fact_ob, arg:X, arg:Y,  sem:[[L0|L1]|L2]-[L1|L2]]) -->
  { lexicon([cat:verb, wform:[objectifies], num:N, type:ob_rel, arg:X, arg:Y, sem:L0]) },
  [objectifies].

%-------------------------------------------------------------------------------

s([mode:M, type:ob_fact, rel:R, sem:L1-L3]) --> 
  np([mode:M, num:N, type:ob_fact, pos:obj, arg:X, sem:L1-L2]), 
  vp([mode:M, num:N, type:ob_fact, rel:R, arg:X, sem:L2-L3]).

vp([mode:M, num:N, type:ob_fact, rel:R, arg:X, sem:L1-L3])-->
  radj([mode:M, num:N, type:ob_fact, rel:R, arg:X, arg:Y, sem:L1-L2]),
  np([mode:M, num:_N, type:ob_fact, pos:obj, arg:Y, sem:L2-L3]).
  
np([mode:M, num:N, type:T, pos:P, arg:X, sem:L1-L2]) -->
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:L1-L2]).

noun([mode:proc, num:N, type:ob_fact, pos:P, arg:X, sem:[L1|L2]-[[L0|L1]|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:L0]) },
  WForm.

noun([mode:gen, num:N, type:ob_fact, pos:P, arg:X, sem:[[L0|L1]|L2]-[L1|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:L0]) },
  WForm.

/*
np([mode:proc, num:N, type:ob_fact, func:subj, arg:X, sem:[L1|L2]-[[L0|L1]|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:mid, arg:X, sem:L0]) },
  WForm.

np([mode:proc, num:N, type:ob_fact, func:obj, arg:X, sem:[L1|L2]-[[L0|L1]|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:mid, arg:X, sem:L0]) }, 
  WForm.
  
np([mode:gen, num:N, type:ob_fact, func:subj, arg:X, sem:[[L0|L1]|L2]-[L1|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:mid, arg:X, sem:L0]) },
  WForm.
  
np([mode:gen, num:N, type:ob_fact, func:obj, arg:X, sem:[[L0|L1]|L2]-[L1|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:mid, arg:X, sem:L0])},
  WForm.
*/
  
radj([mode:proc, num:N, type:ob_fact, rel:R, arg:X#T1, arg:Y#T2, sem:[L1|L2]-[[R#relation(X, Y, Z)|L1]|L2]]) -->
  { lexicon([cat:radj, wform:WForm, num:N, type:brel, arg:X#T1, arg:Y#T2, sem:relation(X, Y, Z)]) }, WForm.

radj([mode:gen, num:N, type:ob_fact, rel:R, arg:X#T1, arg:Y#T2, sem:[[R#relation(X, Y, E)|L1]|L2]-[L1|L2]])-->
  { lexicon([cat:radj, wform:WForm, num:N, type:brel, arg:X#T1, arg:Y#T2, sem:relation(X, Y, E)]) }, WForm.


lexicon([cat:noun, wform:['Enrolment'], num:sg, type:entity, pos:subj, arg:X#enrollment, sem:entity(X, enrolment)]).

lexicon([cat:noun, wform:[student], num:sg, type:entity, pos:obj, arg:X#student, sem:entity(X, student)]).

lexicon([cat:noun, wform:[program], num:sg, type:entity, pos:obj, arg:X#program, sem:entity(X, program)]).

lexicon([cat:radj, wform:[is, enrolled, in], num:sg, type:brel, arg:X#student, arg:Y#program, sem:relation(X, Y, is_enrolled_in)]).

lexicon([cat:verb, wform:[objectifies], num:sg, type:ob_rel, arg:X#T, arg:Y, sem:objectify(X, Y)]).


%------------------------------------------------

test1 :-
   s([mode:proc, type:fact_ob, sem:[[]]-L], ['Enrolment', objectifies, '"', student, is, enrolled, in, program, '"', '.'], []),!,
   L = [List],
   reverse(List, RList),
   numbervars(RList),
   write(RList),
   nl, nl.

test2 :-
   L = [entity(A,enrolment),objectify(A,B),entity(C,student),B#relation(C,D,is_enrolled_in),entity(D,program)],
   s([mode:gen, type:fact_ob, sem:[L]-[[]]], S, []),!,
   writeq(S),
   nl, nl.

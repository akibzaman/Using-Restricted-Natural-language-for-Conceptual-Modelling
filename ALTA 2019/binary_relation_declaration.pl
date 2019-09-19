:- style_check([-discontiguous, -singleton]).  
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#').    % infix operator '#' added

:- dynamic lexicon/1.

%--------------------------------------------------------
% Fact Type Declaration:- Student is enrolled in program.
%--------------------------------------------------------

s([mode:M, type:fact, sem:L1-L3]) --> 
  np([mode:M, num:N, type:fact, pos:subj, arg:X, sem:L1-L2]), 
  vp([mode:M, num:N, type:fact, arg:X, sem:L2-L3]),
  ['.'].

vp([mode:M, num:N, type:fact, arg:X, sem:L1-L3])-->
  radj([mode:M, num:N, type:fact, arg:X, arg:Y, sem:L1-L2]),
  np([mode:M, num:_N, type:fact, pos:obj, arg:Y, sem:L2-L3]).
  
np([mode:M, num:N, type:T, pos:P, arg:X, sem:L1-L2]) -->
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:L1-L2]).

noun([mode:proc, num:N, type:fact, pos:P, arg:X, sem:[L1|L2]-[[L0|L1]|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:L0]) },
  WForm.

noun([mode:gen, num:N, type:fact, pos:P, arg:X, sem:[[L0|L1]|L2]-[L1|L2]]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:L0]) },
  WForm.
  
radj([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:[L1|L2]-[[L0|L1]|L2]]) -->
  lexical_rule([cat:radj, num:N, arg:X, arg:Y, sem:L0]).

radj([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:[[L0|L1]|L2]-[L1|L2]])-->
  { lexicon([cat:radj, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:L0]) }, WForm.

%-------------------------------------------------------------------------------------------

lexical_rule([cat:radj, num:N, arg:X#T1, arg:Y#T2, sem:L0], P1, P2) :-
  generate_sem(radj, WForm, P1, P2, X, Y, L0),
  (
    assert(lexicon([cat:radj, wform:WForm, num:N, type:brel, arg:X#T1, arg:X#T2, sem:L0]))
  ;
    retract(lexicon([cat:radj, wform:WForm, num:N, type:brel, arg:X#T1, arg:X#T2, sem:L0]))
  ).

generate_sem(radj, [is|WForm], P1, P2, X, Y, L0) :-
   append([is|WForm], P2, P1),
   atomic_list_concat([is|WForm], '_', Term),
   L0 =.. [relation, X, Y, Term].

%-------------------------------------------------------------------------------------------
  
lexicon([cat:noun, wform:['Student'], num:sg, type:entity, pos:subj, arg:X#student, sem:entity(X, student)]).   
lexicon([cat:noun, wform:['program'], num:sg, type:entity, pos:obj, arg:X#program, sem:entity(X, program)]).   
lexicon([cat:noun, wform:['Program'], num:sg, type:entity, pos:subj, arg:X#program, sem:entity(X, program)]).
   
test1 :-
    s([mode:proc, type:fact, sem:[[]]-[L]],['Student', is, enrolled, in, program, '.'], []),
    !,
    reverse(L, RL),
    numbervars(RL), 
    write(RL), 
    nl, nl.
 
test2 :-
    L = [entity(B, student), relation(B, A, is_enrolled_in), entity(A, program)],
    s([mode:gen, type:fact, sem:[L]-[[]]], S, []),
    writeq(S),
    nl, nl.  

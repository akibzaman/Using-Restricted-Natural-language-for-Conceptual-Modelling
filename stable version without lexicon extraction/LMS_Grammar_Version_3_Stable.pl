% ==============================================
% Bidirectional Grammar without lexicon creation
% Author: Bayzid Ashik Hossain
% ==============================================

:- style_check([-discontiguous, -singleton]).
:- consult(tests).


:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').

%----------------------------------------------------------------------
% Entity Declaration / Class Declaration :- Every student is an entity.
%----------------------------------------------------------------------
s([sem:Sem]) --> np_e([num:N, arg:X, scope:Sco, sem:Sem]), vp_e([num:N, arg:X, scope:Sco]), ['.'].

np_e([num:N, arg:X, scope:Sco, sem:Sem]) --> det([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]), n([num:N, arg:X, sem:Res]).

vp_e([num:N, arg:X, scope:Sco]) --> iv([num:N, arg:X, scope:Sco]).

det([num:sg, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) --> ['Every'].

%det([num:sg, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) --> ['A'].

iv([num:sg, arg:X, scope:entity(X)]) --> [is, an, 'entity'].


%----------------------------------------------
% Disjoint Class :- No student is a program.
%----------------------------------------------

s([sem:Sem]) --> np([num:N, arg:X, scope:Sco, sem:Sem]), vp([num:N, arg:X, scope:Sco]), ['.'].

np([num:N, arg:X, scope:Sco, sem:Sem]) --> 
	det_sd([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]), 
	n([num:N, arg:X, sem:Res]).

vp([num:N, arg:X, scope:Sco]) --> 
	v([num:N, arg:X, scope:Sco]), 
	np_o([num:N, arg:X, scope:Sco]).

np_o([num:N, arg:X, scope:Sco]) --> 
	det_od([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]), 
	n([num:N, arg:X, sem:Sco]).

det_sd([num:N, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> not(Sco))]) --> 
	['No'].

det_od([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) --> 
	[a].

v([num:sg, arg:X, scope:Sco]) --> [is].


%-------------------------------------------------------
% Object Property "Study, composed of, studied by, belongs to" 
% Every student studies some unit. Every program is composed of some units.
%------------------------------------------------------

s([sem:Sem]) -->
   np_s([num:N, arg:X, scope:Sco, sem:Sem]),
   vp_s([num:N, arg:X, scope:Sco]),
   ['.'].
 
np_s([num:N, arg:X, scope:Sco, sem:Sem]) -->
   det_s([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, arg:X, sem:Res]).

np_o([num:N, arg:Y, scope:Sco, sem:Sem]) -->
   det_o([num:N, arg:Y, restrictor:Res, scope:Sco, sem:Sem]),
   n_o([num:N, arg:Y, sem:Res]).

vp_s([num:N, arg:X, scope:Sco]) -->
   v_s([num:N, arg:X, arg:Y, sem:Sem]),
   np_o([num:_, arg:Y, scope:Sem, sem:Sco]).

det_s([num:N, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
   ['Every'].

det_o([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) -->
   [some].

% from formal to RNL
%det_o_m([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) -->[one, or, more]. 


%----------------------------------------------------
% Object Property "Enrol" with exactly cardinality
%----------------------------------------------------


s([sem:Sem]) -->
   np_se([num:N, arg:X, scope:Sco, sem:Sem]),
   vp([num:N, arg:X, scope:Sco]),
   ['.'].
 
np_se([num:N, arg:X, scope:Sco, sem:Sem]) -->
   det([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, arg:X, sem:Res]).

np_oe([num:N, arg:Y, scope:Sco, sem:Sem]) -->
   det([num:N, arg:Y, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, arg:Y, sem:Res]).

vp([num:N, arg:X, scope:Sco]) -->
   v([num:N, arg:X, arg:Y, sem:Sem]),
   np_oe([num:_, arg:Y, scope:Sem, sem:Sco]).

%det([num:N, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
   ['Every'].

det([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco:card(L))]) -->
   [exactly, L].

%det([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco:card(L))]) -->[at, most, L].




%--------------------------------------------------------------------------
% Object Property "Study" with minimum and maximum cardinality constraint
%--------------------------------------------------------------------------


s([sem:Sem]) -->
   np_cs([num:N, arg:X, scope:Sco, sem:Sem]),
   vp_cs([num:N, arg:X, scope:Sco]),
   ['.'].
 
np_cs([num:N, arg:X, scope:Sco, sem:Sem]) -->
   det([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, arg:X, sem:Res]).

np_o([num:N, arg:X, scope:Sco, sem:Sem]) -->
   det_cs([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n_o([num:pl, arg:X, sem:Res]).

vp_cs([num:N, arg:X, scope:Sco]) -->
   v([num:N, arg:X, arg:Y, sem:Sem]),
   np_o([num:N, arg:Y, scope:Sem, sem:Sco]).

%det([num:N, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->['Every'].

det_cs([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & min(L):Sco:max(U))]) -->
   [at, least, L, and, at, most, U]. %, {number(L), number(U), L > 0, U > 0, L < U}.

%positive_number(N):- number(N), N > 0.



%---------------------------------------
% Data Property "Student"
%---------------------------------------

s([sem:Sem]) -->
   np_dp([num:N, arg:X, scope:Sco, sem:Sem]),
   vp([crd:'+', num:N, arg:X, sem:Sco]),
   ['.'].

   
np_dp([num:N, arg:X, scope:Sco, sem:Sem]) -->
   det_s([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:sg, arg:X, sem:Res]). % edited

   
np_odp([num:N, arg:Y, scope:Sco, sem:Sem]) -->
   det_odp([num:N, arg:Y, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, arg:Y, sem:Res]).
   

vp([crd:'+', num:N, arg:X, sem:(Sco1 & Sco2)]) -->   
    vp([crd:'-', num:N, arg:X, sem:Sco1]),
    cc([wfm:[and]]),
    vp([crd:'+', num:N, arg:X, sem:Sco2]).
   

vp([crd:'+', num:N, arg:X, sem:Sco]) -->   
    v([num:N, arg:X, arg:Y, sem:Sem]), 
    np_odp([num:_, arg:Y, scope:Sem, sem:Sco]).


vp([crd:'-', num:N, arg:X, sem:Sco]) -->   
    v([num:N, arg:X, arg:Y, sem:Sem]), 
    np_odp([num:_, arg:Y, scope:Sem, sem:Sco]).


%det_s([num:N, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
%  ['Every'].

det_odp([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) -->[a].

%det_odp([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) -->[an].

cc([wfm:[and]]) --> [and].

%-------------------------------------------------
% Objectification
%-------------------------------------------------

s([sem:Sem]) -->
   np([num:N, arg:X, arg:Y, scope:Sco, sem:Sem]),
   vp([num:N, arg:X, arg:Y, scope:Sco]),
   ['.'].

np([num:N, arg:X, arg:Y, scope:Sco, sem:Sem]) -->   
   det([num:N, arg:X, arg:Y, arg:E, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, arg:E, sem:Res]).

vp([num:N, arg:X, arg:Y, scope:Sem]) -->
   v_ob([num:N, arg:X, arg:Y, restrictor:Res1, restrictor:Res2, scope:Sco, sem:Sem]),
   s_ob([num:N, arg:X, arg:Y, restrictor:Res1, restrictor:Res2, scope:Sem, sem:Sco]).

s_ob([num:N, arg:X, arg:Y, restrictor:Res1, restrictor:Res2, scope:Sem, sem:Sco]) -->
   n([num:N, arg:X, sem:Res1]),
   vp_ob([num:N, arg:X, arg:Y, restrictor:Res2, scope:Sem, sem:Sco]).

vp_ob([num:N, arg:X, arg:Y, restrictor:Res2, scope:Sem, sem:Sco]) -->
   v([num:N, arg:X, arg:Y, scope:Sem, sem:Sco]),
   n([num:N, arg:Y, sem:Res2]).

det([num:N, arg:X, arg:Y, arg:E, restrictor:Res, scope:Sco, sem:forall(E: Sco ==> Res)]) -->
   ['Every'].
   
v_ob([num:N, arg:X, arg:Y, restrictor:Res1, restrictor:Res2, scope:Sco, sem:exists(X, Y, Res1, Res2 & Sco)]) -->
   [objectifies].


%----------------------------------------------
% Lexicon details
%----------------------------------------------

n([num:sg, arg:X, sem:student(X)]) --> [student].

n([num:sg, arg:X, sem:program(X)]) --> [program].

n([num:sg, arg:X, sem:unit(X)]) --> [unit].

n([num:sg, arg:E, sem:enrolment(E)]) --> [enrolment].


n_o([num:pl, arg:X, sem:unit(X)]) --> [units].

n_o([num:pl, arg:X, sem:student(X)]) --> [students].

n_o([num:pl, arg:X, sem:program(X)]) --> [programs].


n([num:sg, arg:X, sem:student_id(X)]) --> [student, id].

n([num:sg, arg:X, sem:student_name(X)]) --> [student, name].

n([num:sg, arg:X, sem:student_address(X)]) --> [student, address].

n([num:sg, arg:X, sem:password(X)]) --> [password].


n([num:sg, arg:X, sem:program_id(X)]) --> [program, id].

n([num:sg, arg:X, sem:program_name(X)]) --> [program, name].

n([num:sg, arg:X, sem:department(X)]) --> [department].

n([num:sg, arg:X, sem:enrolment_date(X)]) --> [enrolment, date].


n([num:sg, arg:X, sem:unit_code(X)]) --> [unit, code].

n([num:sg, arg:X, sem:unit_name(X)]) --> [unit, name].


v([num:sg, arg:X, arg:Y, sem:possess(X, Y)]) --> [possesses].

%v([num:sg, arg:X, arg:Y, sem:possess(X, Y)]) --> [has].

v([num:sg, arg:X, arg:Y, sem:study(X, Y)]) --> [studies].

v([num:pl, arg:X, arg:Y, sem:study(X, Y)]) --> [study].

v([num:_, arg:X, arg:Y, scope:Sem, sem:enrol(X, Y)]) --> [enrolled, in].

v([num:sg, arg:X, arg:Y, sem:enrol(X, Y)]) --> [is, enrolled, in].

v_s([num:sg, arg:X, arg:Y, sem:composed_of(X, Y)]) --> [is, composed, of].

v_s([num:sg, arg:X, arg:Y, sem:enrolled_by(X, Y)]) --> [is, enrolled, by].

v_s([num:sg, arg:X, arg:Y, sem:studied_by(X, Y)]) --> [is, studied, by].

v_s([num:sg, arg:X, arg:Y, sem:belong_to(X, Y)]) --> [is, belongs, to].


%--------------------------------------------
% From RNL to FOL
%--------------------------------------------

formal(Sentence):- open('FOL.txt',append,Stream), s(M2, Sentence, []), numbervars(M2),
				   write(Stream,M2), nl(Stream), close(Stream).
				   
%--------------------------------------------
% From FOL to RNL
%-------------------------------------------- 

rnl(Formal_sentence):- open('CNL_RNL.txt',append,Stream), s(Formal_sentence, Sentence, []), 
					   write(Stream,Sentence), nl(Stream), close(Stream). 

%--------------------------------------------
% Tests
%--------------------------------------------		   


test(proc, Num) :-
  specification(Num, Text),
  process_specification(Text).

% test(gen, Num) :-
%  specification(Num, Text),
%  generate_specification(Text).


process_specification([]).

process_specification([Sentence|Sentences]) :-
  s([sem:Sem], Sentence, []),
  write('Sentence: '), 
  writeq(Sentence), 
  nl,
  numbervars(Sem),
  write('Sem:      '),
  writeq(Sem), 
  nl, nl,
  process_specification(Sentences).

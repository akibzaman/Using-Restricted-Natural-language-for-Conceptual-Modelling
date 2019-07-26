% ===========================================
% Bidirectional Grammar for Lexicon Creation
% with difference list implementation
% Author: Bayzid Ashik Hossain
% Date: 24-07-2019
% ===========================================

:- style_check([-discontiguous, -singleton]).

:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').

:- dynamic lexicon/1.


%------------------------------------------------
% Entity Declaration:- Student is an entity type.
%------------------------------------------------

s([mode:M, type:entity, sem:Sem]) --> 
  np([mode:M, num:sg, type:entity, sem:Sem]), 
  [is, an, entity, type, '.'].

np([mode:M, num:N, type:entity, sem:Sem]) -->
   noun([mode:M, num:N, type:entity, sem:Sem]).

noun([mode:proc, num:N, type:entity, sem:[T1|T2]-[[T|T1]|T2]]) -->
   lexical_rule([cat:noun, num:N, type:entity, sem:T]).

noun([mode:gen, num:N, type:entity, sem:[[T|T1]|T2]-[T1|T2]]) -->
   { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:X, sem:T]) },
   WForm. 

%------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, sem:Sem], List1, List2) :-
   process_noun([wform:WForm, arg:X, sem:Sem, List1, List2]),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, arg:X, sem:Sem])).
   
process_noun([wform:List3, arg:X, sem:Sem, List1, List2]) :-
   append(List3, [is, an, entity, type, '.'], List1), 
   List2 = [is, an, entity, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5),
   Sem =.. [entity, X, List5].

lower_case_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_lower(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]). 
   
%-------------------------------------------------------------
% Data Type Declaration:- Student id is of integer/string/date data type.
%-------------------------------------------------------------

s([mode:M, type:attribute, sem:Sem]) --> 
  np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]), 
  [is, of, DT, data, type, '.'].

np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]) -->
   noun([mode:M, num:N, type:attribute, dt:DT, sem:Sem]).
   

noun([mode:proc, num:N, type:attribute, dt:DT, sem:[T1|T2]-[[T|T1]|T2]])-->
   lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:T]).
   
noun([mode:gen, num:N, type:attribute, dt:DT, sem:[[T|T1]|T2]-[T1|T2]])-->
   { lexicon([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]) },
   WForm. 

%------------------------------------------------------------------------------

lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem], L1, L2):-
   process_attribute([wform:WForm, dt:DT, arg:X, sem:Sem], L1, L2),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:attribute, dt:DT, arg:X, sem:Sem])).
   
process_attribute([wform:List3, dt:D, arg:X, sem:Sem], List1, List2):-
   (D = integer; D = string; D = date; D = boolean),
   append(List3, [is, of, D, data, type, '.'], List1),
   List2 = [is, of, D, data, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5),
   Sem =.. [data_property, X, List5].

%--------------------------------------------------------
% Fact Type Declaration:- Student is enrolled in program.
%--------------------------------------------------------

s([mode:M, type:fact, sem:Sem]) --> 
  np([mode:M, num:N, type:fact, func:subj, arg:X]), 
  vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]), ['.'].
  
vp([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:[T1|T2]-[[Sem|T1]|T2]])-->
  verb([mode:proc, type:fact, label:List]),
  np([mode:proc, num:N, type:fact, func:obj, arg:Y]),
  lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, label:List, sem:Sem]).
  
vp([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:[[Sem|T1]|T2]-[T1|T2]])-->
  verb([mode:gen, type:fact, label:List]),
  np([mode:gen, num:N, type:fact, func:obj, arg:Y]).

np([mode:proc, num:N, type:fact, func:_, arg:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:subj, arg:X]) -->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) },
  { lexicon([cat:noun, wform:NP, num:N, type:entity, arg:_X, sem:X]) }, NP.
  
np([mode:gen, num:N, type:fact, func:obj, arg:X]) -->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) },
  { lexicon([cat:noun, wform:Noun, num:N, type:entity, arg:_X, sem:Y]), lower_case(Noun, NP) }, NP.
  
  
verb([mode:proc, type:fact, label:List3], List1, L) :-
   append(List3, List2, List1), uppercase_first_atom(List2, L).
   
verb([mode:gen, type:fact, label:List])-->
   { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, WForm.

%------------------------------------------------------------------------------

lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, label:List, sem:Sem], P1, P2):-
   atomic_list_concat(List, '_', L), Sem =.. ['FactType', L, X, Y],
   assert(lexicon([cat:verb, wform:List, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem])).
  
lower_case(Noun, NP):-
   lower_case_first_atom(Noun, NP).
  
  
uppercase_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_upper(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).

%-------------------------------------------------------------------------------
% Fact Type [Objectification]
%-------------------------------------------------------------------------------

s([mode:proc, type:fact_ob, sem:[T1|T2]-[[objectify(S,T)|T1]|T2]]) --> 
  np([mode:proc, num:N, type:fact_ob, func:subj, arg:X, sem:S]),
  [objectifies], 
  s([mode:proc, type:ob_fact, sem:[A|B]-[[T|A]|B]]),
  {assert(lexicon([cat:verb, wform:[S,objectifies, T], num:sg, type:ob_rel, arg1:S, arg2:T, sem:objectify(S,T)]))}.
  
s([mode:gen, type:fact_ob, sem:[[objectify(S,T)|T1]|T2]-[T1|T2]]) --> 
  np([mode:gen, num:N, type:fact_ob, func:subj, arg:X, sem:S]),
  [objectifies], s([mode:gen, type:ob_fact, sem:[[T|A]|B]-[A|B]]).

np([mode:proc, num:N, type:fact_ob, func:_, arg:List, sem:S]) -->
  {lexicon([cat:noun, wform:List, num:N, type:entity, arg:_X, sem:S])}, List.
  
np([mode:gen, num:N, type:fact_ob, func:_, arg:List, sem:S]) -->
  {lexicon([cat:noun, wform:List, num:N, type:entity, arg:_X, sem:S])}, List.

s([mode:M, type:ob_fact, sem:Sem]) --> 
  np([mode:M, num:N, type:ob_fact, func:subj, arg:X]), 
  vp([mode:M, num:N, type:ob_fact, arg:X, arg:Y, sem:Sem]), ['.'].
  
vp([mode:proc, num:N, type:ob_fact, arg:X, arg:Y, sem:[T1|T2]-[[List|T1]|T2]])-->
  verb([mode:proc, type:ob_fact, label:List]),
  np([mode:proc, num:N, type:ob_fact, func:obj, arg:Y]).
  
vp([mode:gen, num:N, type:ob_fact, arg:X, arg:Y, sem:[[Sem|T1]|T2]-[T1|T2]])-->
  verb([mode:gen, type:ob_fact, label:List]),
  np([mode:gen, num:N, type:ob_fact, func:obj, arg:Y]).

np([mode:proc, num:N, type:ob_fact, func:_, arg:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) , lower_case(WForm, WForm1)}, WForm1.
  
np([mode:gen, num:N, type:ob_fact, func:subj, arg:X]) -->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) },
  { lexicon([cat:noun, wform:Noun, num:N, type:entity, arg:_X, sem:X]), lower_case(Noun, NP) }, NP.
  
np([mode:gen, num:N, type:ob_fact, func:obj, arg:X]) -->
  { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) },
  { lexicon([cat:noun, wform:Noun, num:N, type:entity, arg:_X, sem:Y]), lower_case(Noun, NP) }, NP.
  
verb([mode:proc, type:ob_fact, label:Sem]) -->
  {lexicon([cat:verb, wform:List3, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem])}, List3.
   
verb([mode:gen, type:ob_fact, label:List])-->
   { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, WForm.
   
   
%-------------------------------------------------------------------------------
% Object Property with exact cardinality: 
%	Every student is enrolled in exactly 1 program.
% Object Property with 1 or more cardinality: 
%	Every student is enrolled in 1 or more program.
% Object Property with minimum and maximum cardinality: 
% 	Every student is enrolled in at, least, L, and, at, most, U program.
%-------------------------------------------------------------------------------


s([mode:M, type:const, sem:Sem]) -->
   np([num:N, mode:M, type:const, arg:X, func:subj, scope:Sco, sem:Sem]),
   vp([num:N, mode:M, type:const, arg:X, scope:Sco]),
   ['.'].
 
np([num:N, mode:M, type:const, arg:X, func:subj, scope:Sco, sem:Sem]) -->
   det([num:N, mode:M, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, mode:M, type:const, arg:X, func:subj, sem:Res]).

np([num:N, mode:M, type:const, arg:K, func:obj, scope:Sco, sem:Sem]) -->
   det([num:N, mode:M, type:const, arg:K, func:obj, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, mode:M, type:const, arg:K, func:obj, sem:Res]).

n([num:N, mode:proc, type:const, arg:X, func:subj, sem:Res])-->
   lr_entity_match_brelsub(V, SV), {lexicon([cat:noun, wform:SV, num:N, type:entity, arg:X, sem:Res])}. 
   
n([num:N, mode:gen, type:const, arg:X, func:subj, sem:Res])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:X, sem:Res])}, downcase_noun(WForm, W).
  
vp([num:N, mode:M, type:const, arg:X, scope:Sco]) -->
   v([num:N, mode:M, type:const, arg:X, arg:K, sem:S]),
   np([num:_, mode:M, type:const, arg:K, func:obj, scope:S, sem:Sco]).
   
n([num:N, mode:proc, type:const, arg:K, func:obj, sem:Sco])-->
   lr_entity_match(V, SV), {lexicon([cat:noun, wform:V, num:N, type:entity, arg:K, sem:Sco])}. 
   
n([num:N, mode:gen, type:const, arg:K, func:obj, sem:Sco])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:K, sem:Sco])}, downcase_noun(WForm, W). 
   
v([num:N, mode:M, type:const, arg:X, arg:K, sem:Sem])-->
   {lexicon([cat:verb, wform:WForm, num:sg, type:brel, X:_X, K:_K, sem:Sem])}, WForm.

det([num:N, mode:proc, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:[A|B]-[[forall(X, Res ==> Sco)|A]|B]]) -->
   ['Every'].

det([num:N, mode:gen, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:[[forall(X, Res ==> Sco)|A]|B]-[A|B]]) -->
   ['Every'].

det([num:N, mode:M, type:const, arg:X, func:obj, restrictor:Res, scope:Sco, sem:exists(X, Res & min(L):Sco:max(L))]) -->
   [exactly, L].
   
det([num:N, mode:M, type:const, arg:X, func:obj, restrictor:Res, scope:Sco, sem:exists(X, Res & min(1):Sco:max(m))]) -->
   [1, or, more].
   
det([num:N, mode:M, type:const, arg:X, func:obj, restrictor:Res, scope:Sco, sem:exists(X, Res & min(L):Sco:max(U))]) -->
   [at, least, L, and, at, most, U], {number(L), number(U), L>0, U>0, L<U}.
   
%--------------------------------------------------  
lr_entity_match(V, SV, P1, P3):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:entity, arg:_X, sem:Sem]), Ent),
  search_v_lr(Ent, P1, V), atomic_list_concat(V,V1), string_lower(V1, SV1), 
  string_to_atom(SV1, SV), append([SV], P3, P1).
  
lr_entity_match_brelsub(V, SV1, P1, P2):-
	findall(WForm, lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]), Rel),
    search_rel_sub(Rel, P1, V), uppercase_first_atom(V, SV1), lower_case_first_atom(V, SV2), append(SV2, P2, P1).
	
search_rel_sub([], _, _) :- false.

search_rel_sub([R|Rel], P1, P3):-
  (sublist(R, P1) -> findall(X, lexicon([cat:verb, wform:R, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]), P2), [E | RestList1] = P2,
					 lexicon([cat:noun, wform:WForm, num:sg, type:entity, arg:_X, sem:E]), P3 = WForm	; 
                     search_rel_sub(Rel, P1, P3)). 
					 
sublist(Sub, List) :-
   sublist_(List, Sub).
   
sublist_([], []).
sublist_([H|T], Sub) :-
	sublist__(T, H, Sub).

sublist__([], H, [H]).
sublist__([], _, []).
sublist__([H|T], X, [X|Sub]) :-
  sublist__(T, H, Sub).
sublist__([H|T], _, Sub) :-
  sublist__(T, H, Sub).
					 
downcase_noun(WForm, W, L1, L2):-
  downcase_list(WForm, W), append(W, L2, L1).

search_v_lr([], _, _) :- false.

search_v_lr([En|Ent], P1, P2):-
  atomic_list_concat(En,V), string_lower(V, V1), string_to_atom(V1, V2),
  (member(V2, P1) -> P2 = En ; search_v_lr(Ent, P1, P2)).
  
downcase_list(AnyCaseList, DownCaseList):-
  maplist(downcase_atom, AnyCaseList, DownCaseList).


  
:- 
s([mode:proc, type:entity, sem:Sem1], ['Student',is,an,entity,type,'.'], []), numbervars(Sem1),
s([mode:proc, type:entity, sem:Sem2], ['Program',is,an,entity,type,'.'], []), numbervars(Sem2),
s([mode:proc, type:entity, sem:Sem3], ['Enrolment',is,an,entity,type,'.'], []), numbervars(Sem3),
s([mode:proc, type:entity, sem:Sem6], ['Unit',is,an,entity,type,'.'], []), numbervars(Sem6),
s([mode:proc, type:fact, sem:Sem4], ['Student',is,enrolled,in,program,'.'], []), numbervars(Sem4),
s([mode:proc, type:fact, sem:Sem7], ['Student',studies,unit,'.'], []), numbervars(Sem7),
s([mode:proc, type:fact_ob, sem:Sem5], ['Enrolment',objectifies, student,is,enrolled,in,program,'.'], []), numbervars(Sem5).

%s([mode:gen, type:fact_ob, sem:[[objectify(entity(C, enrolment), 'FactType'([is, enrolled, in], entity(D, student), entity(E, program)))|A]|B]-[A|B]], S, []), numbervars(S).
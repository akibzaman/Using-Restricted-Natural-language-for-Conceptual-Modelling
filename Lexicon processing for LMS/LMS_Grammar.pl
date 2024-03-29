% ===========================================
% Bidirectional Grammar with Lexicon Creation
% Author: Bayzid Ashik Hossain
% Date: 12-07-2019
% ===========================================

:- style_check([-discontiguous, -singleton]).

%:- consult(function_words).
:- consult(tests).

:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').

:- dynamic lexicon/1.


%--------------------------------------------
% Entity Declaration:- Student is an entity.
%--------------------------------------------

s([mode:M, type:entity, sem:Sem]) --> 
  np([mode:M, num:sg, type:entity, sem:Sem]), 
  [is, an, entity, '.'].

np([mode:M, num:N, type:entity, sem:Sem]) -->
   noun([mode:M, num:N, type:entity, sem:Sem]).

noun([mode:proc, num:N, type:entity, sem:Sem]) -->
   lexical_rule([cat:noun, num:N, type:entity, sem:Sem]).

noun([mode:gen, num:N, type:entity, sem:Sem]) -->
   { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) },
   WForm. 


%------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, sem:Sem], List1, List2) :-
   process_noun([wform:WForm, arg:X, sem:Sem, List1, List2]),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, arg:X, sem:Sem])).


%------------------------------------------------------------

process_noun([wform:List3, arg:X, sem:Sem, List1, List2]) :-
   append(List3, [is, an, entity, '.'], List1), 
   List2 = [is, an, entity, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5),
   Sem =.. [List5, X].

lower_case_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_lower(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).  

   
%--------------------------------------------
% Data Type Declaration:- Student id is of integer/string/date data type.
%--------------------------------------------

s([mode:M, type:attribute, sem:Sem]) --> 
  np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]), 
  [is, of, DT, data, type, '.'].

np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]) -->
   noun([mode:M, num:N, type:attribute, dt:DT, sem:Sem]).
   

noun([mode:proc, num:N, type:attribute, dt:DT, sem:Sem])-->
   lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]).
   
noun([mode:gen, num:N, type:attribute, dt:DT, sem:Sem])-->
   { lexicon([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]) },
   WForm. 

%------------------------------------------------------------------------------

lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem], L1, L2):-
   process_noun_att([wform:WForm, dt:DT, arg:X, sem:Sem], L1, L2),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:attribute, dt:DT, arg:X, sem:Sem])).
   
process_noun_att([wform:List3, dt:D, arg:X, sem:Sem], List1, List2):-
   (D = integer; D = string; D = date; D = boolean),
   append(List3, [is, of, D, data, type, '.'], List1),
   List2 = [is, of, D, data, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5),
   Sem =.. [List5, X].

%   assert(lexicon([syn:[cat:noun, wform:P3, num:sg], sem:[type:attribute, att_type:DT, arg:X, lit:Sem]])).
%   assert(lexicon([cat:attribute, wform:P3, type:DT, num:sg, arg:X, sem:Sem])).


%--------------------------------------------
% Fact Type Declaration:- Student is enrolled in program.
%--------------------------------------------

s([mode:M, type:fact, sem:Sem]) --> 
  np([mode:M, num:N, type:fact, func:subj, arg:X]), 
  vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]), ['.'].
  
vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
  verb([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]).

np([mode:proc, num:N, type:fact, func:_, arg:WForm]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:_, arg:X]) -->
  { lexicon([cat:verb, wform:P3, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, [X].
   
verb([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
   lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem]).
   
verb([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
   { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, WForm, [Y].

%------------------------------------------------------------------------------

lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem], P1, P2):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:entity, arg:_X, sem:Sem]), Ent),
  search_v(Ent, P1, V),
  append(P3, [V,'.'], P1),  
  atomic_list_concat(P3,'_',P4), 
  (search_v(Ent, [V], Y) ->
  atomic_list_concat(X, Xn), string_lower(Xn, X1), string_to_atom(X1, X2), Y = V1,
  %X3 =.. [X2, A], Y1 =.. [Y, B],
  Sem =.. [P4, A, B],
  assert(lexicon([cat:verb, wform:P3, num:sg, type:brel, A:X2, B:Y, sem:Sem]))).
%  assert(lexicon([cat:rel, wform:P4, type:bdrel, arg1:X2, arg2:Y, sem:Sem])).
  
search_v([], _, _) :- false.

search_v([En|Ent], P1, P2):-
  atomic_list_concat(En,V), string_lower(V, V1), string_to_atom(V1, V2),
  (member(V2, P1) -> P2 = V2 ; search_v(Ent, P1, P2)).

  
%--------------------------------------------
% Fact Type [Objectification]
%--------------------------------------------

s([mode:M, type:fact_ob, sem:Sem]) --> 
  np([mode:M, num:N, type:fact_ob, func:subj, arg:X, sem:Sem]),
  [objectifies],	
  verb([mode:M, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem]), ['.'].
  
verb([mode:M, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem])-->
  v([mode:M, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem]).

np([mode:proc, num:N, type:fact_ob, func:_, arg:List, sem:Sem]) -->
  {lexicon([cat:noun, wform:List, num:N, type:entity, arg:_X, sem:S])}, List.
  
np([mode:gen, num:N, type:fact_ob, func:_, arg:List, sem:Sem]) -->
  { lexicon([cat:verb, wform:W, num:sg, type:obj_rel, arg1:Arg1, arg2:Arg2, sem:Sem]) }, [Arg1].
  
v([mode:proc, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:V])-->
  {lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X2, arg2:Y, sem:Sem])},
  lexical_rule_objectification([X, WForm, X2, Y, Sem, V]).
  
v([mode:gen, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem])-->
  { lexicon([cat:verb, wform:W, num:sg, type:obj_rel, arg1:Arg1, arg2:Arg2, sem:Sem]) },
  {lexicon([cat:verb, wform:Arg2, num:sg, type:brel, arg1:X2, arg2:Y, sem:S])}, [X2], Arg2, [Y].
  
  %{ lexicon([cat:verb, wform:W, num:sg, type:obj_rel, arg1:Arg1, arg2:Arg2, sem:F]) }

%-------------------------------------------------------------------------
  
lexical_rule_objectification([X, WForm, X2, Y, Rel, F], P1, P2):-
	(sublist(WForm, P1) ->
	atomic_list_concat(X,V), V1 =.. [V, E], 
	atomic_list_concat(WForm,'_',P4), P5 =.. [P4, A, B], X3 =.. [X2, P], Y2 =.. [Y, K], %P5 =.. [P4, X3, Y2]
	F =.. [objectify, V1, E:P5], W = [V, objectify, Rel],
	assert(lexicon([cat:verb, wform:W, num:sg, type:obj_rel, arg1:V, arg2:WForm, sem:F]))).
	
	
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
 
%--------------------------------------------
% Object Property with exact cardinality :- Every student is enrolled in exactly 1 program.
%--------------------------------------------

s([mode:M, type:const, sem:Sem]) -->
   np_s([num:N, mode:M, type:const, arg:X, scope:Sco, sem:Sem]),
   vp_s([num:N, mode:M, type:const, arg:X, scope:Sco]),
   ['.'].
 
np_s([num:N, mode:M, type:const, arg:X, scope:Sco, sem:Sem]) -->
   det_s([num:N, mode:M, type:const, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, mode:M, type:const, arg:X, sem:Res]).

np_o([num:N, mode:M, type:const, arg:K, scope:Sco, sem:Sem]) -->
   det_o([num:N, mode:M, type:const, arg:K, restrictor:Res, scope:Sco, sem:Sem]),
   n_o([num:N, mode:M, type:const, arg:K, sem:Res]).

n([num:N, mode:proc, type:const, arg:X, sem:Res])-->
   lr_entity_match_brelsub(V, SV), {lexicon([cat:noun, wform:SV, num:N, type:entity, arg:X, sem:Res])}. 
   
n([num:N, mode:gen, type:const, arg:X, sem:Res])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:X, sem:Res])}, downcase_noun(WForm, W).
  
vp_s([num:N, mode:M, type:const, arg:X, scope:Sco]) -->
   v_s([num:N, mode:M, type:const, arg:X, arg:K, sem:S]),
   np_o([num:_, mode:M, type:const, arg:K, scope:S, sem:Sco]).
   
n_o([num:N, mode:proc, type:const, arg:K, sem:Sco])-->
   lr_entity_match(V, SV), {lexicon([cat:noun, wform:V, num:N, type:entity, arg:K, sem:Sco])}. 
   
n_o([num:N, mode:gen, type:const, arg:K, sem:Sco])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:K, sem:Sco])}, downcase_noun(WForm, W). 
   
v_s([num:N, mode:M, type:const, arg:X, arg:K, sem:Sem])-->
   {lexicon([cat:verb, wform:WForm, num:sg, type:brel, X:_X, K:_K, sem:Sem])}, WForm.
   
det_s([num:N, mode:M, type:const, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
   ['Every'].

det_o([num:N, mode:M, type:const, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & min(L):Sco:max(L))]) -->
   [exactly, L].
   
%--------------------------------------------------  
lr_entity_match(V, SV, P1, P3):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:entity, arg:_X, sem:Sem]), Ent),
  search_v_lr(Ent, P1, V), atomic_list_concat(V,V1), string_lower(V1, SV1), 
  string_to_atom(SV1, SV), append([SV], P3, P1).
  
downcase_noun(WForm, W, L1, L2):-
  downcase_list(WForm, W), append(W, L2, L1).

%--------------------------------------------
% Object Property with one or more cardinality :- Every program is composed of one or more unit.
%--------------------------------------------

s([mode:M, type:const_m, sem:Sem]) -->
   np_s([num:N, mode:M, type:const_m, arg:X, scope:Sco, sem:Sem]),
   vp_s([num:N, mode:M, type:const_m, arg:X, scope:Sco]),
   ['.'].
 
np_s([num:N, mode:M, type:const_m, arg:X, scope:Sco, sem:Sem]) -->
   det_s([num:N, mode:M, type:const_m, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n_s([num:N, mode:M, type:const_m, arg:X, sem:Res]).

np_o([num:N, mode:M, type:const_m, arg:K, scope:Sco, sem:Sem]) -->
   det_o([num:N, mode:M, type:const_m, arg:K, restrictor:Res, scope:Sco, sem:Sem]),
   n_o([num:N, mode:M, type:const_m, arg:K, sem:Res]).

n_s([num:N, mode:proc, type:const_m, arg:X, sem:Res])-->
   lr_entity_match_brelsub(V, SV1), {lexicon([cat:noun, wform:SV1, num:N, type:entity, arg:X, sem:Res])}. 
   
n_s([num:N, mode:gen, type:const_m, arg:X, sem:Res])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:X, sem:Res])}, downcase_noun(WForm, W). 
  
vp_s([num:N, mode:M, type:const_m, arg:X, scope:Sco]) -->
   v_s([num:N, mode:M, type:const_m, arg:X, arg:K, sem:S]),
   np_o([num:_, mode:M, type:const_m, arg:K, scope:S, sem:Sco]).
   
n_o([num:N, mode:proc, type:const_m, arg:K, sem:Sco])-->
   lr_entity_match(V, SV), {lexicon([cat:noun, wform:V, num:N, type:entity, arg:K, sem:Sco])}. 
   
n_o([num:N, mode:gen, type:const_m, arg:K, sem:Sco])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:K, sem:Sco])}, downcase_noun(WForm, W). 
   
v_s([num:N, mode:M, type:const_m, arg:X, arg:K, sem:Sem])-->
   {lexicon([cat:verb, wform:WForm, num:sg, type:brel, X:_X, K:_K, sem:Sem])}, WForm.

det_s([num:N, mode:M, type:const_m, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
   ['Every'].

det_o([num:N, mode:M, type:const_m, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & min(1):Sco:max(*))]) -->
   [1, or, more].

%--------------------------------------------

lr_entity_match_brelsub(V, SV1, P1, P2):-
	findall(WForm, lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]), Rel),
    search_rel_sub(Rel, P1, V), upper_case_first_atom(V, SV1), append(V, P2, P1).
	
search_rel_sub([], _, _) :- false.

search_rel_sub([R|Rel], P1, P3):-
  (sublist(R, P1) -> findall(X, lexicon([cat:verb, wform:R, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]), P3) ; 
                     search_rel_sub(Rel, P1, P3)). 
	
%--------------------------------------------
% Object Property with at least L and at most M cardinality :- Every student studies at least 1 and at most 4 unit.
% Object Property with minimum and maximum cardinality constraint
%--------------------------------------------

s([mode:M, type:minmax, sem:Sem]) -->
   np([num:N, mode:M, type:minmax, arg:X, scope:Sco, sem:Sem]),
   vp([num:N, mode:M, type:minmax, arg:X, scope:Sco]),
   ['.'].
 
np([num:N, mode:M, type:minmax, arg:X, scope:Sco, sem:Sem]) -->
   det_s([num:N, mode:M, type:minmax, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:N, mode:M, type:minmax, arg:X, sem:Res]).

np_o([num:N, mode:M, type:minmax, arg:K, scope:Sco, sem:Sem]) -->
   det_o([num:N, mode:M, type:minmax, arg:K, restrictor:Res, scope:Sco, sem:Sem]),
   n_o([num:N, mode:M, type:minmax, arg:K, sem:Res]).

n([num:N, mode:proc, type:minmax, arg:X, sem:Res])-->
   lr_entity_match_brelsub(V, SV1), {lexicon([cat:noun, wform:SV1, num:N, type:entity, arg:X, sem:Res])}. 
   
n([num:N, mode:gen, type:minmax, arg:X, sem:Res])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:X, sem:Res])}, downcase_noun(WForm, W). 
  
vp([num:N, mode:M, type:minmax, arg:X, scope:Sco]) -->
   v_s([num:N, mode:M, type:minmax, arg:X, arg:K, sem:S]),
   np_o([num:_, mode:M, type:minmax, arg:K, scope:S, sem:Sco]).
   
n_o([num:N, mode:proc, type:minmax, arg:K, sem:Sco])-->
   lr_entity_match(V, SV), {lexicon([cat:noun, wform:V, num:N, type:entity, arg:K, sem:Sco])}.

n_o([num:N, mode:gen, type:minmax, arg:K, sem:Sco])-->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:K, sem:Sco])}, downcase_noun(WForm, W).     
   
v_s([num:N, mode:M, type:minmax, arg:X, arg:K, sem:Sem])-->
   {lexicon([cat:verb, wform:WForm, num:sg, type:brel, X:_X, K:_K, sem:Sem])}, WForm.

det_s([num:N, mode:M, type:minmax, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
   ['Every'].

det_o([num:N, mode:M, type:minmax, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & min(L):Sco:max(H))]) -->
   [at,least,L,and,at,most,H].


%-----------------------------------------------------------

search_v_lr([], _, _) :- false.

search_v_lr([En|Ent], P1, P2):-
  atomic_list_concat(En,V), string_lower(V, V1), string_to_atom(V1, V2),
  (member(V2, P1) -> P2 = En ; search_v_lr(Ent, P1, P2)).

upper_case_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_upper(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).


%--------------------------------------------
% Data Property "Student"
%--------------------------------------------

s([mode:const, type:dp, sem:Sem]) -->
   np_dp([num:N, mode:const, type:dp, arg:X, scope:Sco, sem:Sem]),
   vp([crd:'+', num:N, mode:const, type:dp, arg:X, sem:Sco]),
   ['.'].

   
np_dp([num:N, mode:const, type:dp, arg:X, scope:Sco, sem:Sem]) -->
   det_s([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
   n([num:sg, arg:X, sem:Res]). % edited

   
np_odp([num:N, arg:Y, scope:Sco, sem:Sem]) -->
   det_odp([num:N, arg:Y, restrictor:Res, scope:Sco, sem:Sem]),
   n_o([num:N, arg:Y, sem:Res]).
   
n([num:N, arg:X, sem:Res]) -->
  lr_entity_match(V, SV1), {lexicon([cat:noun, wform:V, num:N, type:entity, arg:X, sem:Res])}.
   
n_o([num:N, arg:Y, sem:Res]) -->
  lr_attribute_match(V, SV1), {lexicon([cat:noun, wform:V, num:N, type:attribute, dt:DT, arg:Y, sem:Res])}.
   

vp([crd:'+', num:N, mode:const, type:dp, arg:X, sem:(Sco1 & Sco2)]) -->   
    vp([crd:'-', num:N, mode:const, type:dp, arg:X, sem:Sco1]),
    cc([wfm:[and]]),
    vp([crd:'+', num:N, mode:const, type:dp, arg:X, sem:Sco2]).
   

vp([crd:'+', num:N, mode:const, type:dp, arg:X, sem:Sco]) -->   
    v([num:N, arg:X, arg:Y, sem:Sem]), 
    np_odp([num:_, arg:Y, scope:Sem, sem:Sco]).


vp([crd:'-', num:N, mode:const, type:dp, arg:X, sem:Sco]) -->   
    v([num:N, arg:X, arg:Y, sem:Sem]), 
    np_odp([num:_, arg:Y, scope:Sem, sem:Sco]).

v([num:N, arg:X, arg:Y, sem:possess(X, Y)]) -->
   [has].

det_s([num:N, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) -->
   ['Every'].

det_odp([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) -->[exactly, 1].

%det_odp([num:N, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & Sco)]) -->[an].

cc([wfm:[and]]) --> [and].   
%-----------------------------------------------------------

lr_attribute_match(V, SV, P1, P3):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:attribute, dt:DT, arg:X, sem:Sem]), Ent),
  search_v_att_lr(Ent, P1, V), downcase_list(V,V1), append(V1, P3, P1).
  
search_v_att_lr([], _, _) :- false.

search_v_att_lr([En|Ent], P1, P2):-
  downcase_list(En,V), 
  (sublist(V, P1) -> P2 = En ; search_v_att_lr(Ent, P1, P2)).
  
downcase_list(AnyCaseList, DownCaseList):-
  maplist(downcase_atom, AnyCaseList, DownCaseList).
  
%-------------------------------------------------------------
% Entity Declaration / Class Declaration :- Every student is an entity.
%-------------------------------------------------------------

s([mode:const, type:ed, sem:Sem]) --> 
   np_d([num:N, mode:const, type:ed, arg:X, scope:Sco, sem:Sem]), 
   vp_d([num:N, mode:const, type:ed, arg:X, scope:Sco]), ['.'].

np_d([num:N, mode:const, type:ed, arg:X, scope:Sco, sem:Sem]) --> 
   det_d([num:N, arg:X, restrictor:Res, scope:Sco, sem:Sem]), 
   n_d([num:N, arg:X, sem:Res]).
   
n_d([num:N, arg:X, sem:Res]) -->
   lr_entity_match(V, SV1), {lexicon([cat:noun, wform:V, num:N, type:entity, arg:X, sem:Res])}.

vp_d([num:N, mode:const, type:ed, arg:X, scope:Sco]) --> 
   iv_d([num:N, arg:X, scope:Sco]).

det_d([num:sg, arg:X, restrictor:Res, scope:Sco, sem:forall(X, Res ==> Sco)]) --> 
   ['Every'].

iv_d([num:sg, arg:X, scope:entity(X)]) --> 
   [is, an, entity, type].  
  

%-----------------------------------------------------------
test(proc, Num) :-
  specification(Num, Text),
  process_specification(Text).

% test(gen, Num) :-
%  specification(Num, Text),
%  generate_specification(Text).


process_specification([]).

process_specification([Sentence|Sentences]) :-
  (
  (s([mode:proc, type:entity, sem:Sem], Sentence, [])) ; 
  (s([mode:proc, type:attribute, sem:Sem], Sentence, [])) ; 
  (s([mode:proc, type:fact, sem:Sem], Sentence, [])) ;
  (s([mode:proc, type:fact_ob, sem:Sem], Sentence, []));
  (s([mode:proc, type:const, sem:Sem], Sentence, [])) ;
  (s([mode:proc, type:minmax, sem:Sem], Sentence, [])) ;
  (s([mode:proc, type:const_m, sem:Sem], Sentence, [])) ;
  (s([mode:const, type:dp, sem:Sem], Sentence, [])) ;
  (s([mode:const, type:ed, sem:Sem], Sentence, []))
  ),
  write('Sentence: '), 
  writeq(Sentence), 
  nl,
  numbervars(Sem),
  write('Sem:      '),
  writeq(Sem), 
  nl, nl,
  process_specification(Sentences).  
  
%-------------------------------------------------------------------------
% Storing the lexicon file.
%-------------------------------------------------------------------------

store_lexicon():-
   findall(S, (lexicon(S), numbervars(S)), List), 
   open('Lexicon.txt',write,Stream),
   write_lexicon(List, Stream), close(Stream).

write_lexicon([],_).

write_lexicon([L|List], Stream):-
	write(Stream,L), 
	nl(Stream), 
	write_lexicon(List, Stream).

%-------------------------------------------------------------------------
% Generating IDL for the constraints.
%-------------------------------------------------------------------------
test(const, Num) :-
  specification(Num, Text),
  open('const.txt',write,Stream),
  process_const(Text, Stream), close(Stream).
  
process_const([],_).

process_const([Sentence|Sentences], Stream) :-
  (
  (s([mode:const, type:exact, sem:Sem], Sentence, [])) ;
  (s([mode:const, type:some, sem:Sem], Sentence, [])) ;
  (s([mode:const, type:minmax, sem:Sem], Sentence, [])) ;
  (s([mode:const, type:dp, sem:Sem], Sentence, [])) ;
  (s([mode:const, type:ed, sem:Sem], Sentence, []))
  ),
  write('Sentence: '), 
  writeq(Sentence), 
  nl,
  numbervars(Sem),
  write('Sem:      '),
  writeq(Sem), 
  nl, nl,
  write(Stream,Sem), 
  nl(Stream), 
  process_const(Sentences, Stream).  

%-------------------------------------------------------------------------
 process_cnl_spec():-
   test(proc, 2), store_lexicon(), test(const, 3). 
 

% ===========================================
% Bidirectional Grammar with Lexicon Creation
% Author: Bayzid Ashik Hossain
% Date: 08-07-2019
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

s([mode:M, sem:Sem]) --> 
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

s([mode:M, sem:Sem]) --> 
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

s([mode:M, sem:Sem]) --> 
  np([mode:M, num:N, type:fact, func:subj, arg:X]), 
  vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]), [Y, '.'].
  
vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
  verb([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]).

np([mode:proc, num:N, type:fact, func:_, arg:WForm]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) }, WForm.
  
np([mode:gen, num:N, type:fact, func:_, arg:X]) -->
  { lexicon([cat:verb, wform:P3, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, [X].
   
verb([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
   lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem]).
   
verb([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
   { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, WForm.

%------------------------------------------------------------------------------

lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem], P1, P2):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:entity, arg:_X, sem:Sem]), Ent),
  search_v(Ent, P1, V),
  append(P3, [V,'.'], P1),  
  atomic_list_concat(P3,'_',P4), 
  (search_v(Ent, [V], Y) ->
  atomic_list_concat(X, Xn), string_lower(Xn, X1), string_to_atom(X1, X2), Y = V1,
  %X3 =.. [X2, A], Y1 =.. [Y, B],
  Sem =.. [P4, X2, Y],
  assert(lexicon([cat:verb, wform:P3, num:sg, type:brel, arg1:X2, arg2:Y, sem:Sem]))).
%  assert(lexicon([cat:rel, wform:P4, type:bdrel, arg1:X2, arg2:Y, sem:Sem])).
  
search_v([], _, _) :- false.

search_v([En|Ent], P1, P2):-
  atomic_list_concat(En,V), string_lower(V, V1), string_to_atom(V1, V2),
  (member(V2, P1) -> P2 = V2 ; search_v(Ent, P1, P2)).

  
%--------------------------------------------
% Fact Type [Objectification]
%--------------------------------------------

s([mode:M, sem:Sem]) --> 
  np([mode:M, num:N, type:fact_ob, func:subj, arg:X]),
  [objectifies],	
  verb([mode:M, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem]).
  
verb([mode:M, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem])-->
  v([mode:proc, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:Sem]).

np([mode:M, num:N, type:fact_ob, func:_, arg:List]) -->
  {lexicon([cat:noun, wform:List, num:N, type:entity, arg:_X, sem:Sem])}, List.

v([mode:proc, num:N, type:fact_ob, arg:X, arg:Y, arg:Z, arg:K, sem:V])-->
  {lexicon([cat:verb, wform:P3, num:sg, type:brel, arg1:X2, arg2:Y, sem:Sem])},
  lexical_rule_objectification([X, P3, X2, Y, Sem, V]).
  
lexical_rule_objectification([X, P3, X2, Y, Rel, F], P1, P2):-
	atomic_list_concat(X,V), V1 =.. [V, E], 
	atomic_list_concat(P3,'_',P4), P5 =.. [P4, X3, Y2], X3 =.. [X2, P], Y2 =.. [Y, K], 
	F =.. [objectify, V1, E:P5], W = [X, objectify, Rel],
	assert(lexicon([syn:[cat:verb, wform:W, num:sg], sem:[type:obj_rel, arg1:V, arg2:P4, lit:F]])).
%	assert(lexicon([cat:objectification, wform:X, arg1:V, arg2:Rel, sem:F])).
  
  
  
  
  
  
/*
 
%--------------------------------------------------------------------------------
  
 s([mode:M, sem:Sem]) --> 
  np([mode:M, num:N, type:fact, func:subj, arg:X, sem:Sem]), 
  vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]), ['.'].
  
vp([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
  verb([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]),
  np([mode:M, num:N, type:fact, func:obj, arg:Z, sem:Sem]),
  lexical_rule_fact([mode:M, num:N, type:fact, arg:X, arg:Y, arg:Z, sem:Sem]).

np([mode:proc, num:N, type:fact, func:_, arg:WForm, sem:_]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, arg:_X, sem:Sem]) }, WForm.

np([mode:gen, num:N, type:fact, func:_, arg:X, sem:Sem]) -->
  { lexicon([cat:verb, wform:P3, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, [X].
   
verb([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
   lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem]).
   
verb([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
   { lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg1:X, arg2:Y, sem:Sem]) }, WForm. 
  
lexical_rule([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem], P1, P2):-
  findall(W, lexicon([cat:noun, wform:W, num:N, type:entity, arg:_X, sem:Sem]), Ent),
  search_v(Ent, P1, V),
  append(P3, [V,'.'], P1),  
  atomic_list_concat(P3,'_',P4),
  P2 = P3, Y = P4.

lexical_rule_noun([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem], P1, P2):-  
  
  
lexical_rule_fact([mode:M, num:N, type:fact, arg:X, arg:Y, arg:Z, sem:Sem], P1, P2):-
  Sem =.. [Y, X, Z],
  assert(lexicon([cat:verb, wform:P3, num:sg, type:brel, arg1:X, arg2:Z, sem:Sem])).
  
search_v([], _, _) :- false.

search_v([En|Ent], P1, P2):-
  atomic_list_concat(En,V), string_lower(V, V1), string_to_atom(V1, V2),
  (member(V2, P1) -> P2 = V2 ; search_v(Ent, P1, P2)).
  
*/
 
%------------------------------------------------------------------------

test(proc, Num) :-
  specification(Num, Text),
  process_specification(Text).

% test(gen, Num) :-
%  specification(Num, Text),
%  generate_specification(Text).


process_specification([]).

process_specification([Sentence|Sentences]) :-
  s([mode:proc, sem:Sem], Sentence, []),
  write('Sentence: '), 
  writeq(Sentence), 
  nl,
  numbervars(Sem),
  write('Sem:      '),
  writeq(Sem), 
  nl, nl,
  process_specification(Sentences).  
  
%-------------------------------------------------------------------------

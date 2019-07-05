% ===========================================
% Bidirectional Grammar with Lexicon Creation
% Testing with lookahead DCG
% Author: Bayzid Ashik Hossain
% ===========================================

:- style_check([-discontiguous, -singleton]).

:- consult(function_words).
:- consult(tests).

:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').

:- dynamic lexicon/1.

s([mode:M, sem:Sem]) --> 
  np([mode:M, num:N, type:data, arg:X, sem:Sem]), [is, of, DT, data, type, '.'].
	
np([mode:M, num:N, type:data, arg:X, sem:Sem]) -->
   n([mode:M, num:N, type:data, arg:X, sem:Sem]).

n([proc:proc, num:N, type:data, arg:X, sem:Sem]), [is, of, DT, data, type, '.'] -->
    WForm,
    { lexicon([cat:n, mode:proc, wform:WForm, num:N, type:DT, arg:X, sem:Sem]) }.
	
	
lexicon([cat:n, mode:proc, wform:WForm, num:N, type:DT, arg:X, sem:Sem]) :-
   DT = ( integer ; string ; date ),
   lower_case_first_atom(WForm, P4),
   atomic_list_concat(P4, '_', P5),
   Sem =.. [P5, X],
   assert(lexicon([syn:[cat:noun, wform:P4, num:sg], sem:[type:entity,  arg:X, lit:Sem]])).

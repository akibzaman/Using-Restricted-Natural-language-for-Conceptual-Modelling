:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:- dynamic lexicon/1.
%:- consult(lexgen).
:- consult(predicates).
:- consult(test).

% --------------------
% |Class Declaration|
% --------------------

s([mode:M, type:entity, sem:json(['Atom'=Sem])]) --> 
  np([mode:M, num:sg, type:entity, pos:subj, arg:X, sem:Sem]), 
  [is, a, class, type], ['.'].

np([mode:M, num:N, type:entity, pos:P, arg:X, sem:Sem]) --> 
  noun([mode:M, num:N, type:entity, pos:P, arg:X, sem:Sem]). 

noun([mode:proc, num:N, type:entity, pos:subj, arg:X, sem:Sem]) -->        
  lexical_rule([cat:noun, num:N, type:entity, pos:subj, arg:X, sem:Sem]). 

noun([mode:gen, num:N, type:entity, pos:subj, arg:X, sem:Sem]) -->         
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:yes, arg:X, sem:Sem])}, 
  WForm. 

% ------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, pos:subj, arg:X, sem:Sem], List1, List2) :-  
   process_noun([wform:WForm, List5, List1, List2]), 
   Sem = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]),
   downcase_list(WForm, DWForm), last(WForm, L), atomics_to_string(WForm, WF), morphology(L, Lp),
   (( 
	L \= WF, append([W], [L], DWForm), append([W], [Lp], PWForm),
    assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:subj, init:yes, arg:X, sem:Sem])), 
    assert(lexicon([cat:noun, wform:DWForm, num:sg, type:entity, pos:subj, init:no, arg:X, sem:Sem])), 
    assert(lexicon([cat:noun, wform:DWForm, num:sg, type:entity, pos:obj, arg:X, sem:Sem])), 
    assert(lexicon([cat:noun, wform:PWForm, num:pl, type:entity, pos:obj, arg:X, sem:Sem])));
   ( downcase_list([Lp], PWF),
    assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:subj, init:yes, arg:X, sem:Sem])), 
    assert(lexicon([cat:noun, wform:DWForm, num:sg, type:entity, pos:subj, init:no, arg:X, sem:Sem])), 
    assert(lexicon([cat:noun, wform:DWForm, num:sg, type:entity, pos:obj, arg:X, sem:Sem])), 
    assert(lexicon([cat:noun, wform:PWF, num:pl, type:entity, pos:obj, arg:X, sem:Sem]))
   )).   
   
process_noun([wform:List3, List5, List1, List2]) :-
   append(List3, [is, a, class, type, '.'], List1), 
   List2 = [is, a, class, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5).
   
% -------------------------------------------------------------

% -----------------------
% |Attribute Declaration|
% -----------------------   
   
s([mode:M, type:attribute, sem:json(['Atom'=Sem])]) --> 
  np([mode:M, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem]),       
  [is, of, DT, data, type, '.'].

np([mode:M, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem]) -->
   noun([mode:M, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem]).

noun([mode:proc, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem])-->     
   lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem]).
   
noun([mode:gen, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem])-->
   { lexicon([cat:noun, wform:WForm, num:N, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem]) },
   WForm. 

% ------------------------------------------------------------------------------

lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, pos:subj, dt:DT, arg:_X, sem:Sem], L1, L2):-
   process_attribute([wform:WForm, dt:DT, arg:X, sem:Sem], L1, L2), downcase_list(WForm, DWForm),
   assert(lexicon([cat:noun, wform:DWForm, num:sg, type:attribute, pos:obj, dt:DT, arg:X, sem:Sem])),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:attribute, pos:subj, dt:DT, arg:X, sem:Sem])).
   
process_attribute([wform:List3, dt:D, arg:X, sem:Sem], List1, List2):-
   (D = integer; D = string; D = date; D = boolean),
   append(List3, [is, of, D, data, type, '.'], List1),
   List2 = [is, of, D, data, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5), V = ['_type'=D, '__text'=X],
   Sem =json(['Rel'=attribute, 'Ind'=List5, 'Var'=json(V)]).
   
% ------------------------------------------------------------------------------   
   
% ---------------------------------
% |Binary Relationship Declaration|
% ---------------------------------

s([mode:M, type:fact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([mode:M, num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  vp([mode:M, num:N, type:fact, arg:X, sem:Sco]),
  ['.'].

np([mode:M, num:N, type:fact, pos:P, arg:X, sem:Sem]) -->                 
  noun([mode:M, num:N, type:fact, pos:P, arg:X, sem:Sem]). 

vp([mode:M, num:N, type:fact, arg:X, sem:[P, O]])-->                   
  verb([mode:M, num:N, type:fact, arg:X, arg:Y, sem:P]),
  np([mode:M, num:_N, type:fact, pos:obj, arg:Y, sem:O]).

noun([mode:M, num:N, type:fact, pos:subj, arg:X,  sem:Sem]) -->       
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:yes, arg:X, sem:Sem]) },
  WForm.
  
noun([mode:M, num:N, type:fact, pos:obj, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:Y, sem:Sem]) }, 
  WForm.
  
verb([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem]) -->
  lexical_rule([cat:verb, num:N, arg:X, arg:Y, sem:Sem]).

verb([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
  { lexicon([cat:verb, wform:WForm, num:N, type:relation, arg:X, arg:Y, sem:Sem]) }, WForm.

% ------------------------------------------------------------------------------

lexical_rule([cat:verb, num:N, arg:X, arg:Y, sem:B], P1, P2) :-
  generate_sem(WForm, P1, P2, X, Y, B),
  (
  assert(lexicon([cat:verb, wform:WForm, num:N, type:relation, arg:X, arg:Y, sem:B]))
  ;
  retract(lexicon([cat:verb, wform:WForm, num:N, type:relation, arg:X, arg:Y, sem:B]))
  ).
  

generate_sem(WForm, P1, P2, X, Y, B) :-
   append(WForm, P2, P1),
   WForm \= [], 
   morphology_rel(WForm, WF),
   atomic_list_concat(WF,'_', Term),
   B = json(['Rel'=relation, 'Ind'=Term, 'Var'=[X, Y]]).

morphology_rel([L|L1], L2):-
	((L == is) -> L2 = L1) ; 
	((sub_atom(L,Q,3,0,ies) -> sub_atom(L,_,Q,3,L0), atom_concat(L0,y,L3), L2 = [L3|L1]));
	((sub_atom(L,Q,2,0,ys) -> sub_atom(L,_,Q,2,L0), atom_concat(L0,y,L3), L2 = [L3|L1]));	
	((porter_stem(L,L3), L2 = [L3|L1])).


reverse([])     --> [].
reverse([L|Ls]) --> reverse(Ls), [L].

% ------------------------------------------------------------------------------

% ---------------------------------
% |Ternary Relationship Declaration|
% ---------------------------------

s([mode:M, type:tfact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([mode:M, num:N, type:tfact, pos:subj, arg:X, sem:Res]), 
  vp([mode:M, num:N, type:tfact, arg:X, sem:Sco]), ['.'].

np([mode:M, num:N, type:T, pos:subj, arg:X, sem:Sem]) -->                
  noun([mode:M, num:N, type:T, pos:subj, arg:X, sem:Sem]). 

np1([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]) -->                
  noun([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]). 

np2([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]) -->                
  noun([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]). 

vp([mode:proc, num:N, type:tfact, arg:X, sem:[V, Ob1, Ob2]])-->
  verb([mode:proc, wform:WForm1, type:tfact]),
  np1([mode:proc, num:N, type:tfact, pos:obj, arg:Y, sem:Ob1]),
  verb([mode:proc, wform:WForm2, type:tfact]),
  np2([mode:proc, num:N, type:tfact, pos:obj, arg:Z, sem:Ob2]),
  lexical_rule([mode:proc, wform:[WForm1, WForm2], num:N, type:tfact, arg:X, arg:Y, arg:Z, sem:V]).
  
vp([mode:gen, num:N, type:tfact, arg:X, sem:[V, Ob1, Ob2]])-->
  verb([mode:gen, wform:[WForm1, WForm2], type:tfact, arg:X, arg:Y, arg:Z, sem:V]),
  np1([mode:gen, num:N, type:tfact, pos:obj, arg:Y, sem:Ob1]),
  verb([mode:gen, wform:[WForm1, WForm2], type:tfact, sem:_V]),
  np2([mode:gen, num:N, type:tfact, pos:obj, arg:Z, sem:Ob2]).

noun([mode:M, num:N, type:T, pos:subj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:yes, arg:X, sem:Sem]) }, WForm.
  
noun([mode:M, num:N, type:T, pos:obj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) }, WForm.
  
verb([mode:proc, wform:List3, type:tfact], List1, List2) :-
   append(List3, List2, List1). 

verb([mode:gen, wform:[WForm1, WForm2], type:tfact, arg:X, arg:Y, arg:Z, sem:V]) -->
   { lexicon([cat:verb, wform:[WForm1, WForm2], num:sg, type:relation, arg:X, arg:Y, arg:Z, sem:V]) }, 
   WForm1.

verb([mode:gen, wform:[WForm1, WForm2], type:tfact, sem:_V]) -->
   { lexicon([cat:verb, wform:[WForm1, WForm2], num:sg, type:relation, arg:_X, arg:_Y, arg:Z, sem:_V]) }, 
   WForm2.
   
% ----------------------------------------------------------------------------

lexical_rule([mode:proc, wform:[WForm1, WForm2], num:N, type:tfact, arg:X, arg:Y, arg:Z, sem:Sem], P1, P1):-
   morphology_rel(WForm1, L1), atomic_list_concat(L1, '_', L2), 
   morphology_rel(WForm2, L3), atomic_list_concat(L3, '_', L4),  
   atomic_list_concat([L2,'__',L4], L), V = [X,Y,Z],
   Sem = json(['Rel'=relation, 'Ind'=L, 'Var'=V]),
   assert(lexicon([cat:verb, wform:[WForm1, WForm2], num:sg, type:relation, arg:X, arg:Y, arg:Z, sem:Sem])).

lower_case(Noun, NP):-
   lower_case_first_atom(Noun, NP).
   
uppercase_first_atom([Atom1|Rest], [Atom2|Rest]) :-
   atom_codes(Atom1, [Char|Chars1]),
   to_upper(Char, LowerChar),
   atom_codes(Atom2, [LowerChar|Chars1]).
   
% ----------------------------------------------------------------------------

% ---------------------------------
% |Associative Entity|
% |Binary Relationship Declaration|
% ---------------------------------

s([type:bfact_ob, sem:json(['Atom'=[Res, Sco]])]) -->  
  np([num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  verb([wform:[associates], num:N, type:fact_ob, arg:X, arg:L, sem:Sco]),
  ['"'], s([type:ob_bfact, sem:L]),['"'],
  ['.'].
  
np([num:N, type:T, pos:P, arg:X, sem:S]) -->
   noun([num:N, type:T, pos:P, arg:X, sem:S]).

noun([num:N, type:T, pos:subj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:yes, arg:X, sem:Sem]) },
  WForm.

noun([num:N, type:T, pos:obj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) },
  WForm.

verb([wform:[associates], num:N, type:fact_ob, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, arg:X, arg:Y, sem:Sem]) },
  [associates].


% -------------------------------------------------------------------

s([type:ob_bfact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  vp([num:N, type:ob_bfact, arg:X, sem:Sco]).
  
vp([num:N, type:ob_bfact, arg:X, sem:[P, O]])-->                   
  verb([num:N, type:ob_bfact, arg:X, arg:Y, sem:P]),
  np([num:_N, type:ob_bfact, pos:obj, arg:Y, sem:O]).

verb([num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem])-->
  {lexicon([cat:verb, wform:WForm, num:N, type:relation, arg:X, arg:Y, sem:Sem])}, WForm.
  
% -------------------------------------------------------------------  

% ----------------------------------
% |Associative Entity|
% |Ternary Relationship Declaration|
% ----------------------------------

s([type:tfact_ob, sem:json(['Atom'=[Res, Sco]])]) -->  
  np([num:N, type:fact, pos:subj, arg:X, sem:Res]), 
  verb([wform:[associates], num:N, type:fact_ob, arg:X, arg:L, sem:Sco]),
  ['"'], s([type:ob_tfact, sem:L]),['"'],['.'].

np([num:N, type:T, pos:P, arg:X, sem:S]) -->
   noun([num:N, type:T, pos:P, arg:X, sem:S]).

noun([num:N, type:T, pos:subj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:yes, arg:X, sem:Sem]) },
  WForm.
  
noun([num:N, type:T, pos:obj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) },
  WForm.

verb([wform:[associates], num:N, type:fact_ob, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, arg:X, arg:Y, sem:Sem]) },
  [associates].
  

% ------------------------------------------------------------------------------

s([type:ob_tfact, sem:json(['And'=json(['Atom'=[Res|Sco]])])]) --> 
  np([num:N, type:ob_tfact, pos:subj, arg:X, sem:Res]),  
  vp([num:N, type:ob_tfact, arg:X, sem:Sco]).

vp([num:N, type:ob_tfact, arg:X, sem:[V, Ob1, Ob2]])-->
  verb([wform:[WForm1, WForm2], type:ob_tfact, arg:X, arg:Y, arg:Z, sem:V]),
  np([num:_N, type:ob_tfact, pos:obj, arg:Y, sem:Ob1]),
  verb([wform:[WForm1, WForm2], type:ob_tfact, sem:_V]),
  np([num:_N, type:ob_tfact, pos:obj, arg:Z, sem:Ob2]).

verb([wform:[WForm1, WForm2], type:ob_tfact, arg:X, arg:Y, arg:Z, sem:V])-->
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:relation, arg:X, arg:Y, arg:Z, sem:V]) }, WForm1.
   
verb([wform:[WForm1, WForm2], type:ob_tfact, sem:_V])--> 
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:relation, arg:X, arg:Y, arg:Z, sem:_V]) }, WForm2.
   
% ------------------------------------------------------------------------------

% ------------------------------------
% |Cardinality Constraint Declaration|
% ------------------------------------

s([type:const, sem:Sem]) -->               
  np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]),
  vp([num:N, type:const, arg:X, sem:S]), ['.'].
    
np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]) -->                 
  qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:Sem]),
  noun([num:N, type:const, pos:subj, arg:X, sem:R]).                          
    
np([num:_N, type:const, pos:obj, arg:X, sco:S, sem:Sem]) -->       
  cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]),
  noun([num:N, type:const, pos:obj, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:Sem]) -->
  verb([num:N, type:const, arg:X, arg:Y, sem:S]),
  np([num:_N, type:const, pos:obj, arg:Y, sco:S, sem:Sem]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 
  { lexicon([cat:qnt, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.
	
cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 					
  { lexicon([cat:cst, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.

noun([num:N, type:const, pos:subj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem]) }, WForm.
  
noun([num:N, type:const, pos:obj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) }, WForm.

verb([num:N, type:const, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:WForm, num:N, type:relation, arg:X, arg:Y, sem:Sem]) }, WForm.


lexicon([cat:qnt, wform:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

lexicon([cat:cst, wform:[exactly, 1], num:sg, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) ]).


lexicon([cat:cst, wform:[at, least, L, and, at, most, M], num:pl, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=L])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=M])]),
                                         Res ]
                                       ])])]) ]). % :- atom_number(L1, L), atom_number(M1, M).
									   
lexicon([cat:cst, wform:[1, or, more], num:pl, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:string','__text'='unbound'])]),
                                         Res ]
                                       ])])]) ]).		
									   
% ------------------------------------------------------------------------------	

% --------------------------------------
% |Constraint Declaration on Attributes|
% --------------------------------------

s([type:const_dp, sem:Sem]) -->
   np([num:N, type:const_dp, pos:subj, arg:X, sco:Sco, sem:Sem]),
   vp([crd:'+', num:N, type:const_dp, arg:X, sem:Sco]),
   ['.'].

np([num:N, type:const_dp, pos:P, arg:X, sco:Sco, sem:Sem]) -->
   qnt([num:N, type:const_dp, pos:P, arg:X, res:json(['Atom'=Res]), sco:Sco, sem:Sem]),
   noun([num:sg, type:const_dp, pos:P, arg:X, sem:Res]).

np([num:N, type:const_dp, pos:P, arg:Y, sco:Sco, sem:Sem]) -->
   cst([num:N, type:const_dp, pos:P, arg:Y, res:Res, sco:Sco, sem:Sem]),
   noun([num:N, type:const_dp, pos:P, arg:Y, sem:Res]).

noun([num:N, type:const_dp, pos:subj, arg:X, sem:Sem]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem])}, WForm.
  
noun([num:N, type:const_dp, pos:obj, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:attribute, pos:obj, dt:DT, arg:Y, sem:Sem])}, WForm.
   

vp([crd:'+', num:N, type:const_dp, arg:X, sem:json(['Exists'=[S1, S2]])]) -->   
    vp([crd:'-', num:N, type:const_dp, arg:X, sem:S1]),
    cc([wfm:[and]]),
    vp([crd:'+', num:N, type:const_dp, arg:X, sem:S2]).


vp([crd:'+', num:N, type:const_dp, arg:X, sem:Sem]) -->   
    verb([num:N, type:const_dp, arg:X, arg:Y, sem:Sco]), 
    np([num:_N, type:const_dp, pos:obj, arg:Y, sco:Sco, sem:Sem]).


vp([crd:'-', num:N, type:const_dp, arg:X, sem:Sem]) -->   
    verb([num:N, type:const_dp, arg:X, arg:Y, sem:Sco]), 
    np([num:_N, type:const_dp, pos:obj, arg:Y, sco:Sco, sem:Sem]).
   
verb([num:N, type:const_dp, arg:X, arg:Y, sem:Sem]) -->
   { lexicon([cat:verb, wform:WForm, num:N, arg:X, arg:Y, sem:Sem]) }, WForm.

qnt([num:N, type:const_dp, pos:subj, arg:X, res:JsonHead, sco:JsonBody, 
					sem:json(['Forall'=json(['Var'=X, 
						'Implies'= json([head=JsonHead, body=JsonBody])])])]) --> ['Every'].
   
cst([num:N, type:const_dp, pos:obj, arg:X, res:Res, sco:Sco, 
					sem:json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) --> [exactly, 1].


cc([wfm:[and]]) --> [and].  

% ------------------------------------------------------------------------

% --------------------------------------
% |Subclass Declaration (Inheritance)|
% --------------------------------------

s([type:const, sem:Sem]) -->               
  np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]),
  vp([num:N, type:const, arg:X, sem:S]), ['.'].
    
np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]) -->                 
  qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:Sem]),
  noun([num:N, type:const, pos:subj, arg:X, sem:R]).                          
    
np([num:_N, type:const, pos:obj, arg:X, sem:json(['Atom'=S])]) -->
  noun([num:N, type:const, pos:obj, arg:X, sem:S]).                          

vp([num:N, type:const, arg:X, sem:Sem]) -->
  verb([wfrm:[is, a]]),
  np([num:_N, type:const, pos:obj, arg:X, sem:Sem]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 
  { lexicon([cat:qnt, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.
	
noun([num:N, type:const, pos:subj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem]) }, WForm.
  
noun([num:N, type:const, pos:obj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) }, WForm.

lexicon([cat:qnt, wform:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

				   
verb([wfrm:[is, a]]) --> [is, a]; [is, an].

% -----------------------------------------------------------------
% |Key Verb Phrases|
% -----------------------------------------------------------------

lexicon([cat:verb, wform:[owns], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=own, 'Var'=[X, Y]])]).
lexicon([cat:verb, wform:[includes], num:sg, type:relation, arg:A, arg:B, sem:json(['Rel'=relation,'Ind'=include,'Var'=[A,B]])]).	
lexicon([cat:verb, wform:[is, dependent, of], num:sg, type:relation, arg:A, arg:B, sem:json(['Rel'=relation,'Ind'=dependent_of,'Var'=[A,B]])]).  
lexicon([cat:verb, wform:[associates], num:sg, type:ob_rel, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=associates, 'Var'=X, 'Reify'=Y])]).

% ----------------------------------------------------------------- 


% --------------------
% |Read Specification|
% --------------------

read_specification(Num):-
	specification(Num, Text),
	start(),
	readt(Text),
	end().
	
readt([]).

readt([[Sentence]|Sentences]) :-
  writeq(Sentence), 
  nl,
  tokenize_atom(Sentence, S),
  process_specification([S]), readt(Sentences).
  
process_specification([]).

process_specification([Sentence|Sentences]) :-
  (
  (s([mode:proc, type:entity, sem:Sem], Sentence, []));
  (s([mode:proc, type:attribute, sem:Sem], Sentence, []));
  (s([type:tfact_ob, sem:Sem], Sentence, []));
  (s([mode:proc, type:tfact, sem:Sem], Sentence, []));
  (s([mode:proc, type:fact, sem:Sem], Sentence, []));
  (s([type:bfact_ob, sem:Sem], Sentence, []));
  (s([type:const, sem:Sem], Sentence, []));
  (s([type:const_dp, sem:Sem], Sentence, []) )
  ),!,
  prolog_vars_to_json_vars(Sem, JSONTerm),
  atom_json_term(JSON, JSONTerm, [as(atom)]),
  nl, nl, nl,
  store_idl(JSON),
  process_specification(Sentences). 
  
store_idl(S) :- open('RuleML.json',append,Stream), write(Stream,S), write(Stream,','), nl(Stream), close(Stream).
start():- open('RuleML.json',append,Stream), write(Stream,'{ "RuleML":'), nl(Stream), 
          write(Stream,'{ "Assert":['), nl(Stream), close(Stream).
end():- open('RuleML.json',append,Stream), write(Stream,'],'), nl(Stream), 
        write(Stream,'"_style": "ERD"'), nl(Stream), write(Stream,'}'), 
		nl(Stream), write(Stream,'}'), close(Stream).  


% ---------------
% |Store Lexicon|
% --------------- 
  
store_lexicon(Name):-
   findall(lexicon(S), (lexicon(S), numbervars(S)), List), 
   atom_concat(Name,'.pl', L), 
   open(L,write,Stream),
   write_lexicon(List, Stream), close(Stream).
   
write_lexicon([],_).

write_lexicon([L|List], Stream):-
	writeq(Stream,L), write(Stream,'.'),
	nl(Stream), 
	write_lexicon(List, Stream).
	
	
test():- read_specification(1).

% ------------------------
% |Generate Verbalisation| 
% ------------------------ 

generate_specification([]).

generate_specification([[Sentence]|Sentences]) :-
  atom_json_term(Sentence, JSONTerm, [as(atom)]),
  (
  (s([mode:gen, type:entity, sem:JSONTerm], Sem, []));
  (s([mode:gen, type:attribute, sem:JSONTerm], Sem, []));
  (s([mode:gen, type:tfact_ob, sem:JSONTerm], Sem, []));
  (s([mode:gen, type:tfact, sem:JSONTerm], Sem, []));
  (s([mode:gen, type:fact, sem:JSONTerm], Sem, []));
  (s([mode:gen, type:bfact_ob, sem:JSONTerm], Sem, []));
  (s([type:const, sem:JSONTerm], Sem, []));
  (s([mode:gen, type:const_dp, sem:JSONTerm], Sem, []) )
  ),!,
  %write('Sem: '),  
  %writeq(Sentence), 
  nl,
  append(S1,['.'], Sem), atomic_list_concat(S1,' ',S2),
  atom_concat(S2,'.', Sen),
  store_v(Sen),	
  nl,
  generate_specification(Sentences). 
  
generate_verbalisation(Num):-
	specification(Num, Text),
	generate_specification(Text).

store_v(S) :- open('generate_verbalisation.txt',append,Stream), write(Stream,S), nl(Stream), close(Stream).	


test1 :-
    s([mode:proc, type:entity, sem:PrologTerm],
      ['Student', is, an, entity, type, '.'], []),
	prolog_vars_to_json_vars(PrologTerm, JSONTerm),
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    writeq(JSON).							   
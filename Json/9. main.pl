:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
% :- use_module('6. associative_entity_trel_json').
:- dynamic lexicon/1.
:- consult(p_to_j).
:- consult(test).

%------------------------------------
%------- Entity Declaration ---------
%------------------------------------

:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#'). 

 s([mode:M, type:entity, sem:json(['Atom'=Sem])]) --> 
   np([mode:M, num:sg, type:entity, pos:subj, sem:Sem]), 
   [is, an, entity, type], ['.'].

 np([mode:M, num:N, type:T, pos:P, sem:Sem]) --> 
   noun([mode:M, num:N, type:T, pos:P, arg:X, sem:Sem]). 

 noun([mode:proc, num:N, type:entity, pos:P, arg:X, sem:Sem]) --> 
   lexical_rule([cat:noun, num:N, type:entity, pos:P, arg:X, sem:Sem]). 

 noun([mode:gen, num:N, type:entity, pos:P, arg:X, sem:Sem]) --> 
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem])}, 
   WForm. 

%------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, pos:P, arg:X, sem:Sems], List1, List2) :-
   process_noun([wform:WForm, List5, List1, List2]), 
   downcase_list(WForm, DWForm), atomic_list_concat(DWForm, ' ', WForm_sg), morphology(WForm_sg, WForm_pl),
   Sems = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]),
   Semo = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:subj, arg:X, sem:Sems])),
   assert(lexicon([cat:noun, wform:[WForm_sg], num:sg, type:entity, pos:obj, arg:X, sem:Semo])),
   assert(lexicon([cat:noun, wform:[WForm_pl], num:pl, type:entity, pos:obj, arg:X, sem:Semo])).
   
process_noun([wform:List3, List5, List1, List2]) :-
   append(List3, [is, an, entity, type, '.'], List1), 
   List2 = [is, an, entity, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5).
   %Sem = json(['Rel'=entity, 'Ind'=List5, 'Var'=X]).
   
   
%------------------------------------
%------ Attribute Declaration -------
%------------------------------------   

s([mode:M, type:attribute, sem:json(['Atom'=Sem])]) --> 
  np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]), 
  [is, of, DT, data, type, '.'].

np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]) -->
   n([mode:M, num:N, type:attribute, dt:DT, sem:Sem]).
   

n([mode:proc, num:N, type:attribute, dt:DT, sem:Sem])-->
   lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]).
   
n([mode:gen, num:N, type:attribute, dt:DT, sem:Sem])-->
   { lexicon([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]) },
   WForm. 

%---------------------------------------------------------------------------------------------

lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem], L1, L2):-
   process_attribute([wform:WForm, dt:DT, arg:X, sem:Sem], L1, L2),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:attribute, dt:DT, arg:X, sem:Sem])).
   
process_attribute([wform:List3, dt:D, arg:X, sem:Sem], List1, List2):-
   (D = integer; D = string; D = date; D = boolean),
   append(List3, [is, of, D, data, type, '.'], List1),
   List2 = [is, of, D, data, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5), V = ['Type'=D, 'Text'=X],
   Sem =json(['Rel'=attribute, 'Ind'=List5, 'Var'=json(V)]). 


%---------------------------------------
%--- Binary Relationship Declaration ---
%--------------------------------------- 

s([mode:M, type:fact, sem:json(['And'=json(['Atom'=Sem])])]) --> 
  np([mode:M, num:N, type:fact, pos:subj, arg:X, sco:Sem, sem:S]), 
  vp([mode:M, num:N, type:fact, arg:X, sub:S, sem:Sem]),
  ['.'].

vp([mode:M, num:N, type:fact, arg:X, sub:S, sem:[S,Sem,O]])-->
  verb([mode:M, num:N, type:fact, arg:X, arg:Y, sem:Sem]),
  np([mode:M, num:N, type:fact, pos:obj, arg:Y, sem:O]).
  
np([mode:proc, num:N, type:fact, pos:P, arg:X, sco:Sem, sem:S]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:S]) },
  WForm.

np([mode:M, num:N, type:fact, pos:P, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:_N, type:entity, pos:P, arg:Y, sem:Sem]) }, 
  WForm.
  
np([mode:gen, num:N, type:fact, pos:P, arg:X, sco:[S,Sem,O], sem:S]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:S]) },
  WForm.
  
%np([mode:gen, num:N, type:fact, pos:P, arg:X, sem:Sem]) -->
%  {lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:P, arg:X, sem:Sem])},
%  WForm.
  
verb([mode:proc, num:N, type:fact, arg:X, arg:Y, sem:Sem]) -->
  lexical_rule([cat:verb, num:N, arg:X, arg:Y, sem:Sem]).

verb([mode:gen, num:N, type:fact, arg:X, arg:Y, sem:Sem])-->
  { lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:Sem]) }, WForm.

%------------------------------------------------------------------------------

lexical_rule([cat:verb, num:N, arg:X, arg:Y, sem:B], P1, P2) :-
  generate_sem(WForm, P1, P2, X, Y, B),
  (
  assert(lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:B]))
  ;
  retract(lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:B]))
  ).
  

generate_sem(WForm, P1, P2, X, Y, B) :-
   append(WForm, P2, P1),
   WForm \= [], 
   morphology_rel(WForm, WF),
   atomic_list_concat(WF,'_', Term),
   V = [X,Y],
   B = json(['Rel'=relation, 'Ind'=Term, 'Var'=V]).

% morphology function for a relation (is_enrolled_in --> enrolled_in or belongs_to --> belong_to)
   
%morphology_rel([L|L1], L2):-
%	((L == is) -> L2 = L1) ; 
%	( porter_stem(L,L3), L2 = [L3|L1]).


%----------------------------------------
%--- ternary Relationship Declaration ---
%----------------------------------------

s([mode:M, type:tfact, sem:json(['And'=json(['Atom'=Sem])])]) --> 
  np([mode:M, num:N, type:tfact, pos:subj, arg:X, sco:Sem, sem:S]), 
  vp([mode:M, num:N, type:tfact, arg:X, arg:Y, arg:Z, sub:S, sem:Sem]), ['.'].
  
vp([mode:proc, num:N, type:tfact, arg:X, arg:Y, arg:Z, sub:S, sem:[S,V,Ob1,Ob2]])-->
  v([mode:proc, type:tfact, label:R]),
  np([mode:proc, num:N, type:tfact, pos:obj, arg:Y, sem:Ob1]),
  v([mode:proc, type:tfact, label:P]),
  np([mode:proc, num:N, type:tfact, pos:obj, arg:Z, sem:Ob2]),
  lexical_rule([mode:proc, num:N, type:tfact, arg:X, arg:Y, arg:Z, label:R, label:P, sem:V]).
  
vp([mode:gen, num:N, type:tfact, arg:X, arg:Y, arg:Z, sub:S, sem:[S,V,Ob1,Ob2]])-->
  v([mode:gen, type:tfact, slot:WForm2, pos:beg, sem:V]),
  np([mode:gen, num:N, type:tfact, pos:obj, arg:Y, sem:Ob1]),
  v([mode:gen, type:tfact, slot:WForm2, pos:end, sem:V]),
  np([mode:gen, num:N, type:tfact, pos:obj, arg:Z, sem:Ob2]).

np([mode:M, num:N, type:tfact, pos:P, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) }, WForm.
  
np([mode:M, num:N, type:tfact, pos:P, arg:X, sco:[S,V,Ob1,Ob2], sem:S]) -->
  { lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:P, arg:X, sem:S]) }, WForm.
   
v([mode:proc, type:tfact, label:List3], List1, List2) :-
   append(List3, List2, List1). 
   
v([mode:gen, type:tfact, slot:WForm2, pos:beg, sem:V])-->
   { lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:trel, arg:X, label:_List1, arg:Y, label:_List2, arg:Z, sem:V]) }, WForm1.
   
v([mode:gen, type:tfact, slot:WForm2, pos:end, sem:V])--> WForm2.

%------------------------------------------------------------------------------

lexical_rule([mode:proc, num:N, type:tfact, arg:X, arg:Y, arg:Z, label:List1, label:List2, sem:Sem], P1, P1):-
   morphology_rel(List1, L1), atomic_list_concat(L1, '_', L2), 
   morphology_rel(List2, L3), atomic_list_concat(L3, '_', L4),  
   atomic_list_concat([L2,'__',L4], L), V = [X,Y,Z], %Sem =.. [relation,L,X,Y,Z],
   Sem = json(['Rel'=relation, 'Ind'=L, 'Var'=V]),
   assert(lexicon([cat:verb, wform:[List1, List2], num:sg, type:trel, arg:X, label:L2, arg:Y, label:L4, arg:Z, sem:Sem])).
   
morphology_rel([L|L1], L2):-
	((L == is) -> L2 = L1) ; 
	((sub_atom(L,Q,3,0,ies) -> sub_atom(L,_,Q,3,L0), atom_concat(L0,y,L3), L2 = [L3|L1])); 
	((porter_stem(L,L3), L2 = [L3|L1])).

%----------------------------------------
%-- Associative Entity Declaration for --
%--------- Binary Relationship ----------
%----------------------------------------

s([mode:M, type:bfact_ob, sem:Sem]) -->  
  np([mode:M, num:N, type:bfact_ob, pos:subj, arg:X, sem:S]), 
  verb([mode:M, wform:[associates], num:N, type:bfact_ob, sub:S, arg:X, arg:json(['And'=json(['Atom'=L])]), sem:Sem]),
  ['"'], s([mode:M, type:ob_bfact, sem:L]),['"'],['.'].
  
np([mode:M, num:N, type:bfact_ob, pos:P, arg:X, sem:S]) -->
   noun([mode:M, num:N, type:bfact_ob, pos:P, arg:X, sem:S]).

noun([mode:M, num:N, type:bfact_ob, pos:P, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) },
  WForm.

verb([mode:M, wform:[associates], num:N, type:bfact_ob, sub:S, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, sub:S, arg:X, arg:Y, sem:Sem]) },
  [associates].

%-------------------------------------------------------------------------------

s([mode:M, type:ob_bfact, sem:Sem]) --> 
  np([mode:M, num:N, type:ob_bfact, func:subj, arg:X, sco:Sem, sem:S]), 
  vp([mode:M, num:N, type:ob_bfact, arg:X, sub:S, sem:Sem]).

vp([mode:M, num:N, type:ob_bfact, arg:X, sub:S, sem:[S,Sem,O]])-->
  verb([mode:M, num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem]),
  np([mode:M, num:_N, type:ob_bfact, func:obj, arg:Y, sem:O]).
  
np([mode:proc, num:N, type:ob_bfact, func:subj, arg:X, sco:Sem, sem:S]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, arg:X, sem:S]) },
  WForm.

np([mode:proc, num:N, type:ob_bfact, func:obj, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:Y, sem:Sem]) }, 
  WForm.
  
np([mode:gen, num:N, type:ob_bfact, func:subj, arg:X, sco:[S,Sem,O], sem:S]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, arg:X, sem:S]) },
  WForm.
  
np([mode:gen, num:N, type:ob_bfact, func:obj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem])},
  WForm.
  
verb([mode:M, num:N, type:ob_bfact, arg:X, arg:Y, sem:Sem])-->
  {lexicon([cat:verb, wform:WForm, num:N, type:brel, arg:X, arg:Y, sem:Sem])}, WForm. 

%----------------------------------------
%-- Associative Entity Declaration for --
%-------- Ternary Relationship ----------
%----------------------------------------		

s([mode:M, type:tfact_ob, sem:Sem]) -->  
  np([mode:M, num:N, type:tfact_ob, pos:subj, arg:X, sem:S]), 
  verb([mode:M, wform:[associates], num:N, type:tfact_ob, sub:S, arg:X, arg:json(['And'=json(['Atom'=L])]), sem:Sem]),
  ['"'], s([mode:M, type:ob_tfact, rel:R, sem:L]),['"'],['.'].
  
np([mode:M, num:N, type:tfact_ob, pos:P, arg:X, sem:S]) -->
   noun([mode:M, num:N, type:tfact_ob, pos:P, arg:X, sem:S]).

noun([mode:M, num:N, type:tfact_ob, pos:P, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Sem]) },
  WForm.

verb([mode:M, wform:[associates], num:N, type:tfact_ob, sub:S, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:[associates], num:N, type:ob_rel, sub:S, arg:X, arg:Y, sem:Sem]) },
  [associates].

%-------------------------------------------------------------------------------

s([mode:M, type:ob_tfact, rel:R, sem:L]) --> 
  np([mode:M, num:N, type:tfact_ob, pos:subj, arg:X, sem:S]),  
  vp([mode:M, num:N, type:ob_tfact, rel:R, sub:S, arg:X, arg:Y, arg:Z, sem:L]).

vp([mode:M, num:N, type:ob_tfact, rel:R, sub:S, arg:X, arg:Y, arg:Z, sem:[S,V,Ob1,Ob2]])-->
  verb([mode:M, type:ob_tfact, slot:WForm2, pos:beg, arg:X, arg:Y, arg:Z, sem:V]),
  np([mode:M, num:N, type:tfact_ob, pos:obj, arg:Y, sem:Ob1]),
  verb([mode:M, type:ob_tfact, slot:WForm2, pos:end, arg:X, arg:Y, arg:Z, sem:V]),
  np([mode:M, num:N, type:tfact_ob, pos:obj, arg:Z, sem:Ob2]).
  
verb([mode:M, type:ob_tfact, slot:WForm2, pos:beg, arg:X, arg:Y, arg:Z, sem:V])-->
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:trel, arg:X, label:_List1, arg:Y, label:_List2, arg:Z, sem:V]) }, WForm1.
   
verb([mode:M, type:ob_tfact, slot:WForm2, pos:end, arg:X, arg:Y, arg:Z, sem:V])--> 
   {lexicon([cat:verb, wform:[WForm1,WForm2], num:sg, type:trel, arg:X, label:_List1, arg:Y, label:_List2, arg:Z, sem:V]) }, WForm2.

	
lexicon([cat:verb, wform:[associates], num:sg, type:ob_rel, sub:S, arg:X, arg:Y, 
		sem:json(['Atom'=[S,json(['Rel'=relation, 'Ind'=associates, 'Var'=X, 'Reify'=Y])]])]).
		
lexicon([cat:verb, wform:[includes], num:sg, type:brel, arg:A, arg:B, sem:json(['Rel'=relation,'Ind'=include,'Var'=[A,B]])]).	
lexicon([cat:verb, wform:[is, dependent, of], num:sg, type:brel, arg:A, arg:B, sem:json(['Rel'=relation,'Ind'=dependent_of,'Var'=[A,B]])]).	

%----------------------------------------
%--------- Constraint Declaration -------
%----------------------------------------

s([type:const, sem:L]) -->               
  np([num:N, type:const, func:subj, arg:X, sco:S, sem:L]),
  vp([num:N, type:const, arg:X, sem:S]), ['.'].
    
np([num:N, type:const, func:subj, arg:X, sco:S, sem:L]) -->                 
   qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:L]),
   noun([num:N, type:const, arg:X, sem:R]).                          
    
np([num:_N, type:const, func:obj, arg:X, sco:S, sem:L]) -->       
   cst([num:N, type:const, arg:X, res:R, sco:S, sem:L]),
   noun([num:_N, type:const, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:L]) -->
   verb([num:N, type:const, arg:X, arg:Y, sem:S]),
   np([num:_N, type:const, func:obj, arg:Y, sco:S, sem:L]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:L0]) --> 
   { lexicon([cat:qnt, wfm:[Wfm|Wfms], num:N, arg:X, res:R, sco:S, sem:L0]) }, [Wfm|Wfms].
	
cst([num:N, type:const, arg:X, res:R, sco:S, sem:L0]) --> 					
   { lexicon([cat:cst, wfm:[Wfm|Wfms], num:N, arg:X, res:R, sco:S, sem:L0]) }, [Wfm|Wfms].

noun([num:N, type:const, arg:X, sem:L0]) -->
   { lexicon([cat:noun, wform:[Wfm|Wfms], num:N, type:entity, pos:obj, arg:X, sem:L0]) }, [Wfm|Wfms].

verb([num:N, type:const, arg:X, arg:Y, sem:L0]) -->
   { lexicon([cat:verb, wform:[Wfm|Wfms], num:N, type:brel, arg:X, arg:Y, sem:L0]) }, [Wfm|Wfms].


lexicon([cat:qnt, wfm:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

lexicon([cat:cst, wfm:[exactly, 1], num:sg, arg:X, res:Res, sco:Sco,
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


lexicon([cat:cst, wfm:[at, least, L, and, at, most, M], num:sg, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=L])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=M])]),
                                         Res ]
                                       ])])]) ]).% :- atom_number(L1, L), atom_number(M1, M).
									   
lexicon([cat:cst, wfm:[1, or, more], num:sg, arg:X, res:Res, sco:Sco,
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
		

%----------------------------------------
%--------- Constraint Declaration -------
%------------ for attributes ------------
%----------------------------------------

s([mode:M, type:const_dp, sem:Sem]) -->
   np([num:N, mode:M, type:const_dp, pos:subj, arg:X, scope:Sco, sem:Sem]),
   vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:Sco]),
   ['.'].

np([num:N, mode:M, type:const_dp, pos:P, arg:X, scope:Sco, sem:Sem]) -->
   qnt([num:N, mode:M, type:const_dp, pos:P, arg:X, restrictor:json(['Atom'=Res]), scope:Sco, sem:Sem]),
   noun([num:sg, mode:M, type:const_dp, pos:obj, arg:X, sem:Res]).
   
np([num:N, mode:M, type:const_dp, pos:P, arg:Y, scope:Sco, sem:Sem]) -->
   cst([num:N, mode:M, type:const_dp, pos:P, arg:Y, restrictor:Res, scope:Sco, sem:Sem]),
   noun([num:N, mode:M, type:const_dp, arg:Y, sem:Res]).
   
noun([num:N, mode:M, type:const_dp, pos:P, arg:X, sem:Res]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Res])}, WForm.
  
noun([num:N, mode:M, type:const_dp, arg:Y, sem:Res]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:Y, sem:Res])}, downcase_noun(WForm, W).
   

vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:json(['Exists'=[S1,S2]])]) -->   
    vp([crd:'-', num:N, mode:M, type:const_dp, arg:X, sem:S1]),
    cc([wfm:[and]]),
    vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:S2]).
   

vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:Sco]) -->   
    v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:Sem]), 
    np([num:_, mode:M, type:const_dp, pos:obj, arg:Y, scope:Sem, sem:Sco]).


vp([crd:'-', num:N, mode:M, type:const_dp, arg:X, sem:Sco]) -->   
    v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:Sem]), 
    np([num:_, mode:M, type:const_dp, pos:obj, arg:Y, scope:Sem, sem:Sco]).

v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=own, 'Var'=[X,Y]])]) -->
   [owns].
v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=consist_of, 'Var'=[X,Y]])]) -->
   [consists, of].

qnt([num:N, mode:M, type:const_dp, pos:subj, arg:X, restrictor:JsonHead, scope:JsonBody, 
					sem:json(['Forall'=json(['Var'=X, 
						'Implies'= json([head=JsonHead, body=JsonBody])])])]) --> ['Every'].
   
cst([num:N, mode:M, type:const_dp, pos:obj, arg:X, restrictor:Res, scope:Sco, 
					sem:json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) -->[exactly, 1].

cc([wfm:[and]]) --> [and].		

%------------------------------------
%------- Read Specification ---------
%------------------------------------

read_specification(Num):-
	specification(Num, Text),
	readt(Text).
	
readt([]).

readt([[Sentence]|Sentences]) :-
  write('Sentence: '), 
  writeq(Sentence), 
  nl,
  tokenize_atom(Sentence, S),
  process_specification([S]), readt(Sentences).
  
process_specification([]).

process_specification([Sentence|Sentences]) :-
  (
  (s([mode:proc, type:entity, sem:Sem], Sentence, []));
  (s([mode:proc, type:attribute, sem:Sem], Sentence, []));
  (s([mode:proc, type:tfact_ob, sem:Sem], Sentence, []));
  (s([mode:proc, type:tfact, sem:Sem], Sentence, []));
  (s([mode:proc, type:fact, sem:Sem], Sentence, []));
  (s([mode:proc, type:bfact_ob, sem:Sem], Sentence, []));
  (s([type:const, sem:Sem], Sentence, []));
  (s([mode:proc, type:const_dp, sem:Sem], Sentence, []) )
  ),!,
%  write('Sentence: '), 
  writeq(Sentence), nl,
  write('Sem:      '),
  prolog_vars_to_json_vars(Sem, JSONTerm),
  atom_json_term(JSON, JSONTerm, [as(atom)]),
  write(JSON),
  nl, nl, nl,
  process_specification(Sentences). 

%------------------------------------
%---------- Store Lexicon -----------
%------------------------------------  
  
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
 
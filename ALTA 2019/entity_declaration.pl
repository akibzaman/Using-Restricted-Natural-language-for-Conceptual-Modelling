%-----------------------------------------------------------------------
% Input:  Student is an entity.
% Output: entity(A, student)
%-----------------------------------------------------------------------
:- style_check([-discontiguous, -singleton]).

:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').

 s([mode:M, type:entity, sem:L1-L2]) --> 
   np([mode:M, num:sg, type:entity, pos:subj, sem:L1-L2]), 
   [is, an, entity, type], ['.'].

 np([mode:M, num:N, type:T, pos:P, sem:L1-L2]) --> 
   noun([mode:M, num:N, type:T, pos:P, sem:L1-L2]). 

 noun([mode:proc, num:N, type:entity, pos:P, sem:[L1|L2]-[[L0|L1]|L2]]) --> 
   lexical_rule([cat:noun, num:N, type:entity, pos:P, sem:L0]). 

 noun([mode:gen, num:N, type:entity, pos:P, sem:[[L0|L1]|L2]-[L1|L2]]) --> 
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:_X, sem:L0])}, 
   WForm. 

%------------------------------------------------------------

lexical_rule([cat:noun, num:sg, type:entity, pos:P, sem:Sem], List1, List2) :-
   process_noun([wform:WForm, arg:X, sem:Sem, List1, List2]),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:entity, pos:subj, arg:X, sem:Sem])), 
   downcase_list(WForm, DWForm), atomic_list_concat(DWForm, ' ', WForm_sg), morphology(WForm_sg, WForm_pl),
   assert(lexicon([cat:noun, wform:[WForm_sg], num:sg, type:entity, pos:obj, arg:X, sem:Sem])),
   assert(lexicon([cat:noun, wform:[WForm_pl], num:pl, type:entity, pos:obj, arg:X, sem:Sem])).
   
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

downcase_list(AnyCaseList, DownCaseList):-
  maplist(downcase_atom, AnyCaseList, DownCaseList).   

morphology(W, Wo):-
	  (sub_atom(W,_, 2, 0, C), (C == sh; C = ch)); (sub_atom(W,_,1,0,P), (P == s; P == z; P == x)) -> atom_concat(W,es,Wo) ; 
	  (sub_atom(W,Q,1,0,L), (L == y)) -> sub_atom(W,_,Q,1,L1), atom_concat(L1,ies,Wo) ; atom_concat(W,s,Wo).   
   
test1 :-
    s([mode:proc, type:entity, sem:[[]]-L],
      ['Student', is, an, entity, type, '.'], []),
    numbervars(L),
    write(L), nl, nl.
	
test2 :-
    Sem = [[entity(A,student)]],								       
    s([mode:gen, type:entity, sem:Sem-[[]]], S, []), write(S), nl, nl.
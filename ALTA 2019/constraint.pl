    % -----------------------------------------------------------------------------
    % Input:  (20) Every student studies at least 1 and at most 4 units.
    % Output: forall(A, student(A)=>exists(B, unit(B) & min(1):study(A, B):max(4)))
    % -----------------------------------------------------------------------------
	:- style_check([-discontiguous, -singleton]).
	:- consult(lexicon).   
  
    :- op(900, yfx, '==>').
    :- op(800, yfx, '&').
    :- op(900, yfx, ':').
    
	s([mode:M, type:const, sem:Sem]) -->
	   np([num:N, mode:M, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:Sem]),
	   vp([num:N, mode:M, type:const, restrictor:Res, scope:Sco]),
	   ['.'].
 
	np([num:N, mode:M, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:Sem]) -->
	   qnt([num:N, mode:M, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:Sem]),
	   n([num:N, mode:M, type:const, arg:X, func:subj, sem:Res]).
	   
	vp([num:N, mode:M, type:const, restrictor:Res, scope:Sco]) -->
	   v([num:N, mode:M, type:const, arg1:Res, arg2:Res2, sem:S]),
	   np([num:_N, mode:M, type:const, arg:X, restrictor:Res2, func:obj, scope:S, sem:Sco]).

	np([num:N, mode:proc, type:const, arg:X, restrictor:Res2, func:obj, scope:Sco, sem:Sem]) -->
	   cst([num:N, mode:proc, type:const, arg:X, func:obj, restrictor:Res2, scope:Sco, sem:[A|B]-[[Sem|A]|B]]),
	   n([num:N, mode:proc, type:const, arg:X, func:obj, sem:Res2]).
	   
	np([num:N, mode:gen, type:const, arg:X, restrictor:Res2, func:obj, scope:Sco, sem:Sem]) -->
	   cst([num:N, mode:gen, type:const, arg:X, func:obj, restrictor:Res2, scope:Sco, sem:[[Sem|A]|B]-[A|B]]),
	   n([num:N, mode:gen, type:const, arg:X, func:obj, sem:Res2]).

	n([num:N, mode:proc, type:const, arg:X, func:subj, sem:Sem])-->
	   {lexicon([cat:noun, wform:[Wfm|WFms], num:sg, type:const, arg:X, sem:Sem])}, [Wfm|WFms]. 
	   
	n([num:N, mode:gen, type:const, arg:X, func:subj, sem:Sem])-->
	   {lexicon([cat:noun, wform:[Wfm|WFms], num:sg, type:const, arg:X, sem:Sem])}, [Wfm|WFms].
	   
	n([num:N, mode:proc, type:const, arg:X, func:obj, sem:Sem])--> 
	   {lexicon([cat:noun, wform:[Wfm|WFms], num:N, type:const, arg:X, sem:Sem])}, [Wfm|WFms]. 
	   
	n([num:N, mode:gen, type:const, arg:X, func:obj, sem:Sem])-->
	   {lexicon([cat:noun, wform:[Wfm|WFms], num:N, type:const, arg:X, sem:Sem])}, [Wfm|WFms]. 
	   
	v([num:N, mode:M, type:const, arg1:Res, arg2:Res2, sem:Sem])-->  
	   {lexicon([cat:verb, wform:[Wfm|WFms], num:sg, type:brel, arg1:Res, arg2:Res2, sem:Sem])}, [Wfm|WFms]. 

	qnt([num:N, mode:proc, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:[A|B]-[[forall(X, Res ==> Sco)|A]|B]]) -->
	   ['Every'].

	qnt([num:N, mode:gen, type:const, arg:X, func:subj, restrictor:Res, scope:Sco, sem:[[forall(X, Res ==> Sco)|A]|B]-[A|B]]) -->
	   ['Every'].
	
	cst([num:N, mode:proc, type:const, arg:X, func:obj, restrictor:Res, scope:Sco, sem:[A|B]-[[exists(X, Res & min(L):Sco:max(U))|A]|B]]) -->
	   [at, least, L, and, at, most, U], {number(L), number(U), L>0, U>0, L<U, N=pl}.
	   
	cst([num:N, mode:gen, type:const, arg:X, func:obj, restrictor:Res, scope:Sco, sem:[[exists(X, Res & min(L):Sco:max(U))|A]|B]-[A|B]]) -->
	   [at, least, L, and, at, most, U], {number(L), number(U), L>0, U>0, L<U, N=pl}.
	   
	%----------------------------------------------------------------------------------------  
    test1 :-
    s([mode:proc, type:const, sem:[[]]-L],['Every', student, studies, at, least, 1, and, at, most, 4, units, '.'], []),
    numbervars(L), write(L), nl, nl.
 
    test2 :-
    Sem = [[forall(C,entity(C,student)==>exists(D,entity(D,unit)&min(1):relation(entity(C,student),entity(D,unit),studies):max(4)))|A]|B]-[A|B],
    s([mode:gen, type:const, sem:Sem], S, []),  append(S1,['.'], S), atomic_list_concat(S1,' ',S2),
    atom_concat(S2,'.', Sen), write(Sen), nl, nl.

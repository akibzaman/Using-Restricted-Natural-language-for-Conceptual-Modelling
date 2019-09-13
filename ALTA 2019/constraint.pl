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
	   np([num:N, mode:M, type:const, func:subj, restrictor:Res, scope:Sco, sem:Sem]),
	   vp([num:N, mode:M, type:const, arg:Res, scope:Sco]),
	   ['.'].
 
	np([num:N, mode:M, type:const, func:subj, restrictor:Res, scope:Sco, sem:Sem]) -->
	   qnt([num:N, mode:M, type:const, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
	   n([num:N, mode:M, type:const, arg:X, func:subj, sem:Res]).
	   
	vp([num:N, mode:M, type:const, arg:X, scope:Sco]) -->
	   v([num:N, mode:M, type:const, arg:X, arg:Y, sem:Sem]),
	   np([num:_N, mode:M, type:const, func:obj, restrictor:Y, scope:Sem, sem:Sco]).

	np([num:N, mode:M, type:const, func:obj, restrictor:Res, scope:Sco, sem:Sem]) -->
	   cst([num:N, mode:M, type:const, arg:X, restrictor:Res, scope:Sco, sem:Sem]),
	   n([num:N, mode:M, type:const, arg:X, func:obj, sem:Res]).
	   
	n([num:N, mode:proc, type:const, arg:X, func:subj, sem:Sem])-->
	   {lexicon([cat:noun, wform:WForm, num:sg, type:const, arg:X, sem:Sem])}, WForm. 
	   
	n([num:N, mode:gen, type:const, arg:X, func:subj, sem:Sem])-->
	   {lexicon([cat:noun, wform:WForm, num:sg, type:const, arg:X, sem:Sem])}, WForm.
	   
	n([num:N, mode:proc, type:const, arg:X, func:obj, sem:Sem])--> 
	   {lexicon([cat:noun, wform:WForm, num:N, type:const, arg:X, sem:Sem])}, WForm. 
	   
	n([num:N, mode:gen, type:const, arg:X, func:obj, sem:Sem])-->
	   {lexicon([cat:noun, wform:WForm, num:N, type:const, arg:X, sem:Sem])}, WForm. 
	   
	v([num:N, mode:M, type:const, arg:X, arg:Y, sem:Sem])-->  
	   {lexicon([cat:verb, wform:WForm, num:sg, type:brel, arg:X, arg:Y, sem:Sem])}, WForm. 

	qnt([num:N, mode:proc, type:const, arg:X, restrictor:Res, scope:Sco, sem:[A|B]-[[forall(X, Res ==> Sco)|A]|B]]) -->
	   ['Every'].

	qnt([num:N, mode:gen, type:const, arg:X, restrictor:Res, scope:Sco, sem:[[forall(X, Res ==> Sco)|A]|B]-[A|B]]) -->
	   ['Every'].
	
	cst([num:pl, mode:M, type:const, arg:X, restrictor:Res, scope:Sco, sem:exists(X, Res & min(L):Sco:max(U))]) -->
	   [at, least, L, and, at, most, U], {number(L), number(U), L>0, U>0, L<U}.
	   
   
	%----------------------------------------------------------------------------------------  
    test1 :-
    s([mode:proc, type:const, sem:[[]]-L],['Every', student, studies, at, least, 1, and, at, most, 4, units, '.'], []),
    numbervars(L), write(L), nl, nl.
 
    test2 :-
    Sem = [[forall(C,entity(C,student)==>exists(D,entity(D,unit)&min(1):relation(entity(C,student),entity(D,unit),studies):max(4)))|A]|B]-[A|B],
	s([mode:gen, type:const, sem:Sem], S, []),  append(S1,['.'], S), atomic_list_concat(S1,' ',S2),
    atom_concat(S2,'.', Sen), write(Sen), nl, nl.
